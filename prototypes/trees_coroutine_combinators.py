#!/usr/bin/python3

# This is a reimplementation of my recursive-descent combinators with
# trampolined dispatch.  My goals for this prototype are a simple
# inlined trampoline, a trampoline that works for both coroutines and
# simple functions (this is critical because eventually struct and re,
# which are functions, plus blackbox parsers will need to be
# integrated into the trampoline), moving the self-contained stack in
# Sequence to the trampoline stack, an adjacency list representation
# for the tree, and better error reporting (since I've replaced the
# call stack, this probably involves creating a stack trace myself).
# I could probably add data aggregation for Alternation and Sequence,
# passing a list of results to the trampoline to be added to the
# stack, but that will require that the trampoline handle heterogenous
# lists of coroutines, lists of Successes, and Failures, and since the
# only benefit would be to performance, I don't think it merits the
# effort in a prototype.

# There are two big problems with encapsulating the tree
# representation, one ill-defined and one well-defined, respectively:
# I'm not sure what the optimal interface to make semantic actions
# easy to define for users is, and representation independence in
# Python compromises performance.  Even subclassing built-in types,
# different representations will need different methods to provide a
# common interface and those methods have to be implemented in Python,
# plus performance enhancements like comprehensions can't be used with
# subclasses.  I see only two possible solutions to the performance
# problem: move the representation into Cython or C, either by using
# an existing C/C++/Cython module, or compiling the code to eliminate
# the intermediate data structures and thus the overhead from using a
# representation implemented in Python.

# Even these simple combinators illustrate the distinction I made in
# my notes between stateful coroutines and stateless coroutines that
# here I'm representing with simple functions.  Sequence and
# Alternation are full stateful coroutines.  Action has no internal
# state variables, but transforming it so it's an eternally-looping
# coroutine would require adding an additional exit/entry point for
# the stream and offset, thus it's also a stateful coroutine.  Recursive
# and Terminal have no need to interact with the trampoline so can be
# simple functions.  I don't know if the other distinctions I outlined
# are useful or not.

# There are several additional ideas for debugging that I haven't
# implemented so far.  First, maybe Success and Failure could be
# overloaded, with all parse and _parse functions taking a default
# value pointing to the current Success and Failure objects but
# allowing the possibility of a DebugSuccess and DebugFailure that
# contain more tracing information about the path that data took
# through the parser.  I'm not too sure about the performance
# implications of this so I'm deferring it for now.  Second, I should
# provide combinators that help parser writers provide their users
# informative error messges, thinking along the lines of the kind of
# error messages that traditional compilers and interpreters for
# computer languages give their users or the modifications I made to
# some of Construct's classes to tell me where errors occurred.  This
# is definitely polish and not needed in these prototypes.  Third, I
# need some way to make useful conditional breakpoints in the debugger
# easier, but I'm not sure what this is---currently, I'm having trace
# keep a crude count of how many times each combinator is called, but
# this is most useful when an exception is thrown.

from __future__ import print_function

import types
import pprint
import collections
import sys

from trees import TreeAL as Tree


class CombinatorError(Exception):
    pass

class CombinatorTypeError(CombinatorError, TypeError):
    pass


class Result:
    __slots__ = ()
    def __str__(self):
        """Strictly for debugging and output, so this doesn't have to be
        efficient."""
        raise NotImplementedError

class Success(Result):
    __slots__ = ('tree', 'stream', 'offset')
    def __init__(self, stream, offset=0, *args, **kwds):
        self.tree = Tree(*args, **kwds)
        self.stream = stream
        self.offset = offset
    def __str__(self):
        return 'Success: "{}", "{}"'.format(self.tree, self.stream[self.offset:])
    __repr__ = __str__

class Failure(Result):
    """It would be possible to distinguish between Successes and Failures
    by using try-except rather than if-else.  However, I'm concerned
    about the performance implications of this approach, so I haven't
    implemented this at the moment.  The final decision will have to
    come down to profiling.  Thus, at the moment Failure inherits from
    Result not Exception.

    """

    __slots__ = ('tree', 'msg', 'stream', 'offset')
    def __init__(self, msg='', stream='', offset=0, *args, **kwds):
        self.tree = Tree(*args, **kwds)
        self.msg = msg
        self.stream = stream
        self.offset = offset
    def __str__(self):
        return 'Failure: "{}", {}'.format(self.tree, self.msg.format(self.stream[self.offset:]))
    __repr__ = __str__
    def __bool__(self):
        return False

DEFAULT_FAILURE = Failure('This is the default failure for combinators that cannot generate their own failures.  It should never be returned.', offset=-1)


class Combinator:
    def __init__(self):
        raise NotImplementedError

    def parse(self, stream, offset=0):
        """Synchronization in the case where everything's a coroutine is
        maintained by the stack, but when there are regular functions
        involved, there has to be another way to figure out when to
        pop a generator from the stack.  Note: this is clearer when
        StopIteration has a return value, from PEP 380.

        This trampoline is based on Cooke's version.  I'm reasonably
        sure catching StopIteration would work for the trampoline
        based on Klode's/PEP 342's trampoline because they're more or
        less the same, but I think the implementation looks more
        difficult.  While this trampoline does propagate exceptions,
        the traceback includes the trampoline between each call to a
        combinator.

        """
        stack = []
        result = self._parse(stream, offset)
        error = None
        while 1:
            if isinstance(result, types.GeneratorType):
                # Push a new coroutine onto the stack and prepare to
                # initialize it.
                stack.append(result)
                result = None
            elif stack:
                # Send results into a coroutine until it raises
                # StopIteration; when it does, remove it from the
                # stack.  The last result it returned was its
                # final output.
                try:
                    if error:
                        result = stack[-1].throw(error)
                    else:
                        result = stack[-1].send(result)
                # Python 2
                # except BaseException as e:
                #     stack.pop()
                #     error = None if isinstance(e, StopIteration) else sys.exc_info()
                # Python 3
                except BaseException as e:
                    stack.pop()
                    error = None if isinstance(e, StopIteration) else e
            elif error:
                self._traceback(error)
            else:
                return result

    def trace(self, stream, offset=0):
        """Same trampoline as parse but prints a trace as the parser runs.

        This isn't very polished, since it duplicates a bunch of code
        in the basic trampoline.  I'm also concerned about the
        possibility of introducing cyclic references.  However, it
        contains the essential ideas.  The main deficiency of this
        trace is that it doesn't show the combinators that are pure
        functions, and I don't know how to fix that without
        redesigning everything in ways that would be counterproductive
        for performance and add unnecessary complexity.

        """
        def print_parser(parser, result, indent):
            name = parser.gi_frame.f_locals['self'].name if parser.gi_frame.f_locals['self'].name else type(parser.gi_frame.f_locals['self']).__name__
            if isinstance(result, (list, Failure)):
                print(indent, '{} yields:'.format(name), sep = '')
                print(indent, result, sep = '')
            else:
                print(indent, '{} started:'.format(name), sep = '')

        counter = collections.Counter()
        stack = []
        result = self._parse(stream, offset)
        error = None
        while 1:
            if isinstance(result, types.GeneratorType):
                # Push a new coroutine onto the stack and prepare to
                # initialize it.
                stack.append(result)
                result = None
            elif stack:
                # Send results into a coroutine until it raises
                # StopIteration; when it does, remove it from the
                # stack.  The last result it returned was its
                # final output.
                try:
                    if error:
                        result = stack[-1].throw(error)
                    else:
                        result = stack[-1].send(result)
                        print_parser(stack[-1], result, ' '*len(stack)*2)
                        counter[type(stack[-1].gi_frame.f_locals['self']).__name__] += 1
                except BaseException as e:
                    stack.pop()
                    error = None if isinstance(e, StopIteration) else e
            elif error:
                self._traceback(error, counter)
            else:
                return result

    def _traceback(self, e, counter = ''):
        """This produces correctly-formatted tracebacks, but does not play
        nicely with error handling that might exist outside this
        module.

        This is still broken in 2.7.

        """
        import inspect
        import traceback

        print('Traceback (most recent call last):')
        # Python 2
        # _, error, trace = e
        # Python 3
        trace = error.__traceback__
        # The trampoline should always be the first frame
        trampoline = None # trace.tb_frame.f_code
        while trace:
            frame = trace.tb_frame
            if trampoline is None or frame.f_code is not trampoline:
                frame_info = inspect.getframeinfo(frame)
                if frame.f_locals.get('self'):
                    if hasattr(frame.f_locals['self'], 'name') and frame.f_locals['self'].name:
                        name = frame.f_locals['self'].name + '.' + frame_info.function
                    else:
                        name = type(frame.f_locals['self']).__name__ + '.' + frame_info.function
                else:
                    name = frame_info.function
                print('  File, "{}", line {}, in {}'.format(frame_info.filename, frame_info.lineno, name))
                print('    %s' % frame_info.code_context[0].lstrip(), end='')
                if trampoline is None:
                    trampoline = trace.tb_frame.f_code
            trace = trace.tb_next
        print(*traceback.format_exception_only(type(error), error.args), end='')
        print(counter)
        raise type(error)(*error.args)

    # + is the operator Python uses for concatenation.
    def __add__(self, other):
        return Sequence(self, other)
    def __radd__(self, other):
        return Sequence(other, self)
    def __or__(self, other):
        return Alternation(self, other)
    def __ror__(self, other):
        return Alternation(other, self)
    # There's no standard syntax here so the >> operator seems as good as any.
    def __rshift__(self, act):
        return Action(self, act)


class Alternation(Combinator):
    def __init__(self, *combinators, name=None):
        self.combinators = list(combinators)
        self.name = name

    def _parse(self, stream, offset):
        successes = []
        failure = DEFAULT_FAILURE
        for combinator in self.combinators:
            result = (yield combinator._parse(stream, offset))
            if result:
                successes.extend(result)
            else:
                if failure.offset < result.offset:
                    failure = result
        if successes:
            yield successes
        else:
            yield failure

    def __or__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(self.combinators + other.combinators))
        else:
            return Alternation(*(self.combinators + [other]))
    def __ror__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(other.combinators + self.combinators))
        else:
            return Alternation(*([other] + self.combinators))
    def __ior__(self, other):
        if isinstance(other, Alternation):
            self.combinators.extend(other.combinators)
            return self
        else:
            self.combinators.append(other)
            return self


# Could also be called Concatenation.
class Sequence(Combinator):
    def __init__(self, *combinators, name=None):
        combinators = iter(combinators)
        self.combinators = [next(combinators, None)]
        for c in combinators:
            if isinstance(self.combinators[-1], Terminal) and isinstance(c, Terminal):
                self.combinators[-1] += c
            else:
                self.combinators.append(c)
        self.name = name

    def _parse(self, stream, offset):
        """This implementation is an implicit breadth-first search through the
        tree of successes.  Because it uses the trampoline's stack, it
        doesn't need its own queue.  Like the depth-first
        implementation, it keeps track of paths through the tree of
        successes.  The depth-first implementation is probably more
        memory-efficient.

        """
        failure = DEFAULT_FAILURE
        offsets = [offset]
        paths = [[]]
        for combinator in self.combinators:
            for o in offsets:
                result = (yield combinator._parse(stream, o))
                new_paths = []
                if result:
                    new_paths.extend([p + [s] for p in paths for s in result])
                else:
                    if failure.offset < result.offset:
                        # In a depth-first search there's a unique
                        # path to use for the tree in a Failure, but
                        # in a breadth-first search all possible
                        # new paths that could lead to this failure
                        # have been generated.  This arbitrarily picks
                        # the first path with the right offset.
                        for p in paths:
                            if not p:
                                path = []
                                break
                            if p[-1].offset == o:
                                path = p
                                break
                        if not path:
                            failure = Failure(result.msg, stream, result.offset)
                        else:
                            failure = Failure(result.msg, stream, result.offset, [s.tree for s in path])
            if not new_paths:
                yield failure
                return
            paths = new_paths
            offsets = [p[-1].offset for p in paths]
        yield [Success(stream, p[-1].offset, [s.tree for s in p]) for p in paths]

    def __add__(self, other):
        if isinstance(other, Sequence):
            return Sequence(*(self.combinators + other.combinators))
        else:
            return Sequence(*(self.combinators + [other]))
    def __radd__(self, other):
        if isinstance(other, Sequence):
            return Sequence(*(other.combinators + self.combinators))
        else:
            return Sequence(*([other] + self.combinators))
    def __iadd__(self, other):
        if isinstance(other, Sequence):
            self.combinators.extend(other.combinators)
            return self
        else:
            self.combinators.append(other)
            return self


class Terminal(Combinator):
    """This class could be expanded to handle alternations of all
    terminals as well as sequences/concatenations at the cost of
    making the add methods much more complicated.  In fact, by using
    re instead of startswith(), it could handle many simple parsing
    tasks by itself.  However, since I'm primarily concerned with
    parsing binary data, which will require struct rather than re, I'm
    not going to implement that at this point.
    """
    def __init__(self, string, name=None):
        self.string = string
        self.name = name

    def _parse(self, stream, offset):
        length = len(self.string)
        if (length > len(stream) - offset):
            return Failure('Unexpected end of stream (expected "%s")' % self.string)
        else:
            if stream.startswith(self.string, offset):
                return [Success(stream, offset + length, (offset, offset + length), self.string)]
            else:
                return Failure('Expected "%s" got "{}"' % self.string, stream, offset)

    def __add__(self, other):
        if isinstance(other, Terminal):
            return Terminal(self.string + other.string)
        else:
            return NotImplemented
    def __radd__(self, other):
        if isinstance(other, Terminal):
            return Terminal(other.string + self.string)
        else:
            return NotImplemented
    def __iadd__(self, other):
        if isinstance(other, Terminal):
            self.string += other.string
            return self
        else:
            return NotImplemented


class Recursive(Combinator):
    def __init__(self, name):
        """At the moment the name of this parser serves a dual purpose, I
        don't know if this is a problem."""
        # Check if name is a valid Python variable name; this string
        # method doesn't exist in Python 2.
        if name.isidentifier():
            self.name = name
        else:
            raise SyntaxError("Recursive initialized with a string that isn't a valid Python identifier: " + name)

    def _parse(self, stream, offset):
        combinator = globals()[self.name]
        if isinstance(combinator, Combinator):
            # When moving to GLL, need to find the object that parse()
            # belongs to to check for identity, becaue Recursive
            # will retain its own identity.
            self._parse = combinator._parse
            return combinator._parse(stream, offset)
        else:
            raise CombinatorTypeError("Recursive refers to an object that isn't a combinaor: {}".format(type(combinator)))


class Action(Combinator):
    def __init__(self, combinator, act, name=None):
        self.combinator = combinator
        self.act = act
        self.name = name

    def _parse(self, stream, offset):
        result = (yield self.combinator._parse(stream, offset))
        if result:
            yield [Success(stream, s.offset, *self.act(s.tree)) for s in result]
        else:
            yield result


if __name__ == '__main__':
    terminal = Terminal(b'01')
    print('Terminal success,', terminal.parse(b'010101'))
    print('Terminal failure,', terminal.parse(b'121212'))
    terminal = Sequence(Terminal(b'0'), Terminal(b'1'))
    print('Sequence terminal success,', terminal.parse(b'010101'))
    alternation = Alternation(Terminal(b'01'), Terminal(b'12'))
    print('Alternation success,', alternation.parse(b'121212'))
    terminal = Terminal(b'0') + Terminal(b'1')
    print('Add success,', terminal.parse(b'010101'))
    alternation = Terminal(b'01') | Terminal(b'12')
    print('Or success,', alternation.parse(b'121212'))
    sequence = Sequence(Terminal(b'0'), Alternation(Terminal(b'1'),
                                                    Terminal(b'2')))
    print('Sequence alternation success,', sequence.parse(b'012'))
    print('Sequence alternation failure,', sequence.parse(b'032'))

    t = Terminal('a')
    # print(t._parse)
    l = Recursive('t')
    # print(l._parse)
    print('Recursive,', l.parse('a', 0))
    # print(l._parse)
    print('Recursive,', l.parse('a', 0))

    # def int_tree(tree):
    #     return Tree({tree.root : int(tree[tree.root])})

    # num = (Terminal(b'0') | Terminal(b'1')) >> int_tree
    # num.name = 'num'
    # print('Calculator,', num.parse(b'010101'))
    # print('Calculator,', num.parse(b'101010'))

    # def add_tree(tree):
    #     return Tree({tree.root : tree[tree[tree.root][2]] + tree[tree[tree.root][0]]})

    # def subtract_tree(tree):
    #     return Tree({tree.root : tree[tree[tree.root][2]] - tree[tree[tree.root][0]]})

    # # expr = (num + Terminal(b'+') + Recursive('expr')) >> add_tree | (num + Terminal(b'-') + Recursive('expr')) >> subtract_tree | num
    # # expr.name = 'expr'

    # add = num + Terminal(b'+') + Recursive('expr')
    # add.name = 'add'
    # subtract = num + Terminal(b'-') + Recursive('expr')
    # subtract.name = 'subtract'

    # expr = add >> add_tree | subtract >> subtract_tree | num
    # expr.name = 'expr'

    num = (Terminal(b'0') | Terminal(b'1')) >> (lambda t: (t.root, int(t.nested_lists())))
    num.name = 'num'
    print('Calculator,', num.parse(b'010101'))
    print('Calculator,', num.parse(b'101010'))

    expr = (num + Terminal(b'+') + Recursive('expr')) >> (lambda t: (t.root, t.nested_lists()[2] + t.nested_lists()[0])) | (num + Terminal(b'-') + Recursive('expr')) >> (lambda t: (t.root, t.nested_lists()[2] - t.nested_lists()[0])) | num
    expr.name = 'expr'

    print('Calculator,', expr.parse(b'1+1'))
    print('Calculator,', expr.parse(b'1+1+1'))
    print('Calculator,', expr.parse(b'0+1-1+1+1'))
    print('Calculator,', expr.parse(b'1+1+1+1+1'))
    print('Calculator,', expr.parse(b'0-1-1-1-1'))
    print('Calculator,', expr.parse(b'1-1-2'))
    print('Calculator,', expr.parse(b'3'))

    print('Trace')
    print('Trace,', expr.trace(b'0+1-1+1+1'))

    ambiguous = (Terminal('a') + Recursive('ambiguous')) | (Terminal('aa') + Recursive('ambiguous')) | Terminal('')

    print('Ambiguous,', pprint.pformat(ambiguous.parse('aaaaa')))
