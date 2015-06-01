#!/usr/bin/python3

# Primary unresolved issues:

# How do I handle trees of different lengths in the same SPPF?  Is
# this even possible?

# As far as I can tell, I can pack together nodes of different lengths
# just fine.

# When nodes are labeled with parsers, I often end up with useless
# nodes: a Sequence or Alternation node with only a link to one other
# node.  How do I handle this?

# Solution: have parsers return an SPPF label instead of an offset.
# This allows me to write parsers that are completely transparent:
# Recursive will return the SPPF label of the parser that it contains,
# so it will never directly interact with the SPPF.  Note: I could
# also cache failures.  Would that be worthwhile?  Probably not.

# The big problem: how do I apply semantic actions to each tree of a
# forest while passing the SPPF as a mutable object?  Moreover, how do
# I do so space-efficiently?

# Partial answer: when I encounter a packed node while executing a
# semantic action, the semantic action needs to be executed on *all*
# of the nodes inside the packed node.

# Another problem: nodes are going to be created in the SPPF in parse
# branches that eventually terminate.  I need to reclaim the memory
# from these nodes.  How?

# The answer comes from one of the Yakker papers: use a weak
# dictionary so that nodes added in branches that fail get
# garbage-collected.


import types
import pprint
import collections

from spf import SeqNode, PackedNode, SharedPackedForest, Leaf, TreeView, Node


Success = collections.namedtuple('Success', 'node label offsets')

Failure = collections.namedtuple('Failure', 'msg offset')

# I don't understand why I need the ending offset so I'm only using
# the starting one.
Label = collections.namedtuple('Label', 'combinator offset')

DEFAULT_FAILURE = Failure('This is the default failure for combinators that cannot generate their own failures.  It should never be returned.', -1)


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
        sppf = SharedPackedForest()
        result = self._parse(stream, offset, sppf)
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
                except BaseException as e:
                    stack.pop()
                    error = None if isinstance(e, StopIteration) else e
            elif error:
                self.__traceback(error)
            else:
                if isinstance(result, Success):
                    sppf.root = result.node
                return (result, stream, sppf)

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
            if isinstance(result, (Success, Failure)):
                print(indent, '{} yields:'.format(name), sep='')
                print(indent, result, sep = '')
            else:
                print(indent, '{} started:'.format(name), sep='')

        counter = collections.Counter()
        stack = []
        sppf = SharedPackedForest((0, len(stream)))
        result = self._parse(stream, offset, sppf)
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
                self.__traceback(error, counter)
            else:
                if isinstance(result, Success):
                    sppf.root = result.node
                return (result, stream, sppf)

    def __traceback(self, error, counter = ''):
        """This produces correctly-formatted tracebacks, but does not play
        nicely with error handling that might exist outside this
        module.

        """
        import inspect
        import traceback

        print('Traceback (most recent call last):')
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

    def __repr__(self):
        if hasattr(self, 'name') and self.name:
            return '<%s (%s)>' % (self.name, type(self).__name__)
        else:
            return '<%s>' % type(self).__name__

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

    def _parse(self, stream, offset, sppf):
        successes = []
        failure = DEFAULT_FAILURE
        for combinator in self.combinators:
            result = (yield combinator._parse(stream, offset, sppf))
            if isinstance(result, Success):
                successes.append(result)
            else:
                if failure.offset < result.offset:
                    failure = result
        if len(successes) == 1:
            # Nothing to pack if there's only one successful branch
            yield successes[0]
        elif len(successes) > 1:
            label = Label(self, offset)
            try:
                node = sppf._nodes[label]
            except KeyError:
                node = PackedNode({s.node for s in successes})
                sppf._nodes[label] = node
            yield Success(node, label, {o for s in successes for o in s.offsets})
        else:
            yield failure

        # """I believe but am not sure that nodes can only be packed together
        # if they cover the same partition."""
        # packed_nodes = collections.defaultdict(list)
        # failure = DEFAULT_FAILURE
        # for combinator in self.combinators:
        #     result = (yield combinator._parse(stream, offset, sppf))
        #     if isinstance(result, Success):
        #         for o in result.labels:
        #             packed_nodes[o].append((combinator, offset, o))
        #     else:
        #         if failure.offset < result.offset:
        #             failure = result
        # if packed_nodes:
        #     for end in packed_nodes:
        #         if len(packed_nodes[end]) > 1:
        #             sppf._nodes[(self, offset, end)] = PackedNode([sppf._nodes.pop(n) for n in packed_nodes[end]])
        #         else:
        #             # This is a very inelegant solution to the problem
        #             # of other combinators not being able to find
        #             # nodes when they contain alternations as
        #             # sub-combinators.
        #             sppf._nodes[(self, offset, end)] = sppf._nodes[packed_nodes[end][0]]
        #     yield Success(frozenset(packed_nodes.keys()))
        # else:
        #     yield failure


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

    def _parse(self, stream, offset, sppf):
        """This implementation is an implicit breadth-first search through the
        tree of successes.  Because it uses the trampoline's stack, it
        doesn't need its own queue.  Like the depth-first
        implementation, it keeps track of paths through the tree of
        successes.  The depth-first implementation is probably more
        memory-efficient.

        """
        failure = DEFAULT_FAILURE
        offsets = {offset}
        paths = [[]]
        for combinator in self.combinators:
            new_paths = []
            for o in offsets:
                result = (yield combinator._parse(stream, o, sppf))
                if isinstance(result, Success):
                    new_paths.extend([p + [result] for p in paths])
                else:
                    if failure.offset < result.offset:
                        failure = result
            if not new_paths:
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
                    if p[-1].label.offset == o:
                        path = p
                        break
                yield failure
                return
            paths = new_paths
            offsets = {o for p in paths for o in p[-1].offsets}
        label = Label(self, offset)
        try:
            node = sppf._nodes[label]
        except KeyError:
            if len(paths) == 1:
                node = SeqNode([i.node for i in paths[0]])
            else:
                # I'm concerned these nodes aren't going to have labels
                # but don't see how else to do it.
                node = PackedNode([SeqNode([i.node for i in p]) for p in paths])
            sppf._nodes[label] = node
        yield Success(node, label, offsets)
 
        # failure = DEFAULT_FAILURE
        # offsets = {offset}
        # paths = [[]]
        # for combinator in self.combinators:
        #     for o in offsets:
        #         result = (yield combinator._parse(stream, o, sppf))
        #         new_paths = []
        #         if isinstance(result, Success):
        #             new_paths.extend([p + [s] for p in paths for s in result])
        #             new_paths.extend([p + [(combinator, o, s)] for p in paths for s in result.offsets])
        #         else:
        #             if failure.offset < result.offset:
        #                 failure = result
        #     if not new_paths:
        #         # In a depth-first search there's a unique
        #         # path to use for the tree in a Failure, but
        #         # in a breadth-first search all possible
        #         # new paths that could lead to this failure
        #         # have been generated.  This arbitrarily picks
        #         # the first path with the right offset.
        #         for p in paths:
        #             if not p:
        #                 path = []
        #                 break
        #             if p[-1][2] == o:
        #                 path = p
        #                 break
        #         if path:
        #             sppf._nodes[(self, path[0][1], path[-1][2])] = SeqNode(path)
        #         yield failure
        #         return
        #     paths = new_paths
        #     offsets = {p[-1][2] for p in paths}
        # for path in paths:
        #     sppf._nodes[(self, path[0][1], path[-1][2])] = SeqNode(path)    
        # yield Success(labels)


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

    def _parse(self, stream, offset, sppf):
        length = len(self.string)
        if (length > len(stream) - offset):
            return Failure('Unexpected end of stream (expected "%s")' % self.string, offset)
        else:
            if stream.startswith(self.string, offset):
                label = Label(self, offset)
                try:
                    node = sppf._nodes[label]
                except KeyError:
                    node = Leaf(self.string)
                    sppf._nodes[label] = node
                return Success(node, label, {offset + length})
            else:
                return Failure('Expected "%s" got "{}"' % self.string, offset)

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

    def _parse(self, stream, offset, sppf):
        combinator = globals()[self.name]
        if isinstance(combinator, Combinator):
            self._parse = combinator._parse
            return combinator._parse(stream, offset, sppf)
        else:
            raise TypeError("Recursive refers to an object that isn't a combinaor: {}".format(type(combinator)))


class Action(Combinator):
    def __init__(self, combinator, act, name=None):
        """The idea here is to let users of the library write a function that
        acts on a tree.  Then, _parse transforms the input function
        from one that acts on trees to one that acts on SPPFs.
        Because of subtree sharing, an impure input function will work
        in the case of trees but badly break the SPPF, so this is a
        very good reason for the node classes to be immutable.  (I
        can't do anything about mutable Python objects in the leaves,
        but most often those are going to be immutable strings, bytes,
        or ints anyways.)  This prototype still doesn't handle the
        case where f transforms the input tree into another tree.
        Note that because the node that Action creates doesn't include
        a strong reference to the node that Action is passed, that
        node and its descendants get garbage-collected.

        """
        self.act = act
        self.combinator = combinator
        self.name = name

    def _parse(self, stream, offset, sppf):
        result = (yield self.combinator._parse(stream, offset, sppf))
        if isinstance(result, Success):
            label = Label(self, offset)
            if isinstance(result.node, Leaf):
                node = Leaf(self.act(result.node.value))
            else:
                nodes = [Leaf(self.act(t)) for t in sppf.trees(result.node)]
                if len(nodes) == 1:
                    node = nodes[0]
                else:
                    node = PackedNode(nodes)
            try:
                node = sppf._nodes[label]
            except KeyError:
                sppf._nodes[label] = node
            yield Success(node, label, result.offsets)
        else:
            yield result


if __name__ == '__main__':
    def print_parse(string, output):
        result, stream, sppf = output
        if isinstance(result, Success):
            print(string, 'Success:\n%s\n%s' % (sppf, pprint.pformat([stream[o:] for o in result.offsets])))
        else:
            print(string, 'Failure: {}\n{}\n{}'.format(result.msg.format(stream[result.offset:]), sppf, stream[result.offset:]))

    terminal = Terminal(b'01')
    print_parse('Terminal success,', terminal.parse(b'010101'))
    print_parse('Terminal failure,', terminal.parse(b'121212'))
    terminal = Sequence(Terminal(b'0'), Terminal(b'1'))
    print_parse('Sequence terminal success,', terminal.parse(b'010101'))
    alternation = Alternation(Terminal(b'01'), Terminal(b'12'))
    print_parse('Alternation success,', alternation.parse(b'121212'))
    terminal = Terminal(b'0') + Terminal(b'1')
    print_parse('Add success,', terminal.parse(b'010101'))
    alternation = Terminal(b'01') | Terminal(b'12')
    print_parse('Or success,', alternation.parse(b'121212'))
    sequence = Sequence(Terminal(b'0'), Alternation(Terminal(b'1'),
                                                    Terminal(b'2')))
    print_parse('Sequence alternation success,', sequence.parse(b'012'))
    print_parse('Sequence alternation failure,', sequence.parse(b'032'))

    t = Terminal('a')
    # print_parse(t._parse)
    l = Recursive('t')
    # print_parse(l._parse)
    print_parse('Recursive,', l.parse('a', 0))
    # print_parse(l._parse)
    print_parse('Recursive,', l.parse('a', 0))

    ambiguous = (Terminal('a') + Recursive('ambiguous')) | (Terminal('aa') + Recursive('ambiguous')) | Terminal('')

    print_parse('Ambiguous,', ambiguous.parse('aaa'))
    _, _, sppf = ambiguous.parse('aaa')
    for t in sppf:
        print(t)

    num = (Terminal(b'0') | Terminal(b'1')) >> int
    num.name = 'num'
    print_parse('Calculator,', num.parse(b'010101'))
    print_parse('Calculator,', num.parse(b'101010'))

    expr = (num + Terminal(b'+') + Recursive('expr')) >> (lambda t: t[2] + t[0]) | (num + Terminal(b'-') + Recursive('expr')) >> (lambda t: t[2] - t[0]) | num
    expr.name = 'expr'

    print_parse('Calculator,', expr.parse(b'1+1'))
    print_parse('Calculator,', expr.parse(b'1+1+1'))
    print_parse('Calculator,', expr.parse(b'0+1-1+1+1'))
    print_parse('Calculator,', expr.parse(b'1+1+1+1+1'))
    print_parse('Calculator,', expr.parse(b'0-1-1-1-1'))
    print_parse('Calculator,', expr.parse(b'1-1-2'))
    print_parse('Calculator,', expr.parse(b'3'))

    print('Trace')
    print_parse('Trace,', expr.trace(b'0+1-1+1+1'))
