#!/usr/bin/python3

from __future__ import absolute_import, division, print_function

import abc
import collections
import inspect
import pprint
import re
import struct

import six


# TODO: properly segregate the GSS in its own data structure

# rewrite this with the faster GSS modifications.

# SPPF

# Replace copying of stream data with indices

# Rewrite the closures as classes and *profile*.

# Typing

# Swierstra's paper

# Use exceptions for continuation control flow, e.g. handling parse
# failures.


# Success = 

# Failure = 

# Label = collections.namedtuple('Label', 'combinator start end')

# DEFAULT_FAILURE = Failure('This is the default failure for combinators that cannot generate their own failures.  It should never be returned.', -1)


# Namedtuples test for structural equality which is bizarrely not what I want here.

# class Success(collections.namedtuple('Success', 'tree tail')):
#     def copy(self):
#         return Success(self.tree, self.tail)
#     def __str__(self):
#         return 'Success: ' + str(self.tree) + ", '" + str(self.tail) + "'"
#     __repr__ = __str__

# class Failure(collections.namedtuple('Failure', 'message tail')):
#     def copy(self):
#         return Failure(self.message, self.tail)
#     def __str__(self):
#         return 'Failure: ' + self.message % self.tail
#     __repr__ = __str__


def read_only(value):
    return property(lambda g: value)

class Result(object):
    pass

class Success(Result):
    def __init__(self, value, stream, index):
        self.value = value
        self.stream = stream
        self.index = index
    def copy(self):
        return Success(self.value, self.stream, self.index)
    def __str__(self):
        return 'Success: %r, %r' % (self.value, self.stream[self.index:])
    __repr__ = __str__

class Failure(Result):
    def __init__(self, message, stream, index):
        self.message = message
        self.stream = stream
        self.index = index
    def copy(self):
        return Failure(self.message, self.stream, self.index)
    def __str__(self):
        return 'Failure: %s' % (self.message % self.stream[self.index:])
    __repr__ = __str__


class GraphStructuredStack(object):
    NodeLabel = read_only(collections.namedtuple('NodeLabel', 'nonterminal index'))
    Node = read_only(collections.namedtuple('Node', 'edges U P'))
    # EdgeLabel = read_only(collections.namedtuple('EdgeLabel', 'descriptor tree'))
    # Edge = read_only(collections.namedtuple('Edge', 'to'))
    GrammarSlot = read_only(collections.namedtuple('GrammarSlot', 'nonterminal slot'))
    Descriptor = read_only(collections.namedtuple('Descriptor', 'grammar_slot node index'))

    def __init__(self):
        self.nodes = collections.defaultdict(lambda: self.Node({}, set(), set()))
        self.dispatch_stack = [] # 

    def create(self, L: GrammarSlot, u: Node, index: int, tree):
        v = self.nodes[self.NodeLabel(L.nonterminal, index)]
        # if self.EdgeLabel(L, tree) not in v.edges:
        if L not in v.edges:
            # v.edges[EdgeLabel(L, tree)] = u
            v.edges[L] = u
            for k in v.P:
                self.add(L, u, k)
        return v

    def pop(self, u, index):
        if index not in u.P:
            u.P.add(i)
            for L in u.edges:
                self.add(L, u.edges[L], i)

    def add(self, L: GrammarSlot, node: Node, index: int):
        d = self.Descriptor(L, node, index)
        if d not in u.U:
            self.dispatch_stack.append(d)
            u.U.add(L)


def main_loop():
    gss = GraphStructuredStack()
    # Initialize stuff
    while gss.dispatch_stack:
        # c_i = index, c_u = gss_node, c_n = sppf_node
        L, u, i, w = gss.dispatch_stack.pop() # GrammarSlot, GraphStructuredStack.node, int, SPPFnode



class Combinator(six.with_metaclass(abc.ABCMeta, object)):
    # Since combinators pass the same input stream among themselves,
    # to avoid generating runtime errors due to passing text to
    # terminal combinators expecting binary data or binary data to
    # combinators expecting text, all combinators will be assigned as
    # either text or binary data.  Higher-order combinators inherit
    # their type from their sub-combinators while terminals have their
    # data type explicitly defined.  While some terminals are always
    # binary (struct), others depend on how they're initialized (re,
    # string.startswith, and possibly bits depending on which backend
    # I use and how I set up the API).  In Python 2, objects of type
    # 'str' are treated as binary data and objects of type 'unicode'
    # are treated as text.  In Python 3, objects of type 'str' are
    # treated as text and types 'bytes' and 'bytearray' are treated as
    # binary data.  Objects of type 'memoryview' are binary on both.
    # This can lead to weird situations where the Strings combinator
    # accepts binary data, but that's a consequence of Python's design
    # and not something I can do anything about.

    # Testing on 3.4 and 2.7 suggests that struct doesn't actually
    # care what it's given, bytes, str, or unicode, but enforcing
    # consistency on its input won't hurt.  array accepts *only*
    # native strings, so str/bytes on Python 2 and str/unicode on
    # Python 3.
    @abc.abstractmethod
    def __init__(self, arg_type, *combinators, **kws):
        # print(self, arg_type, combinators, kws)
        if len(combinators) == 0:
            raise TypeError('%s takes at least 1 argument.' % type(self).__name__)
        if any(not isinstance(c, arg_type) for c in combinators):
            raise TypeError('Arguments to %s must be %ss.' % (type(self).__name__, arg_type.__name__))

    # Temporarily disabled until GSS segregation
    # def __setattr__(self, key, value):
    #     raise AttributeError('%s object does not support item assignment.' % type(self).__name__)
    # def __delattr__(self, key):
    #     raise AttributeError('%s object does not support item deletion.' % type(self).__name__)

    def __add__(self, other):
        """ + is the operator Python uses for concatenation."""
        return Sequence(self, other)
    def __radd__(self, other):
        return Sequence(other, self)
    def __or__(self, other):
        return Alternation(self, other)
    def __ror__(self, other):
        return Alternation(other, self)
    def __rshift__(self, act):
        """There's no standard syntax here so the >> operator seems as good
        as any."""
        return Action(self, act)

    def parse(self, stream, index=0):
        self.stack = []
        # A map of a stream position to a map mapping combinator
        # instances to sets of continuations.  In the standard GLL
        # algorithm, this seems to correspond to the GSS nodes, though
        # note that Spiewak seems to have already transformed the GSS
        # like Afroozeh and Izmaylova, with nodes recorded using only
        # the nonterminal/combinator instance and the input position.
        # Spiewak uses function identity to avoid needing to keep
        # track of edge labels.
        self.backlinks = {}
        # A map of a stream position to sets of combinators.  This
        # corresponds to U.
        self.done = {}
        # A map of a stream position to a map mapping combinator instances
        # to sets of successes.  This corresponds to P.
        self.popped = {}
        # A map of Results to sets of functions.
        self.saved = {}

        successes = set()
        failures = set()
        def nonterminal_success(result, failure, index):
            # if result.tail:
            #     failures.add(Failure('Unexpected trailing characters: "{}"'.format(str(result.tail))))
            # else:
            # Ugly hack
            if isinstance(result, Result):
                successes.add(result)
            else:
                successes.add(Success(result, stream, index))

        def nonterminal_failure(result, index):
            if isinstance(result, Result):
                failures.add(result)
            else:
                failures.add(Failure(result, stream, index))

        self._parse(self, nonterminal_success, nonterminal_failure, stream, index)

        while self.stack:
            # print('Main loop:', self)
            combinator, index = self.stack.pop()

            # These functions all correspond to the pop() method in
            # the standard GLL algorithm.
            def setup_popped(combinator=combinator, index=index):
                # print('Popped:', pprint.pformat(self.popped), combinator, stream, sep='\n')
                if index not in self.popped:
                    self.popped[index] = {}
                if combinator not in self.popped[index]:
                    self.popped[index][combinator] = set()

            # The saved set is not part of the original algorithm,
            # Spiewak added it.  He's using result identity here
            # to check if something's been done, but there has to
            # be a better way.
            def setup_saved(result):
                if result not in self.saved:
                    self.saved[result] = set()

            def trampoline_success(tree, failure, current_index, past_index=index, combinator=combinator, setup_popped=setup_popped):
                result = Success(tree, stream, current_index)
                # print('Trampoline success: ', tree, pprint.pformat(self.backlinks), pprint.pformat(self.saved), sep='\n')
                # print('Trampoline success:', pprint.pformat(self.popped), combinator, stream, sep='\n')
                setup_popped()
                self.popped[past_index][combinator].add(result)
                setup_saved(result)
                for success in self.backlinks[past_index][combinator]:
                    if success not in self.saved[result]:
                        self.saved[result].add(success)
                        # print(success, tree, failure, current_index)
                        success(tree, failure, current_index)

            # def trampoline_failure(message, current_index, combinator=combinator, past_index=index, setup_popped=setup_popped):
            #     result = Failure(message, stream, current_index)
            #     setup_popped()
            #     setup_saved(result)
            #     for success in self.backlinks[past_index][combinator]:
            #         # print(success, failure)
            #         if success not in self.saved[result]:
            #             self.saved[result].add(success)

            combinator._parse(self, trampoline_success, nonterminal_failure, stream, index)

        if successes:
            return list(successes)
        else:
            return list(failures)

    def add(self, combinator, success, failure, index):
        # print('Add:', self)
        if index not in self.backlinks:
            self.backlinks[index] = {}
        if combinator not in self.backlinks[index]:
            self.backlinks[index][combinator] = set()
        if success not in self.backlinks[index][combinator]:
            self.backlinks[index][combinator].add(success)
        if index in self.popped and combinator in self.popped[index]:
            for result in self.popped[index][combinator].copy():
                success(result.value, failure, result.index)
        else:
            if index not in self.done:
                self.done[index] = set()
            if combinator not in self.done[index]:
                self.stack.append((combinator, index))
                self.done[index].add(combinator)

    # def __str__(self):
    #     return '\n'.join(['Trampoline', 'Stack', pprint.pformat(self.stack), 'Backlinks', pprint.pformat(self.backlinks), 'Done', pprint.pformat(self.done), 'Popped', pprint.pformat(self.popped), 'Saved', pprint.pformat(self.saved)])

    @abc.abstractmethod
    def _parse(self, trampoline, success, failure, stream, index):
        raise NotImplementedError

    # def unparse(self, tree, stream, index=0):
    #     pass

    # def _unparse(self, tree, stream, index):
    #     raise NotImplementedError


class Alternation(Combinator):
    def __init__(self, *combinators, **kws):
        super(Alternation, self).__init__(Combinator, *combinators, **kws)
        vars(self)['combinators'] = frozenset(combinators)

    def _parse(self, trampoline, success, failure, stream, index):
        for combinator in self.combinators:
            trampoline.add(combinator, success, failure, index)

    def __or__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(self.combinators | other.combinators))
        else:
            return Alternation(*(self.combinators | frozenset([other])))
    def __ror__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(other.combinators | self.combinators))
        else:
            return Alternation(*(frozenset([other]) | self.combinators))


# In the standard version of the GLL algorithm, the GSS stores the
# position within a nonterminal, what they call the grammar slot.
# Spiewak's version doesn't because it's forcing all nonterminals to
# be length 2 and using separate continuations for processing the
# first grammar slot and the second grammar slot.  Allowing the use of
# arbitrary-length sequences will require modifying the GSS handling.

class Sequence(Combinator):
    def __init__(self, left, right, **kws):
        vars(self)['left'] = left
        vars(self)['right'] = right

    def _parse(self, trampoline, success, failure, stream, index):
        def left_success(tree1, failure, index):
            def right_success(tree2, failure, index):
                success((tree1, tree2), failure, index)
            self.right._parse(trampoline, right_success, failure, stream, index)
        self.left._parse(trampoline, left_success, failure, stream, index)


# class Sequence(Combinator):
#     def __init__(self, *combinators, **kws):
#         super(Sequence, self).__init__(Combinator, *combinators, **kws)
#         vars(self)['combinators'] = combinators

#     def _parse(self, trampoline, success, failure, stream, index):
#         trees = []
#         # The clean way to do is with a separate index variable,
#         # but Python 2.7 doesn't allow an inner function to alter
#         # the variable of an outer one.  The proper way of working
#         # around this is probably using classes instead of
#         # closures because classes have mutable state.
#         combinators = iter(self.combinators)
#         index = 0
#         def sequence_continuation(tree, failure, stream, index):
#             nonlocal index
#             index += 1
#             # print('Sequence continuation:', index, sequence_continuation, success, trees, failure, stream, index)
#             trees.append(tree)
#             try:
#                 combinator = next(combinators)
#             except StopIteration:
#                 # print('Sequence continuation call:', success, trees, failure, stream, index)
#                 success(tuple(trees), failure, stream, index)
#                 return
#             combinator._parse(trampoline, sequence_continuation, failure, stream, index)
#         next(combinators)._parse(trampoline, sequence_continuation, failure, stream, index)

#     def __add__(self, other):
#         if isinstance(other, Sequence):
#             return Sequence(*(self.combinators + other.combinators))
#         else:
#             return Sequence(*(self.combinators + (other,)))
#     def __radd__(self, other):
#         if isinstance(other, Sequence):
#             return Sequence(*(other.combinators + self.combinators))
#         else:
#             return Sequence(*((other,) + self.combinators))
#     def __mul__(self, other):
#         type(self)(*(other * self.combinators))
#     __rmul__ = __mul__


class Lazy(Combinator):
    def __init__(self, name):
        if (six.PY3 and name.isidentifier()) or name.isalnum():
            vars(self)['name'] = name
        else:
            raise SyntaxError("Lazy initialized with a string that isn't a valid Python identifier: %s" % name)

    def combinator(self):
        try:
            return vars(self)['combinator']
        except KeyError:
            frames = inspect.getouterframes(inspect.currentframe())
            # print(frames)
            try:
                for frame in frames:
                    combinator = frame[0].f_locals.get(self.name, None)
                    if combinator:
                        break
                    else:
                        combinator = frame[0].f_globals.get(self.name, None)
                        if combinator:
                            break
                else:
                    raise NameError("Name '%s' isn't defined" % self.name)
            finally:
                del frames
            if isinstance(combinator, Combinator):
                vars(self)['combinator'] = combinator
                return combinator
            else:
                raise TypeError("'%s' refers to an object that is a %s instead of a combinator." % (self.name, type(combinator)))

    combinator = property(combinator)

    def _parse(self, trampoline, success, failure, stream, index):
        combinator = self.combinator
        self._parse = combinator._parse
        combinator._parse(trampoline, success, failure, stream, index)


class Action(Combinator):
    def __init__(self, combinator, action):
        self.combinator = combinator
        self.action = action

    def _parse(self, trampoline, success, failure, stream, index):
        def action_continuation(tree, failure, index):
            # print('Action:', tree, self.action(tree))
            success(self.action(tree), failure, index)
        self.combinator._parse(trampoline, action_continuation, failure, stream, index)


class Terminal(Sequence):
    """This inherits from Sequence: note that there's no real distinction
    between a sequence of terminals and a single terminal.

    Parsers have a type of input they expect, and you can't mix
    parsers that accept different kinds of input.  Note this is
    different from passing output to a subparser using Act/>>.

    """
    def __init__(self, *combinators, **kws):
        # TODO: should rework this to not hard-code Combinator
        Combinator.__init__(self, Terminal, *combinators, **kws)
        vars(self)['combinators'] = combinators

    def parse(self, stream, index=0):
        result = None
        def terminal_success(tree, failure, index):
            nonlocal result
            result = Success(tree, stream, index)
        def terminal_failure(message, index):
            nonlocal result
            result = Failure(message, stream, index)
        self._parse(None, terminal_success, terminal_failure, stream, index)
        return result

    def _parse(self, trampoline, success, failure, stream, index):
        trees = []
        # The clean way to do is with a separate index variable,
        # but Python 2.7 doesn't allow an inner function to alter
        # the variable of an outer one.  The proper way of working
        # around this is probably using classes instead of
        # closures because classes have mutable state.
        combinators = iter(self.combinators)
        def terminal_continuation(tree, failure, index):
            # print('Sequence continuation:', index, sequence_continuation, success, trees, failure, stream)
            trees.append(tree)
            try:
                combinator = next(combinators)
            except StopIteration:
                # print('Sequence continuation call:', success, trees, failure, stream)
                success(tuple(trees), failure, index)
                return
            combinator._parse(trampoline, terminal_continuation, failure, stream, index)
        next(combinators)._parse(trampoline, terminal_continuation, failure, stream, index)

    def __add__(self, other):
        if isinstance(other, Terminal):
            if type(self) is Terminal and type(other) is Terminal:
                return Terminal(*(self.combinators + other.combinators))
            elif type(self) is Terminal:
                return Terminal(*(self.combinators + (other,)))
            elif type(other) is Terminal:
                return Terminal(*((self,) + other.combinators))
            else:
                return Terminal(self, other)
        else:
            return NotImplemented

    def __radd__(self, other):
        if isinstance(other, Terminal):
            if type(self) is Terminal and type(other) is Terminal:
                return Terminal(*(other.combinators + self.combinators))
            elif type(self) is Terminal:
                return Terminal(*((other,) + self.combinators))
            elif type(other) is Terminal:
                return Terminal(*(other.combinators) + (self,))
            else:
                return Terminal(other, self)
        else:
            return NotImplemented


class Strings(Terminal):
    def __init__(self, *strings, **kws):
        vars(self)['strings_lengths'] = [(s, len(s)) for s in strings]
        vars(self)['combinators'] = (self,)

    def _parse(self, trampoline, success, failure, stream, index):
        trees = []
        for string, length in self.strings_lengths:
            if (length > len(stream) - index):
                return failure('Unexpected end of stream (expected %r, got %%r)' % string, index)
            else:
                if stream.startswith(string, index):
                    trees.append(string)
                    index += length
                else:
                    return failure('Expected %r got %%r' % string, index)
        return success(tuple(trees), failure, index)

    def __str__(self):
        return 'Strings(%s)' % ', '.join(repr(s) for s, _ in self.strings_lengths)
    __repr__ = __str__


class Cache(dict):
    def __init__(self, factory, *args, **kws):
        self.factory = factory
        super(Cache, self).__init__(*args, **kws)
    @classmethod
    def fromkeys(cls, factory, seq, value=None):
        instance = super(Cache, cls).fromkeys(seq, value)
        instance.factory = factory
        return instance
    def __missing__(self, key):
        return self.factory(key)


class Regex(Terminal):
    regexes = read_only(Cache(re.compile))

    def __init__(self, pattern, **kws):
        vars(self)['regex'] = self.regexes[pattern]
        vars(self)['combinators'] = (self,)

    def _parse(self, trampoline, success, failure, stream, index):
        match = self.regex.match(stream, index)
        if match:
            # This API is kind of ugly, a regex always needs at least
            # one group to return a tree element correctly.
            return success(match.groups(), failure, match.end())
        else:
            return failure("%r didn't match %%r" % self.regex.pattern, index)

    def __str__(self):
        return 'Regex(%r)' % self.regex.pattern
    __repr__ = __str__


class Binary(Terminal):
    structs = read_only(struct.Struct)

    def __init__(self, format_string, **kws):
        vars(self)['struct'] = self.structs[format_string]
        vars(self)['combinators'] = (self,)

    def _parse(self, trampoline, success, failure, stream, index):
        try:
            return success(self.struct.unpack_from(stream, index), index + self.struct.size)
        except struct.error as error:
            return failure(error.args[0] + 'at %r', index)


if __name__ == '__main__':
    import cProfile
    import platform
    import sys
    import time
    import timeit

    CPYTHON = True if platform.python_implementation() == 'CPython' else False
    if six.PY3 and CPYTHON:
        import tracemalloc
        tracemalloc.start()

    import trace_calls

    trace_calls = trace_calls.TraceCalls(files=('continuation_gll_combinators.py',))
    # sys.settrace(trace_calls)

    # The implementation in Spiewak's paper doesn't seem to be
    # complete because the only parser that will ever return
    # "Unexpected trailing characters" is a non-terminal parser.
    strings = Strings('ab')
    print('Strings success,', strings.parse('ababab'))
    print('Strings failure,', strings.parse('bcbcbc'))

    terminal = Strings('a') + Strings('b')
    print('Terminal success,', terminal.parse('ababab'))
    terminal = Terminal(Strings('a'), Strings('b'))
    print('Terminal success,', terminal.parse('ababab'))

    sequence = Sequence(Strings('abc'), Strings('def')) + Strings('ghi') + Strings('jkl')
    print('Sequence success,', sequence.parse('abcdefghijkl'))
    # sequence = Strings('abc') + Strings('def') + Strings('ghi') + Strings('jkl')
    # print('Sequence success,', sequence.parse('abcdefghijkl'))
    # sequence = Sequence(Strings('abc'), Strings('def'), Strings('ghi'), Strings('jkl'))
    # print('Sequence success,', sequence.parse('abcdefghijkl'))    

    alternation = Strings('ab') | Strings('bc')
    print('Alternation success,', alternation.parse('bcbcbc'))
    alternation = Alternation(Strings('ab'), Strings('bc'))
    print('Alternation success,', alternation.parse('bcbcbc'))

    sequence_alternation = Strings('a') + (Strings('b') | Strings('c'))
    print('Sequence alternation success,', sequence_alternation.parse('abc'))
    print('Sequence alternation success,', sequence_alternation.parse('acb'))
    print('Sequence alternation failure,', sequence_alternation.parse('cba'))
    sequence_alternation = Sequence(Strings('a'), Alternation(Strings('b'), Strings('c')))
    print('Sequence alternation success,', sequence_alternation.parse('abc'))
    print('Sequence alternation success,', sequence_alternation.parse('acb'))
    print('Sequence alternation failure,', sequence_alternation.parse('cba'))

    alpha = Regex('([a-zA-Z])')
    hex_char = Regex('([a-fA-F0-9])')

    alpha_or_hex = alpha | hex_char
    print('Alpha success,', alpha_or_hex.parse('xyz'))
    print('Alpha and hex success,', alpha_or_hex.parse('ABC'))
    print('Hex success,', alpha_or_hex.parse('123'))

    a = Strings('a')
    l = Lazy('a')
    print('Lazy,', l.parse('a'))
    print('Lazy,', l.parse('a'))
    
    ambiguous = (Lazy('ambiguous') + Lazy('ambiguous') + Lazy('ambiguous')) | (Lazy('ambiguous') + Lazy('ambiguous')) | Strings('a')
    print('Highly ambiguous,', ambiguous.parse('aaa'))
    # pprint.pprint(ambiguous.combinators)

    # There's a major divergence from Koopman and Plasmeijer here
    # because regexes behave like their deterministic combinators, but
    # deterministic behavior at the word level here is probably more
    # realistic for profiling.
    word = Regex('([a-zA-Z]+)')
    sentence = ((word + Strings('.')) >> (lambda t: t[0])) | ((word + Regex(r'[\s,]') + Lazy('sentence')) >> (lambda t: t[0][0] + t[1]))
    print('Sentence success,', sentence.parse('The quick brown fox jumps over the lazy dog.'))

    num = (Strings('0') | Strings('1')) >> (lambda t: int(t[0]))
    print('Calculator,', num.parse('010101'))
    print('Calculator,', num.parse('101010'))

    expr = (num + Strings('+') + Lazy('expr')) >> (lambda t: t[0][0] + t[1]) | (num + Strings('-') + Lazy('expr')) >> (lambda t: t[0][0] - t[1]) | num

    print('Calculator,', expr.parse('1+1'))
    print('Calculator,', expr.parse('1+1+1'))
    print('Calculator,', expr.parse('0+1-1+1+1'))
    print('Calculator,', expr.parse('1+1+1+1+1'))
    print('Calculator,', expr.parse('0-1-1-1-1'))
    print('Calculator,', expr.parse('1-1-2'))
    print('Calculator,', expr.parse('3'))

    dictionary = [w.strip().replace("'", '') for w in open('/usr/share/dict/words', 'r').read().splitlines() if w.strip().isalpha()]
    sample = ' '.join(dictionary[:10000]) + '.'

    start = time.clock()
    sentence.parse(sample)
    end = time.clock()
    print('Dictionary, %.4f seconds' % (end - start,))

    print('Highly ambiguous')
    def time_ambiguous(max_length):
        for i in range(2, max_length):
            print(i, timeit.timeit('ambiguous.parse("' + i * 'a' + '")', 'gc.enable(); from __main__ import ambiguous', number=1000), 'seconds')

    cProfile.run('''
time_ambiguous(9)
''')

    if six.PY3 and CPYTHON:
        snapshot = tracemalloc.take_snapshot()
        top_stats = snapshot.statistics('lineno')

        print("[ Top 10 ]")
        for stat in top_stats[:10]:
            print(stat)

    print('Stack depth:', trace_calls.max_depth)
