#!/usr/bin/python3

from __future__ import absolute_import, division, print_function

import abc
import collections
import inspect
import pprint
import re
import struct

import six

from spf import LabeledForest, BaseForest


# TODO: properly segregate the GSS in its own data structure

# rewrite this with the faster GSS modifications.

# SPPF

# Replace copying of stream data with indices

# Rewrite the closures as classes and *profile*.

# Typing

# Swierstra's paper

# Use exceptions for continuation control flow, e.g. handling parse
# failures.


def read_only(value):
    return property(lambda g: value)

# class Failure(Result):
#     def __init__(self, message, data, index):
#         self.message = message
#         self.data = data
#         self.index = index
#     def copy(self):
#         return Failure(self.message, self.data, self.index)
#     def __str__(self):
#         return 'Failure: %s' % (self.message % self.data[self.index:])
#     __repr__ = __str__

class GraphStructuredStack(object):
    NodeLabel = read_only(collections.namedtuple('NodeLabel', 'combinator index'))
    Node = read_only(collections.namedtuple('Node', 'edges U P'))
    Edge = read_only(collections.namedtuple('Edge', 'continuation index'))

    def __init__(self):
        self.nodes = {}
        self.dispatch_stack = []

    def create(self, called: 'Combinator', calling: 'Combinator',
               continuation, index: int):
        to_label = self.NodeLabel(called, index)
        if to_label in self.nodes:
            to_node = self.nodes[to_label]
            for right_extent in to_node.P:
                self.add(called, continuation, right_extent)
        else:
            to_node = self.Node([], set(), set())
            self.nodes[to_label] = to_node
        to_node.edges.append(self.Edge(called._parse, index))
        return to_node

    def pop(self, combinator: 'Combinator', index: int, right_extent: int):
        gss_node = self.nodes[self.NodeLabel(combinator, index)]
        if right_extent not in gss_node.P:
            gss_node.P.add(right_extent)
            for continuation, i in gss_node.edges:
                self.add(combinator, continuation, i)

    def add(self, combinator: 'Combinator', continuation, index: int):
        gss_node = self.nodes[self.NodeLabel(combinator, index)]
        L = (continuation, index)
        if L not in gss_node.U:
            self.dispatch_stack.append(L)
            gss_node.U.add(L)

    def __str__(self):
        return '\n'.join(['\nGSS', 'Stack', pprint.pformat(self.dispatch_stack), 'Nodes', pprint.pformat(self.nodes)])


# def main_loop():
#     gss = GraphStructuredStack()
#     # Initialize stuff
#     while gss.dispatch_stack:
#         # c_i = index, c_u = gss_node, c_n = sppf_node
#         L, u, i, w = gss.dispatch_stack.pop() # GrammarSlot, GraphStructuredStack.node, int, SPPFnode


# class GraphStructuredStack(object):
#     Node = read_only(collections.namedtuple('Node', 'edges U P'))

#     def __init__(self, nonterminal: 'Combinator', index: int):
#         self.nodes = collections.defaultdict(lambda: self.Node(set(), set(), set()))
#         self.dispatch_stack = [(nonterminal, index)]

#     def create(self, combinator: 'Combinator', success: 'some_function_type', failure: 'some_function_type', index: int):
#         gss_node = self.nodes[(combinator, index)]
#         if success not in gss_node.edges:
#             gss_node.edges.add(success)
#             for result in gss_node.P:
#                 success(result.value, failure, result.index)
#         return gss_node

#     def pop(self, combinator: 'Combinator', index: int, result: Result, failure: 'some_function_type'):
#         gss_node = self.nodes[(combinator, index)]
#         if result not in gss_node.P:
#             gss_node.P.add(result)
#             for success in gss_node.edges:
#                 success(result, failure, index)

#     def add(self, combinator: 'Combinator', index: int):
#         gss_node = self.nodes[(combinator, index)]
#         if index not in gss_node.U:
#             self.dispatch_stack.append((combinator, index))
#             gss_node.U.add(index)

#     def __str__(self):
#         return '\n'.join(['\nGSS', 'Stack', pprint.pformat(self.dispatch_stack), 'Nodes', pprint.pformat(self.nodes)])


class Combinator(six.with_metaclass(abc.ABCMeta, object)):
    @abc.abstractmethod
    def __init__(self, arg_type, *combinators, **kws):
        # print(self, arg_type, combinators, kws)
        if len(combinators) == 0:
            raise TypeError('%s takes at least 1 argument.' % type(self).__name__)
        if any(not isinstance(c, arg_type) for c in combinators):
            raise TypeError('Arguments to %s must be %ss.' % (type(self).__name__, arg_type.__name__))

    # TODO: disabled temporarily until I find a better solution for Lazy
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

    def parse(self, data, index=0, greedy=False):
        # self.stack = []
        # # A map of a data position to a map mapping combinator
        # # instances to sets of continuations.  In the standard GLL
        # # algorithm, this seems to correspond to the GSS nodes, though
        # # note that Spiewak seems to have already transformed the GSS
        # # like Afroozeh and Izmaylova, with nodes recorded using only
        # # the nonterminal/combinator instance and the input position.
        # # Spiewak uses function identity to avoid needing to keep
        # # track of edge labels.
        # self.backlinks = {}
        # # A map of a data position to sets of combinators.  This
        # # corresponds to U.
        # self.done = {}
        # # A map of a data position to a map mapping combinator instances
        # # to sets of successes.  This corresponds to P.
        # self.popped = {}
        # # A map of Results to sets of functions.
        # self.saved = {}

        successes = set()
        failures = set()
        def nonterminal_success(result: Result, failure, index: int):
            if isinstance(result, Result):
                successes.add(result)
            else:
                successes.add(Success(result, data, index))

        def nonterminal_failure(result, index):
            if isinstance(result, Result):
                failures.add(result)
            else:
                failures.add(Failure(result, data, index))

        gss = GraphStructuredStack(self, index) # Combinator, int
        while gss.dispatch_stack:
            print(gss)
            combinator, index = gss.dispatch_stack.pop()
            combinator._parse(gss, nonterminal_success, nonterminal_failure, data, index)

            # # This function corresponds to the pop() method in the
            # # standard GLL algorithm.
            # def trampoline_success(tree, failure, current_index, past_index=index, combinator=combinator):
            #     result = Success(tree, data, current_index)
            #     # print('Trampoline success: ', tree, pprint.pformat(self.backlinks), pprint.pformat(self.saved), sep='\n')
            #     # print('Trampoline success:', pprint.pformat(self.popped), combinator, data, sep='\n')
            #     if past_index not in self.popped:
            #         self.popped[past_index] = {}
            #     if combinator not in self.popped[past_index]:
            #         self.popped[past_index][combinator] = set()
            #     self.popped[past_index][combinator].add(result)

            #     # The saved set is not part of the original algorithm,
            #     # Spiewak added it.  He's using result identity here
            #     # to check if something's been done, but there has to
            #     # be a better way.
            #     if result not in self.saved:
            #         self.saved[result] = set()
            #     for success in self.backlinks[past_index][combinator]:
            #         if success not in self.saved[result]:
            #             self.saved[result].add(success)
            #             # print(success, tree, failure, current_index)
            #             success(tree, failure, current_index)

        if successes:
            if greedy and index < len(data):
                return Failure('Unexpected trailing characters: %r', data, index)
            else:
                return list(successes)
        else:
            return list(failures)

    # def add(self, combinator, success, failure, index):
    #     # print('Add:', self)
    #     if index not in self.backlinks:
    #         self.backlinks[index] = {}
    #     if combinator not in self.backlinks[index]:
    #         self.backlinks[index][combinator] = set()
    #     if success not in self.backlinks[index][combinator]:
    #         self.backlinks[index][combinator].add(success)
    #     if index in self.popped and combinator in self.popped[index]:
    #         for result in self.popped[index][combinator].copy():
    #             success(result.value, failure, result.index)
    #     else:
    #         if index not in self.done:
    #             self.done[index] = set()
    #         if combinator not in self.done[index]:
    #             self.stack.append((combinator, index))
    #             self.done[index].add(combinator)

    @abc.abstractmethod
    def _parse(self, sppf, gss, success, failure, data, index):
        raise NotImplementedError

    # def unparse(self, tree, data, index=0):
    #     pass

    # def _unparse(self, tree, data, index):
    #     raise NotImplementedError
    

class Alternation(Combinator):
    def __init__(self, *combinators, **kws):
        super(Alternation, self).__init__(Combinator, *combinators, **kws)
        vars(self)['combinators'] = frozenset(combinators)

    def _parse(self, sppf, gss, success, failure, data, index):
        for combinator in self.combinators:
            gss.add(combinator, index)

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


class Sequence(Combinator):
    def __init__(self, *combinators, **kws):
        super(Sequence, self).__init__(Combinator, *combinators, **kws)
        vars(self)['combinators'] = combinators

    def _parse(self, sppf, gss, success, failure, data, index):
        trees = []
        def continuation_factory(slot: int):
            if slot == len(self.combinators):
                def sequence_end(tree, failure, index):
                    trees.append(tree)
                    result = Success(tuple(trees), data, index)
                    gss.pop(self, index, result, failure)
                    success(result, failure, index)
                return sequence_end
            else:
                def sequence_continuation(tree, failure, index):
                    trees.append(tree)
                    self.combinators[slot]._parse(gss, continuation_factory(slot + 1), failure, data, index)
                return sequence_continuation
        continuation = continuation_factory(1)
        gss.create(self, continuation, failure, index)
        self.combinators[0]._parse(gss, continuation, failure, data, index)

    def __add__(self, other):
        if isinstance(other, Sequence):
            return Sequence(*(self.combinators + other.combinators))
        else:
            return Sequence(*(self.combinators + (other,)))
    def __radd__(self, other):
        if isinstance(other, Sequence):
            return Sequence(*(other.combinators + self.combinators))
        else:
            return Sequence(*((other,) + self.combinators))
    def __mul__(self, other):
        type(self)(*(other * self.combinators))
    __rmul__ = __mul__


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

    def _parse(self, sppf, gss, success, failure, data, index):
        combinator = self.combinator
        self._parse = combinator._parse
        combinator._parse(gss, success, failure, data, index)


class Action(Combinator):
    def __init__(self, combinator, action):
        self.combinator = combinator
        self.action = action

    def _parse(self, sppf, gss, success, failure, data, index):
        def action_continuation(tree, failure, index):
            success(self.action(tree), failure, index)
        self.combinator._parse(gss, action_continuation, failure, data, index)


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

    def parse(self, data, index=0, greedy=False):
        result = None
        def terminal_success(tree, failure, index):
            nonlocal result
            result = Success(tree, data, index)
        def terminal_failure(message, index):
            nonlocal result
            result = Failure(message, data, index)
        self._parse(GraphStructuredStack(self, index), terminal_success, terminal_failure, data, index)
        return result

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

    def _parse(self, sppf, gss, success, failure, data, index):
        trees = []
        for string, length in self.strings_lengths:
            if (length > len(data) - index):
                return failure('Unexpected end of data (expected %r, got %%r)' % string, index)
            else:
                if data.startswith(string, index):
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

    def _parse(self, sppf, gss, success, failure, data, index):
        match = self.regex.match(data, index)
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
    structs = read_only(Cache(struct.Struct))

    def __init__(self, format_string, **kws):
        vars(self)['struct'] = self.structs[format_string]
        vars(self)['combinators'] = (self,)

    def _parse(self, sppf, gss, success, failure, data, index):
        try:
            return success(self.struct.unpack_from(data, index), index + self.struct.size)
        except struct.error as error:
            return failure(error.args[0] + 'at %r', index)


# if __name__ == '__main__':
#     import cProfile
#     import platform
#     import sys
#     import time
#     import timeit

#     CPYTHON = True if platform.python_implementation() == 'CPython' else False
#     if six.PY3 and CPYTHON:
#         import tracemalloc
#         tracemalloc.start()

#     import trace_calls

#     trace_calls = trace_calls.TraceCalls(files=('continuation_gll_combinators.py',))

#     # The implementation in Spiewak's paper doesn't seem to be
#     # complete because the only parser that will ever return
#     # "Unexpected trailing characters" is a non-terminal parser.
#     strings = Strings('ab')
#     print('Strings success,', strings.parse('ababab'))
#     print('Strings failure,', strings.parse('bcbcbc'))

#     terminal = Strings('a') + Strings('b')
#     print('Terminal success,', terminal.parse('ababab'))
#     terminal = Terminal(Strings('a'), Strings('b'))
#     print('Terminal success,', terminal.parse('ababab'))

#     sequence = Sequence(Strings('abc'), Strings('def')) + Strings('ghi') + Strings('jkl')
#     print('Sequence success,', sequence.parse('abcdefghijkl'))
#     sequence = Strings('abc') + Strings('def') + Strings('ghi') + Strings('jkl')
#     print('Sequence success,', sequence.parse('abcdefghijkl'))
#     sequence = Sequence(Strings('abc'), Strings('def'), Strings('ghi'), Strings('jkl'))
#     print('Sequence success,', sequence.parse('abcdefghijkl'))    

#     alternation = Strings('ab') | Strings('bc')
#     print('Alternation success,', alternation.parse('bcbcbc'))
#     alternation = Alternation(Strings('ab'), Strings('bc'))
#     print('Alternation success,', alternation.parse('bcbcbc'))

#     sequence_alternation = Strings('a') + (Strings('b') | Strings('c'))
#     print('Sequence alternation success,', sequence_alternation.parse('abc'))
#     print('Sequence alternation success,', sequence_alternation.parse('acb'))
#     print('Sequence alternation failure,', sequence_alternation.parse('cba'))
#     sequence_alternation = Sequence(Strings('a'), Alternation(Strings('b'), Strings('c')))
#     print('Sequence alternation success,', sequence_alternation.parse('abc'))
#     print('Sequence alternation success,', sequence_alternation.parse('acb'))
#     print('Sequence alternation failure,', sequence_alternation.parse('cba'))

#     alpha = Regex('([a-zA-Z])')
#     hex_char = Regex('([a-fA-F0-9])')

#     alpha_or_hex = alpha | hex_char
#     print('Alpha success,', alpha_or_hex.parse('xyz'))
#     print('Alpha and hex success,', alpha_or_hex.parse('ABC'))
#     print('Hex success,', alpha_or_hex.parse('123'))

#     a = Strings('a')
#     l = Lazy('a')
#     print('Lazy,', l.parse('a'))
#     print('Lazy,', l.parse('a'))

#     sys.settrace(trace_calls)

#     # Afroozeh/Izmaylova example
#     A = (Strings('a') + Lazy('A') + (Strings('b')) | (Strings('a') + Lazy('A') + Strings('c')) | Strings('a')
#     print('Afroozeh/Izmaylova,', A.parse('aac'))

#     ambiguous = (Lazy('ambiguous') + Lazy('ambiguous') + Lazy('ambiguous')) | (Lazy('ambiguous') + Lazy('ambiguous')) | Strings('a')
#     print('Highly ambiguous,', ambiguous.parse('aaa'))
#     # pprint.pprint(ambiguous.combinators)

#     # There's a major divergence from Koopman and Plasmeijer here
#     # because regexes behave like their deterministic combinators, but
#     # deterministic behavior at the word level here is probably more
#     # realistic for profiling.
#     word = Regex('([a-zA-Z]+)')
#     sentence = ((word + Strings('.')) >> (lambda t: t.value[0])) | ((word + Regex(r'[\s,]') + Lazy('sentence')) >> (lambda t: t.value[0][0] + t.value[1]))
#     print('Sentence success,', sentence.parse('The quick brown fox jumps over the lazy dog.'))

#     num = (Strings('0') | Strings('1')) >> (lambda t: int(t[0]))
#     print('Calculator,', num.parse('010101'))
#     print('Calculator,', num.parse('101010'))

#     expr = (num + Strings('+') + Lazy('expr')) >> (lambda t: t[0] + t[2]) | (num + Strings('-') + Lazy('expr')) >> (lambda t: t[0] - t[2]) | num

#     print('Calculator,', expr.parse('1+1'))
#     print('Calculator,', expr.parse('1+1+1'))
#     print('Calculator,', expr.parse('0+1-1+1+1'))
#     print('Calculator,', expr.parse('1+1+1+1+1'))
#     print('Calculator,', expr.parse('0-1-1-1-1'))
#     print('Calculator,', expr.parse('1-1-2'))
#     print('Calculator,', expr.parse('3'))

#     dictionary = [w.strip().replace("'", '') for w in open('/usr/share/dict/words', 'r').read().splitlines() if w.strip().isalpha()]
#     sample = ' '.join(dictionary[:10000]) + '.'

#     start = time.clock()
#     sentence.parse(sample)
#     end = time.clock()
#     print('Dictionary, %.4f seconds' % (end - start,))

#     print('Highly ambiguous')
#     def time_ambiguous(max_length):
#         for i in range(2, max_length):
#             print(i, timeit.timeit('ambiguous.parse("' + i * 'a' + '")', 'gc.enable(); from __main__ import ambiguous', number=1000), 'seconds')

#     cProfile.run('''
# time_ambiguous(9)
# ''')

#     if six.PY3 and CPYTHON:
#         snapshot = tracemalloc.take_snapshot()
#         top_stats = snapshot.statistics('lineno')

#         print("[ Top 10 ]")
#         for stat in top_stats[:10]:
#             print(stat)

# print('Stack depth:', trace_calls.max_depth)
