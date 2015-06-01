#!/usr/bin/python3

from __future__ import absolute_import, division, print_function

import abc
import inspect
import re
import struct

import six


# TODO: properly segregate the GSS in its own data structure

# rewrite this with the faster GSS modifications.

# SPPF

# Replace copying of stream data with indices

# Rewrite the continuations as classes and *profile*.

# Typing

# Swierstra's paper


class Cache(dict):
    def __init__(self, factory, *args, **kws):
        self.factory = factory
        super(Cache, self).__init__(*args, **kws)
    @classmethod
    def fromkeys(cls, factory, seq, value=None):
        instance = super(Cache, self).fromkeys(seq, value)
        instance.factory = factory
        return instance
    def __missing__(self, key):
        return self.factory(key)


def read_only(value):
    return property(lambda g: value)


# Success = collections.namedtuple('Success', 'node label offsets')

# Failure = collections.namedtuple('Failure', 'msg offset')

# Label = collections.namedtuple('Label', 'combinator start end')

# DEFAULT_FAILURE = Failure('This is the default failure for combinators that cannot generate their own failures.  It should never be returned.', -1)


class Result:
    pass
class Success(Result):
    def __init__(self, value, tail):
        self.value = value
        self.tail = tail
    def copy(self):
        return Success(self.value, self.tail)
    def __str__(self):
      return 'Success: ' + str(self.value) + ", '" + str(self.tail) + "'"
    __repr__ = __str__

class Failure(Result):
    def __init__(self, msg):
        self.msg = msg
    def copy(self):
        return Failure(self.msg)
    def __str__(self):
        return 'Failure: ' + self.msg
    __repr__ = __str__


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

    def parse(self, stream):
        successes = set()
        failures = set()
        # @tracecalls.TraceCalls(show_ret=True)
        def nonterminal_continuation(result):
            if isinstance(result, Success):
                # if result.tail:
                #     failures.add(Failure('Unexpected trailing characters: "{}"'.format(str(result.tail))))
                # else:
                successes.add(result)
            else:
                failures.add(result)

        self.stack = []
        # A map of a stream position to a map mapping combinator instances
        # to sets of continuations.
        self.backlinks = {}
        # A map of a stream position to sets of combinators.
        self.done = {}
        # A map of a stream position to a map mapping combinator instances
        # to sets of successes.
        self.popped = {}
        # A map of Results to sets of functions.
        self.saved = {}

        self._parse(self, stream, nonterminal_continuation)

        while self.stack:
            # logging.debug('Trampoline run() looped.')
            # print(self.stack)
            combinator, stream = self.stack.pop()
            def continuation_factory(combinator, stream):
                # @tracecalls.TraceCalls(show_ret=True)
                def trampoline_continuation(result):
                    if stream not in self.popped:
                        self.popped[stream] = {}
                    if combinator not in self.popped[stream]:
                        self.popped[stream][combinator] = set()
                    if isinstance(result, Success):
                        self.popped[stream][combinator].add(result)
                    if result not in self.saved:
                        self.saved[result] = set()
                    for f in self.backlinks[stream][combinator]:
                        if f.__code__ not in {i.__code__ for i in self.saved[result]}:
                            self.saved[result].add(f)
                            f(result)
                return trampoline_continuation
            combinator._parse(self, stream, continuation_factory(combinator, stream))

        if successes:
            return list(successes)
        else:
            return list(failures)

    def add(self, combinator, stream, continuation):
        # logging.debug('Trampoline add() called:\n    Parser: {}\n    Stream: {}\n    Continuation: {}'.format(parser, stream, continuation))
        if stream not in self.backlinks:
            self.backlinks[stream] = {}
        if combinator not in self.backlinks[stream]:
            self.backlinks[stream][combinator] = set()
        if continuation.__code__ not in {i.__code__ for i in self.backlinks[stream][combinator]}:
            self.backlinks[stream][combinator].add(continuation)
        if stream in self.popped and combinator in self.popped[stream]:
            for result in self.popped[stream][combinator]:
                continuation(result)
        else:
            if stream not in self.done:
                self.done[stream] = set()
            if combinator not in self.done[stream]:
                self.stack.append((combinator, stream))
                self.done[stream].add(combinator)

    @abc.abstractmethod
    def _parse(self, trampoline, stream, continuation):
        raise NotImplementedError

    def unparse(self, tree, stream, offset=0):
        pass

    def _unparse(self, tree, stream, offset):
        raise NotImplementedError


class Alternation(Combinator):
    def __init__(self, *combinators, **kws):
        super(Alternation, self).__init__(Combinator, *combinators, **kws)
        vars(self)['combinators'] = frozenset(combinators)

    def _parse(self, trampoline, stream, continuation):
        results = set()
        def continuation_factory(continuation):
            # @tracecalls.TraceCalls(show_ret=True)
            def alternation_continuation(result):
                if result not in results:
                    continuation(result)
                    results.add(result)
            return alternation_continuation
        for combinator in self.combinators:
            trampoline.add(combinator, stream, continuation_factory(continuation))

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

    def _parse(self, trampoline, stream, continuation):
        results = []
        iterator = iter(self.combinators)
        # logging.debug(chain_log.format(type(self).__name__, str(t), stream, continuation))
        def continuation_factory(continuation):
            # The clean way to do is with a separate index variable,
            # but Python 2.7 doesn't allow an inner function to alter
            # the variable of an outer one.  The proper way of working
            # around this is probably using classes instead of
            # closures because classes have mutable state.
            # @tracecalls.TraceCalls(show_ret=True)
            def sequence_continuation(result):
                if isinstance(result, Success):
                    results.append(result)
                    try:
                        combinator = next(iterator)
                    except StopIteration:
                        return continuation(Success([r.value for r in results], results[-1].tail))
                    combinator._parse(trampoline, result.tail, sequence_continuation)
                else:
                    continuation(result)
            return sequence_continuation
        next(iterator)._parse(trampoline, stream, continuation_factory(continuation))

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

    def _parse(self, trampoline, stream, continuation):
        combinator = self.combinator
        self._parse = combinator._parse
        return combinator._parse(trampoline, stream, continuation)


class Action(Combinator):
    def __init__(self, combinator, action):
        self.combinator = combinator
        self.action = action

    def _parse(self, trampoline, stream, continuation):
        def continuation_factory(continuation):
            def action_continuation(result):
                # print(result)
                if isinstance(result, Success):
                    return continuation(Success(self.action(result.value), result.tail))
                else:
                    return continuation(result)
            return action_continuation
        self.combinator._parse(trampoline, stream, continuation_factory(continuation))


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
        
    def parse(self, stream):
        # logging.debug(apply_log.format(type(self).__name__, stream))
        values = []
        for combinator in self.combinators:
            result = combinator.parse(stream)
            if isinstance(result, Success):
                values.append(result.value)
                stream = result.tail
            else:
                return result.copy()
        return Success(values, result.tail)

    def _parse(self, trampoline, stream, continuation):
        # logging.debug(chain_log.format(type(self).__name__, str(t), stream, f))
        continuation(self.parse(stream))

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

    def parse(self, stream):
        values = []
        for string, length in self.strings_lengths:
            if (length > len(stream)):
                return Failure("Unexpected end of stream (expected '%s')" % string)
            else:
                if stream.startswith(string):
                    values.append(string)
                else:
                    return Failure("Expected '%s' got '%s'" % (string, stream))
        return Success(values, stream[sum(i[1] for i in self.strings_lengths):])

    def __str__(self):
        return 'Strings(' + str(self.strings_lengths) + ')'
    __repr__ = __str__

    
class Regex(Terminal):
    regexes = read_only(Cache(re.compile))

    def __init__(self, pattern, **kws):
        vars(self)['regex'] = self.regexes[pattern]

    def parse(self, stream):
        match = self.regex.match(stream)
        if match:
            # This API is kind of ugly, a regex always needs at least
            # one group to return a tree element correctly.
            return Success(match.groups(), stream[match.end():])
        else:
            return Failure("'%s' didn't match '{}'" % self.regex.pattern)


class Binary(Terminal):
    structs = read_only(struct.Struct)

    def __init__(self, format_string, **kws):
        vars(self)['struct'] = self.structs[format_string]

    def parse(self, stream):
        try:
            return Success(self.struct.unpack_from(stream),
                           stream[self.struct.size:])
        except struct.error as error:
            return Failure(error.args[0], stream)


if __name__ == '__main__':
    import cProfile
    import pprint
    import time
    import timeit

    import platform
    CPYTHON = True if platform.python_implementation() == 'CPython' else False
    if six.PY3 and CPYTHON:
        import tracemalloc
        tracemalloc.start()

    # The implementation in Spiewak's paper doesn't seem to be
    # complete because the only parser that will ever return
    # "Unexpected trailing characters" is a non-terminal parser.
    string = Strings('ab')
    print('Strings success,', string.parse('ababab'))
    print('Strings failure,', string.parse('bcbcbc'))
    terminal = Strings('a') + Strings('b')
    print('Terminal success,', terminal.parse('ababab'))
    sequence = Terminal(Strings('a'), Strings('b'))
    print('Terminal success,', terminal.parse('ababab'))

    alternation = Strings('ab') | Strings('bc')
    print('Alternation success,', alternation.parse('bcbcbc'))
    alternation = Alternation(Strings('ab'), Strings('bc'))
    print('Alternation success,', alternation.parse('bcbcbc'))

    sequence = Strings('a') + (Strings('b') | Strings('c'))
    print('Sequence alternation success,', sequence.parse('abc'))
    print('Sequence alternation success,', sequence.parse('acb'))
    print('Sequence alternation failure,', sequence.parse('cba'))
    sequence = Sequence(Strings('a'), Alternation(Strings('b'), Strings('c')))
    print('Sequence alternation success,', sequence.parse('abc'))
    print('Sequence alternation success,', sequence.parse('acb'))
    print('Sequence alternation failure,', sequence.parse('cba'))

    alpha = Regex('([a-zA-Z])')
    hex_char = Regex('([a-fA-F0-9])')

    alpha_or_hex = alpha | hex_char
    print('Alpha success,', alpha_or_hex.parse('xyz'))
    print('Alpha and hex success,', alpha_or_hex.parse('ABC'))
    print('Hex success,', alpha_or_hex.parse('123'))

    # There's a major divergence from Koopman and Plasmeijer here
    # because regexes behave like their deterministic combinators, but
    # deterministic behavior at the word level here is probably more
    # realistic for profiling.
    word = Regex('([a-zA-Z]+)')
    sentence = ((word + Strings('.')) >> (lambda t: t[0])) | ((word + Regex('[\s,]') + Lazy('sentence')) >> (lambda t: t[0][0] + t[1]))
    print('Sentence success,', sentence.parse('The quick brown fox jumps over the lazy dog.'))

    a = Strings('a')
    l = Lazy('a')
    print('Lazy,', l.parse('a'))
    print('Lazy,', l.parse('a'))

    ambiguous = (Lazy('ambiguous') + Lazy('ambiguous') + Lazy('ambiguous')) | (Lazy('ambiguous') + Lazy('ambiguous')) | Strings('a')
    print('Highly ambiguous,', ambiguous.parse('aaa'))
    # pprint.pprint(ambiguous.combinators)

    num = (Strings('0') | Strings('1')) >> (lambda t: int(t[0]))
    print('Calculator,', num.parse('010101'))
    print('Calculator,', num.parse('101010'))

    expr = (num + Strings('+') + Lazy('expr')) >> (lambda t: t[2] + t[0]) | (num + Strings('-') + Lazy('expr')) >> (lambda t: t[2] - t[0]) | num

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

    def time_ambiguous(max_length):
        for i in range(2, max_length):
            print(i, timeit.timeit('ambiguous.parse("' + i * 'a' + '")', 'gc.enable(); from __main__ import ambiguous', number=1000))

    cProfile.run('''
time_ambiguous(9)
''')

    if six.PY3 and CPYTHON:
        snapshot = tracemalloc.take_snapshot()
        top_stats = snapshot.statistics('lineno')

        print("[ Top 10 ]")
        for stat in top_stats[:10]:
            print(stat)
