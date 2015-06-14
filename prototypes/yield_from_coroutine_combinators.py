#!/usr/bin/python3

import abc
import collections
import inspect
import itertools
import re
import struct
import types


class Cache(dict):
    def __init__(self, factory, *args, **kws):
        self.factory = factory
        super().__init__(*args, **kws)
    @classmethod
    def fromkeys(cls, factory, seq, value=None):
        instance = super().fromkeys(seq, value)
        instance.factory = factory
        return instance
    def __missing__(self, key):
        return self.factory(key)


class Success(collections.namedtuple('Result', 'tree stream offset')):
    __slots__ = ()
    def __str__(self):
        return "Success: {}, {}".format(self.tree, self.stream[self.offset:])
    __repr__ = __str__

class Failure(collections.namedtuple('Result', 'msg tree stream offset')):
    __slots__ = ()
    def __str__(self):
        return "Failure: {}, {}".format(self.tree, self.msg.format(self.stream[self.offset:]))
    __repr__ = __str__
    def __bool__(self):
        return False

DEFAULT_FAILURE = Failure('This is the default failure for combinators that cannot generate their own failures.  It should never be returned.', (), '',  -1)


class Combinator(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self):
        raise NotImplementedError

    def parse(self, stream, offset=0):
        result = self._parse(stream, offset)
        if isinstance(result, types.GeneratorType):
            try:
                next(self._parse(stream, offset))
            except StopIteration as result:
                return result.args[0]
        else:
            return result

    @abc.abstractmethod
    def _parse(self, stream, offset):
        raise NotImplementedError

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
        self.combinators = combinators
        self.name = name

    def _parse(self, stream, offset):
        successes = []
        failure = DEFAULT_FAILURE
        for combinator in self.combinators:
            result = combinator._parse(stream, offset)
            if isinstance(result, types.GeneratorType):
                result = (yield from result)
            if result:
                successes.extend(result)
            else:
                if failure.offset < result.offset:
                    failure = result
        if successes:
            return successes
        else:
            return failure

    def __or__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(self.combinators + other.combinators))
        else:
            return Alternation(*(self.combinators + (other,)))
    def __ror__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(other.combinators + self.combinators))
        else:
            return Alternation(*((other,) + self.combinators))


class Sequence(Combinator):
    def __init__(self, *combinators, name=None):
        self.combinators = combinators
        self.name = name

    def _parse(self, stream, offset):
        failure = DEFAULT_FAILURE
        offsets = [offset]
        paths = [[]]
        for combinator in self.combinators:
            for o in offsets:
                result = combinator._parse(stream, o)
                if isinstance(result, types.GeneratorType):
                    result = (yield from result)
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
                            failure = Failure(result.msg, (), stream,
                                              result.offset)
                        else:
                            failure = Failure(result.msg,
                                              tuple(s.tree for s in path),
                                              stream, result.offset)
            if not new_paths:
                return failure
            paths = new_paths
            offsets = [p[-1].offset for p in paths]
        return [Success(tuple(s.tree for s in p), stream, p[-1].offset) for p in paths]

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


class Terminal(Combinator):
    def parse(self, stream, offset=0):
        return self._parse(stream, offset)


class Strings(Terminal):
    def __init__(self, *strings, name=None):
        self.strings = strings
        self.offsets = (0,) + tuple(itertools.accumulate(map(len, strings)))
        self.name = name

    def _parse(self, stream, offset):
        # Fundamental problem here in Python <3.5, bytes interpolation
        # doesn't work.  I'm not sure how to fix that immediateley.
        for i, string in enumerate(self.strings):
            if (len(string) > len(stream) - offset):
                return Failure("Unexpected end of stream (expected '%s')" % 
                               ' '.join(self.strings[i:]),
                               tuple(self.strings[:i]), 
                               stream, 
                               offset + self.offsets[i])
            else:
                if stream.startswith(string, offset):
                    offset += len(string)
                else:
                    return Failure("Expected '%s' got '{}'" %
                                   ' '.join(self.strings[i:]),
                                   tuple(self.strings[:i]),
                                   stream,
                                   offset + self.offsets[i])
        return [Success(self.strings, stream, offset)]


class Regex(Terminal):
    import re
    regexes = Cache(re.compile)

    def __init__(self, pattern, name=None):
        self.regex = self.regexes[pattern]
        self.name = name

    def _parse(self, stream, offset):
        match = self.regex.match(stream, offset)
        if match:
            return [Success(match.groups(), stream, match.end())]
        else:
            return Failure("'%s' didn't match '{}'" % self.regex.pattern,
                           (), stream, offset)


class Binary(Terminal):
    import struct
    structs = Cache(struct.Struct)

    def __init__(self, format_string, name=None):
        self.struct = self.structs[format_string]
        self.name = name

    def _parse(self, stream, offset):
        try:
            [Success(self.struct.unpack_from(stream, offset),
                    stream, offset + self.struct.size)]
        except struct.error as error:
            return Failure(error.args[0], stream, offset)


class Lazy(Combinator):
    def __init__(self, name):
        if name.isidentifier():
            self.name = name
        else:
            raise SyntaxError("Lazy initialized with a string that isn't a valid Python identifier: %s" % name)

    def _parse(self, stream, offset):
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
            self._parse = combinator._parse
            result = combinator._parse(stream, offset)
            if isinstance(result, types.GeneratorType):
                return (yield from result)
            else:
                return result
        else:
            raise TypeError("'%s' refers to an object that is a %s instead of a combinator." % (self.name, type(combinator)))


class Action(Combinator):
    def __init__(self, combinator, action, name=None):
        self.combinator = combinator
        self.action = action
        self.name = name

    def _parse(self, stream, offset):
        result = self.combinator._parse(stream, offset)
        if isinstance(result, types.GeneratorType):
            result = (yield from result)
        if result:
            return [Success(self.action(s.tree), stream, s.offset)
                    for s in result]
        else:
            return result


if __name__ == '__main__':
    terminal = Strings('01')
    print('Strings success,', terminal.parse('010101'))
    print('Strings failure,', terminal.parse('121212'))
    terminal = Sequence(Strings('0'), Strings('1'))
    print('Sequence terminal success,', terminal.parse('010101'))
    alternation = Alternation(Strings('01'), Strings('12'))
    print('Alternation success,', alternation.parse('121212'))
    terminal = Strings('0') + Strings('1')
    print('Add success,', terminal.parse('010101'))
    alternation = Strings('01') | Strings('12')
    print('Or success,', alternation.parse('121212'))
    sequence = Sequence(Strings('0'), Alternation(Strings('1'),
                                                    Strings('2')))
    print('Sequence alternation success,', sequence.parse('012'))
    print('Sequence alternation failure,', sequence.parse('032'))

    num = (Strings('0') | Strings('1')) >> (lambda t: int(t[0]))
    print('Calculator,', num.parse('010101'))
    print('Calculator,', num.parse('101010'))

    expr = (num + Strings('+') + Lazy('expr')) >> (lambda x: x[2] + x[0]) | (num + Strings('-') + Lazy('expr')) >> (lambda x: x[2] - x[0]) | num

    print('Calculator,', expr.parse('1+1'))
    print('Calculator,', expr.parse('1+1+1'))
    print('Calculator,', expr.parse('0+1-1+1+1'))
    print('Calculator,', expr.parse('1+1+1+1+1'))
    print('Calculator,', expr.parse('0-1-1-1-1'))
    print('Calculator,', expr.parse('1-1-2'))
    print('Calculator,', expr.parse('3'))

    t = Strings('a')
    l = Lazy('t')
    print('Lazy,', l.parse('a', 0))
    print('Lazy,', l.parse('a', 0))

    ambiguous = (Strings('a') + Lazy('ambiguous')) | (Strings('aa') + Lazy('ambiguous')) | Strings('')

    print('Ambiguous,', ambiguous.parse('aaa'))
