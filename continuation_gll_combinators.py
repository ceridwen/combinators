#!/usr/bin/python3

from __future__ import absolute_import, division, print_function

import abc
import inspect
import numbers
import re
import struct
import sys

import six


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
        if len(combinators) == 0:
            raise TypeError('%s takes at least 1 argument.' % self.__name__)
        if any(not isinstance(c, arg_type) for c in combinators):
            raise TypeError('Arguments to %s must be %ss.' % (self.__name__, arg_type.__name__))

    def __setattr__(self, key, value):
        raise AttributeError('%s object does not support item assignment.' % self.__name__)
    def __delattr__(self, key):
        raise AttributeError('%s object does not support item deletion.' % self.__name__)

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

    def parse(self, stream, offset=0):
        pass

    @abc.abstractmethod
    def _parse(self, stream, sppf, offset):
        raise NotImplementedError

    def unparse(self, tree, stream, offset=0):
        pass

    def _unparse(self, tree, stream, offset):
        raise NotImplementedError


class Alternation(Combinator):
    combinators = read_only_instance('combinators')

    def __init__(self, *combinators, **kws):
        super(Alternation, self).__init__(arg_type=Combinator, *combinators, **kws)
        vars(self)['combinators'] = frozenset(*combinators)

    def _parse(self, stream, offset):
        pass

    def __or__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(self.combinators | other.combinators))
        else:
            return Alternation(*(self.combinators | frozenset(other)))
    def __ror__(self, other):
        if isinstance(other, Alternation):
            return Alternation(*(other.combinators | self.combinators))
        else:
            return Alternation(*(frozenset(other) | self.combinators))


class Sequence(Combinator):
    combinators = read_only_instance('combinators')

    def __init__(self, *combinators, **kws):
        super(Sequence, self).__init__(arg_type=Combinator, *combinators, **kws)
        vars(self)['combinators'] = combinators

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
        if isinstance(other, (int, numbers.Integer)):
            type(self)(*(other * self.combinators))
        else:
            return NotImplemented
    __rmul__ = __mul__


class Terminal(Sequence):
    """This inherits from Sequence: note that there's no real distinction
    between a sequence of terminals and a single terminal.

    Parsers have a type of input they expect, and you can't mix
    parsers that accept different kinds of input.  Note this is
    different from passing output to a subparser using Act/>>.

    """
    def __init__(self, *combinators, **kws):
        # TODO: should rework this to not hard-code Combinator
        Combinator.__init__(arg_type=Terminal, *combinators, **kws)
        vars(self)['combinators'] = combinators

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

    def _parse(self, stream, sppf, offset):
        pass


class Strings(Terminal):
    def __init__(self, *strings, **kws):
        vars(self)['strings_lengths'] = [(s, len(s)) for s in strings]

    def _parse(self, stream, offset):
        start = offset
        strings = []
        for string, length in self.strings_lengths:
            if (offset + length > len(stream)):
                return Failure('Unexpected end of stream (expected "%s")' % string, start)
            else:
                if stream.startswith(string, offset):
                    strings.append(string)
                    offset += length
                else:
                    return Failure('Expected "%s" got "{}"' % string, start)
        return (strings, offset)
    

class Regex(Terminal):
    regexes = read_only(Cache(re.compile))

    def __init__(self, pattern, **kws):
        vars(self)['regex'] = self.regexes[pattern]

    def _parse(self, stream, offset):
        match = self.regex.match(stream, offset)
        if match:
            return [Success(match.groups(), stream, match.end())]
        else:
            return Failure("'%s' didn't match '{}'" % self.regex.pattern,
                           (), stream, offset)


class Binary(Terminal):
    structs = read_only(struct.Struct)

    def __init__(self, format_string, **kws):
        vars(self)['struct'] = self.structs[format_string]

    def _parse(self, stream, offset):
        try:
            [Success(self.struct.unpack_from(stream, offset),
                    stream, offset + self.struct.size)]
        except struct.error as error:
            return Failure(error.args[0], stream, offset)


class Lazy(Combinator):
    def __init__(self, name):
        if name.isidentifier():
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

    def _parse(self, stream, offset):
        combinator = self.combinator
        self._parse = combinator._parse
        return combinator._parse(stream, offset)


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

