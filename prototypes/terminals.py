#!/usr/bin/python3

from __future__ import print_function

import collections
import numbers
import itertools
# import functools
import timeit

# Will probably limit support to 2.7 and 3.4, but for the moment 2.6
# and 3.3 should work.

# Single-dispatch generic functions are in, one of the CPython
# developers maintains a backport for Python 2.6.5 onwards to 3.3.
# These are really too useful for this project to avoid.  The enhanced
# buffer protocol, which I'll also need, is in 2.6 on.  Coroutines
# were added in Python 2.5.  New-style string formatting, the abstract
# classes (both abc and collections.abc), and built-in sets were also
# added in 2.6.  The u'' prefix for Unicode strings came back in 3.3
# to make writing compatible code easier.  There's better iterable
# unpacking syntax in 3.0: http://legacy.python.org/dev/peps/pep-3132/
# .

# Big question: 'yield from' was added in Python 3.3.  Experimentation
# suggests that as of 3.4, using "yield from" still hits the maximum
# recursion depth which means that "yield from" still adds a stack
# frame for each call.


# 2.7 support hacks
import six

if six.PY3:
    import teststruct as _struct
    three_bytes_available = True
else:
    # Hack for having not hacked a 2.7 version of struct yet.
    import struct as _struct
    three_bytes_available = False

try:
    from functools import singledispatch
except ImportError:
    from singledispatch import singledispatch

# 3.x vs. 2.7 syntax difference with metaclass assignment here.
# metaclass=ABCMeta
# import functools
# import abc


# In Jensen-Gray's terminology, where I'm starting is an
# internal/embedded DSL that's an API, possibly with embedded data.



# Features struct needs to have and doesn't:

# Three-byte integers

# Arbitrary-length integers

# Bits

# A function that returns the length of the tuple returned by unpack
# or the arguments required by pack.


# The bird's-eye view of the trivial compiler design is that it should
# break up the problem into a set of struct module instances that
# cover all possible reads of the data that will be necessary during
# the execution of the packer/unpacker.  Note that some format strings
# may be duplicated in multiple places in the AST of the declarative
# mini-language: for instance, in my DVI parser, there will be a
# struct string 'b' for an opcode, but the same struct string appears
# in one-parameter mixed-sign opcode quadruplets.  There's no reason
# to make extra instances for these duplicates, so the right data
# structure for storing the struct instances is a dictionary keyed by
# the format strings.  Unfortunately, format strings don't have unique
# semantics (it's possible to express the same format with different
# strings), so the trivial compiler will have to enforce this
# invariant itself.  I *believe* that this can only happen with count
# values and cases where endianness gets broken up differently
# (['>hb', '<h'] and ['>h', '<bh'] would be stored differently in the
# dictionary but have identical packing/upacking properties in toto).
# One other piece of information has to be stored in this dictionary,
# the length of the tuple produced by the struct instance.  The rules
# on which format characters I'm allowing prohibit struct strings with
# dynamic lengths, so every struct instance has an integer length that
# needs to be stored with it.

# There are four cases that have to be handled during compilation:

# Atoms: atoms always fit together in a single string except for endianness.

# Static combinators: each instance of an entirely static combinator
# will carry a list of format strings created when *it's* compiled.
# To put the two lists together, the trivial compiler has to check if
# the beginning of the second and the end of the first can be stuck
# together---if they can, great, do so and concatenate the lists, if
# not, just concatenate the lists.  Note that after this happens, the
# trivial compiler can delete some attributes on the child static
# combinator.  This may be better implemented by a visitor pattern, I
# will have to examine that.

# Dynamic combinators: dynamic combinators have format strings, but
# these can never be merged with static format strings, so they have
# to be just added to the dictionary.

# Static-dynamic combinators: these are static combinators with dynamic
# children.  The dynamic children need to be added to the list to say,
# "Don't call struct, call me," and the static parts of the lists need
# to be concatenated in the usual way.


# Some subtleties that I need to work out:

# For static combinators, it's possible to precalculate all the byte
# offsets in advance.  This saves some additions in the inner loop
# while parsing.  For dynamic combinators, I want to replace this with
# some kind of generator (coroutine?) that runs during parsing and
# spits out byte offsets.  Actually, once the struct strings are
# built, this entire problem can be conceptualized as building a
# generator that keeps track of its own byte offset and spits out
# tuples for the data transformers (adapters) to consume.  What I've
# been doing so far is just working with static structures, but I
# should be able to figure out how to turn the list comprehensions
# I've been using into generators.


# Key idea: moving from static to dynamic involves turning lists into
# generators!

# Less sure about: adapters as coroutines to do data transformation,
# using internal state to keep track of how many objects it's seen so
# it knows when to terminate.


# Very high-level overview of the architecture:

# On one end, the unpacker will be an iterator that produces a stream
# of tuples according to the rules embedded in the the declarative
# structure.  Dynamic combinators affect the rules in this iterator.
# This is a non-trivial compilation problem.

# The rest of the program looks schematically like:

# Unpacker -> data transformers -> Struct/Sequence -> Python representation

# Python representation -> Struct/Sequence -> data transformers -> Packer

# The unpacker is the producer, taking the binary data and making it
# into python tuples.  Everything else, including the packer, should
# be coroutines.  That said, at some point there has to be a yield
# statement to produce the Python representation that's the output of
# the parser and another producer that feeds the Python representation
# into the coroutines leading to the packer.  Theoretically, if the
# problem solution involves transforming binary data in some way or
# writing binary data back, the coroutine at the end of the unpacking
# process could skip the Struct/Sequence step altogether and send data
# straight into a packer, either the same packer to write back to the
# same binary data format with the data transformed in some way or a
# different packer to write to a different binary data format.

# Note that the order of the coroutines for the unpacking process goes
# from the innermost levels of the declarative structure outwards.
# However, I don't think this works for all cases---transforming bytes
# into bits is going to have to live outside the data-processers in
# the declarative syntax but at the innermost level in the procedural
# implementation.  The pipeline may have to be set up on a
# case-by-case basis.

# This framework has some natural iterative structure.  I need to set
# up the declarative syntax for deciding the final units for the
# Python representation.

# To figure out how to set this up, let's try some examples from DVI
# files that aren't too hard.  (No tunneling, yet.)

# VF preamble: 'B', 'BB', length-'s', 'qq'

# Note: have to be able to dynamically add struct format strings for
# things like Pascal strings to the dictionary at run time.

# Procedurally:

# 1: parser generates a one-tuple of an int.

# 2: byte -> command name in return object but not context, sends
# return object on to Struct.

# 3: parser reads context, figures out it needs to call 'BB' next,
# generates a two-tuple of ints.

# 4: first byte is checked, raise an error if it doesn't match, sends
# it nowhere.

# 5: second byte put into the context but not sent on.

# 6: parser reads the previous byte and generates a string of that length.

# 7: decode string and send it on to Struct.

# 8: parser generates a two-tuple of ints.

# 9: send first int on to Struct.

# 10: transform int into a fix_word and send it on to Struct.

# 11: Struct groups objects it received into an OrderedDict with its
# own names and returns the Python representation.


# Base class for parts of the grammar known at compile time.

# Need to inherit from object for super() to work in 2.7
class Static(object):
    # The trivial compiler itself
    def __init__(self, *combinators):
        # There are two central data structures here.  The first is a
        # dictionary with struct format strings as keys and tuples of
        # struct instances and the length of the tuple that each
        # instance produces and accepts for unpacking and packing.
        # (Struct doesn't provide a convenient method for finding this
        # out.)  The second is a list of format strings that's used to
        # build the lists of pointers to struct.unpack or struct.pack,
        # or possibly dynamic combinators' pack and unpack functions.
        self.structs = {}
        self.formats = []

        self._compile = singledispatch(self._compile)
        self._compile.register(_StructBase, self._compile_atom)
        self._compile.register(Static, self._compile_static)
        # print(self._compile.registry)

        # While being built, each list starts with the endianness
        # (initially neither) followed by all the format codes.
        self.compiling = _StructSequence([])
        for combinator in combinators:
            self._compile(combinator)
        self.formats.append(self.compiling.format)
        if self.formats[-1] not in self.structs:
            self.structs[self.formats[-1]] = (self.compiling.length, _struct.Struct(self.formats[-1]))
        byte_offsets = list(itertools.accumulate([0] + [self.structs[s][1].size for s in self.formats]))
        self._packers = [(b, self.structs[s][1].pack_into) for s, b in zip(self.formats, byte_offsets)]
        self._unpackers = [(b, self.structs[s][1].unpack_from) for s, b in zip(self.formats, byte_offsets)]
        self.lengths = [self.structs[s][0] for s in self.formats]
        self.iterations = len(self.lengths)
        self.length = sum(self.lengths)

        self._pack_into = self._sink_coroutine().send
        self._pack_into(None)

    def _compile(self, combinator):
        raise NotImplementedError

    def _compile_atom(self, atom):
        if atom.same_endianness(self.compiling):
            self.compiling = self.compiling + atom
        else:
            self.formats.append(self.compiling.format)
            if self.formats[-1] not in self.structs:
                self.structs[self.formats[-1]] = (self.compiling.length, _struct.Struct(self.formats[-1]))
            self.compiling = atom

    # Debug code to look at changes in the shared state variable.

    # @property
    # def data(self):
    #     return self._data

    # @data.setter
    # def data(self, value):
    #     self._data = value
    #     print(self._data)

    def _compile_static(self, combinator):
        raise NotImplementedError

    # Two ways to get data out of a coroutine: use shared state, which
    # means in this case passing in a mutable object and having each
    # coroutine alter it---notice that since the coroutines will in
    # general not belong to the same class, the variable has to be
    # passed---or have each yield/send both accept and return a value,
    # allowing the coroutine chain to pass back the result of its
    # computation to the original caller.  Construct 2 and 3 (I think)
    # use pass-back.

    # I'm going for pass back, despite its problems, for conceptual
    # simplicity.  First, not all the methods will belong to the same
    # class instance, so to use a mutable object, I'd have to pass it
    # forward each time: when initializing each coroutine, assign the
    # same mutable object to each one.  Second, consider a simple case
    # of a nested Sequence.  With shared state, the inner Sequence
    # needs to insert a list into the mutable object, but it doesn't
    # know anything about the object so doesn't know how to insert it.
    # However, if it just assembles whatever it receives into a list
    # and passes that forward, then the terminal coroutine can insert
    # the list into the final object, and it has to know what the
    # final object is.

    # Insight: all the coroutines except the sink and the producer
    # need to be retargetable with each piece of data sent into them.
    # Examples: a nested Sequence/Struct will require the producer to
    # retarget from the outer Struct/Sequence to the inner one and
    # possibly vice versa.  A stateless data mutator that just takes
    # an integer and does something to it like an enum or a fix_word
    # might need to send its output to many different coroutines
    # during the course of parsing.  This also solves the problem with
    # instantiating coroutines at compile time: when instantiating
    # one, I already have stop it with a yield that just eats the
    # first send(None) to start it up, and that means the first real
    # call will already include both data and target.

    # I need to think about instantiation differently: class-based
    # variables known at compile time (example: Struct names) should
    # be included in the instantiation call to a coroutine in the
    # class's __init__.  Changing how targeting works requires me to
    # rethink the pack_into pipeline.  Most likely, the pack_into
    # coroutine will have to become much more complicated because it
    # will have to take over some of the burden of setting up
    # targeting.  This may require instantiating the sink coroutine
    # when pack_into is first called.

    # This is the ultimate producer
    def unpack_from(self, stream):
        target = self._unpack_from
        for byte_offset, unpacker in self._unpackers:
            data = target(unpacker(stream, byte_offset))
        return data

    def _unpack_coroutine(self):
        raise NotImplementedError

    def _pack_coroutine(self):
        raise NotImplementedError

    # This is the ultimate sink
    def _sink_coroutine(self):
        while 1:
            stream = (yield)
            for byte_offset, packer in self._packers:
                data = (yield)
                packer(stream, byte_offset, *data)

# Lengths and byte sizes have to be checked at parse time because of
# the possibility of dynamic packers and unpackers.  For static
# packers and unpackers, I could pre-compute these values, but I don't
# know how much speed that will actually get me---how slow is a
# look-up to self compared to a function call?
# This is a sub-category of a more general problem: I have lots of
# lists that are going to be mixes of constant values and functions that
# need to be computed at parse time.  I need to figure out how to
# efficiently handle this problem, because it's everywhere and it's
# going to be a huge part of how good my performance is.

# For unpacking from a buffer, I can unpack and then compute the
# length afterward.  This is a built-in function so it ought to be
# fairly fast, probably faster than keeping a list of lengths and
# doing the lookup and certainly faster than calling Python functions.

# In any event, I'm going to need the sizes in bytes for both packing
# and unpacking: for buffers, I'll need to know them to calculate the
# offset in the buffer, and for files and io.BytesIO objects, I'll
# need them to calculate how many bytes to read.  (Note: reads to
# files and io.BytesIO objects are also in the inner loop, I should
# probably just read the entire data stream in rather than making
# repeated calls and unpack from the resulting buffer.)


# These two classes are central because, essentially, everything has
# to be wrapped in one or the other to avoid O(n^2) behavior.  Struct
# returns tuples, tuples are immutable, concatenating immutable
# sequences is O(n^2) in the total sequence length (and nesting
# doesn't help), thus the tuples have to be converted to mutable
# objects before doing other data transformations.

class Sequence(Static):
    def __init__(self, *combinators):
        # No-argument form of super() is not allowed in 2.7
        super(Sequence, self).__init__(*combinators)
    
        # Set up coroutines
        self._unpack_from = self._unpack_coroutine().send
        self._unpack_from(None)
        self.pack_into = self._pack_coroutine(self._pack_into).send
        self.pack_into(None)

    def _unpack_coroutine(self):
        unpacked = (yield)
        while 1:
            data = []
            for i in range(self.iterations):
                data.extend(unpacked)
                unpacked = (yield data)

    def _pack_coroutine(self, target):
        while 1:
            stream, data = (yield)
            position = 0
            target(stream)
            for length in self.lengths:
                target(data[position:position + length])
                position += length


class Struct(Static):
    def __init__(self, *combinators):
        self.names = [combinator[0] for combinator in combinators]
        # No-argument form of super() is not allowed in 2.7
        super(Struct, self).__init__(*[combinator[1] for combinator in combinators])

        # Set up coroutines
        self._unpack_from = self._unpack_coroutine(self.names).send
        self._unpack_from(None)
        self.pack_into = self._pack_coroutine(self._pack_into, self.names).send
        self.pack_into(None)

    def _unpack_coroutine(self, names):
        unpacked = (yield)
        while 1:
            data = collections.OrderedDict()
            position = 0
            for i in range(self.iterations):
                for name, datum in zip(names[position:], unpacked):
                    data[name] = datum
                position += len(unpacked)
                unpacked = (yield data)

    def _pack_coroutine(self, target, names):
        while 1:
            stream, data = (yield)
            position = 0
            target(stream)
            for length in self.lengths:
                target([data[name] for name in names[position:position + length]])
                position += length


# The following classes implement an algebra of struct format strings.
# _StructElement is the basic unit making up format strings.  Each
# _StructElement has three properties, endianness, a pair of a count
# and a character, and a length that represents the length of the
# tuple that the _StructElement would return for unpacking or require
# for packing.  (In the standard library struct module, this length is
# always 1 except for padding, 'x', for which it is zero, but I'm
# leaving it as a parameter for extensibility: for instance, an
# implementation of bits in struct might unpack one byte and return a
# tuple of eight bits, and if I implemented C unions, I'd need even
# more flexibility.)  _StructSequence is a container for
# _StructElements.  There are two kinds of _StructElements, numbers
# and strings.  The _StructSequences and the two kinds of
# _StructElements transform in different ways under two operations,
# format addition and scalar multiplication.

# There are four kinds of endianness of concern, aligned native, big,
# little, and any, but they can be represented internally by three
# codes, '', '>', and '<'.  Addition of formats with different
# endianness isn't allowed, but bytes and other data types interpreted
# as arrays of bytes (strings) have no endianness so can be combined
# in _StructSequences of any endianness, otherwise the sum
# _StructSequence has endianness as below.

#          bar
#        <  >  ''  
#     <  <  X  <
# foo >  X  >  >
#     '' <  >  ''

# Since struct defaults to aligned native endianness when no
# endianness character is present, if for some reason someone needs
# that they can use a string with no endianness set.

# Scalar multiplication doesn't affect endianness.

# Scalar multiplication examples:

# Number -> number: 2*'2b' = '4b'
# String -> sequence: 2*'2s' = '2s2s'
# Sequence -> sequence: 2*'bh' = 'bhbh'
# Sequence -> sequence: 2*'b2sb' = 'b2s2b2sb'

# Scalar multiplication is commutative.

# Sequence addition examples:

# Same numbers = number: '2b' + 'b' = '3b'
# Different numbers = sequence: 'b' + 'h' = 'bh'
# Number + string = sequence: 'b' + '2s' = 'b2s'
# String + string = sequence: '2s' + '3s' = '2s3s'
# Number + sequence = sequence: '2b' + 'bh' = '3bh'
# String + sequence = sequence: '2s' + 'bh' = '2sbh'
# Sequence + sequence = sequence: 'bh' + 'BH' = 'bhBH'
# Sequence + sequence = sequence: 'bhb' + bh' = 'bh2bh'

# Sequence addition is *not* commutative.

# Length transforms simply: multiply it for scalar multiplication, add
# the lengths for sequence addition.

# Padding, 'x', transforms like a number.

# Need to inherit from object for singledispatch to work in 2.7
class _StructBase(object):
    @property
    def endianness(self):
        return self._ENDIANNESS

    def same_endianness(self, other):
        return self.endianness in other.endianness or other.endianness in self.endianness

    def _check_mul(self, other):
        if not isinstance(other, (int, numbers.Integral)):
            return NotImplemented
            # raise NotImplementedError('Cannot multiply by a noninteger: {}'.format(other))
        if other < 0:
            return NotImplemented
            # raise NotImplementedError('Cannot multipy by a negative number: {}'.format(other))


class _StructSequence(_StructBase):
    _ENDIANNESS = ''

    def __init__(self, elements):
        self._elements = elements

    @property
    def length(self):
        return sum(s.length for s in self._elements)

    @property
    def format(self):
        return ''.join(str(i) for i in itertools.chain.from_iterable([self._ENDIANNESS] + [(j, k) for i, j, k in self._elements]) if i != 1)

    def _check_add(self, other):
        if not isinstance(other, _StructBase):
            return NotImplemented
            # NotImplementedError('Cannot add a format string to another type: {}'.format(other))
        if not self.same_endianness(other):
            return NotImplemented
            # raise NotImplementedError('Cannot add format strings with different endiannesses.')
        if self.endianness == other.endianness or len(self.endianness) > len(other.endianness):
            return type(self)
        else:
            if isinstance(other, _StructSequence):
                return type(other)
            else:
                return other._STRUCT_SEQUENCES[other.endianness]

    def __add__(self, other):
        struct_sequence = self._check_add(other)
        if isinstance(other, _StructSequence):
            if other._elements == []:
                return struct_sequence(self._elements.copy())
            else:
                begin = other._elements[0]
                rest = other._elements[1:]
        else:
            begin = other
            rest = []
        if self._elements == []:
            rest.insert(0, begin)
            return struct_sequence(rest)
        if self._elements[-1].character == begin.character:
            return struct_sequence(self._elements[:-1] + [self._elements[-1] + begin] + rest)
        else:
            return struct_sequence(self._elements + [begin] + rest)

    def __radd__(self, other):
        struct_sequence = self._check_add(other)
        if isinstance(other, _StructSequence):
            if other._elements == []:
                return struct_sequence(self._elements.copy())
            else:
                end = other._elements[-1]
                rest = other._elements[:-1]
        else:
            end = other
            rest = []
        if self._elements == []:
            rest.append(end)
            return struct_sequence(rest)
        if self._elements[0].character == begin.character:
            return struct_sequence(rest + [end + self._elements[0]] + self._elements[1:])
        else:
            return struct_sequence(rest + end + self._elements)

    def __iadd__(self, other):
        struct_sequence = self._check_add(other)        
        if type(self) != struct_sequence:
            return self + other
        if isinstance(other, _StructSequence):
            if other._elements == []:
                return self
            begin = other._elements[0]
            rest = other._elements[1:]
        else:
            begin = other
            rest = []
        if self._elements == []:
            rest.append(end)
            return struct_sequence(rest)
        end = self._elements.pop()
        if end.character == begin.character:
            self._elements.append(end + begin).extend(rest)
            return self
        else:
            self._elements.append(end).append(begin).extend(rest)
            return self

    def __mul__(self, other):
        _check_mul(self, other)
        if self._elements == []:
            return type(self)([])
        if self._elements[0].character == self._elements[-1].character:
            number = self._elements[-1] + self._elements[0]
            repeating_elements = self._elements[1:-1].append(number)
            return type(self)(self._elements[0] + repeating_elements * (other - 1) + self._elements[1:])
        else:
            return type(self)(self._elements * other)

    def __imul__(self, other):
        _check_mul(self, other)
        if self._elements == []:
            return self
        if self._elements[0].character == self._elements[-1].character:
            begin = self._elements.pop(0)
            end = self._elements.pop()
            self._elements += [begin + end]
            self._elements *= other - 1
            self._elements.insert(0, begin).append(end)
            return self
        else:
            self._elements *= other
            return self

    __rmul__ = __mul__

class _BigEndianStructSequence(_StructSequence):
    _ENDIANNESS = '>'

class _LittleEndianStructSequence(_StructSequence):
    _ENDIANNESS = '<'


# Inheriting from a namedtuple here makes the module-level definitions
# immutable.
_StructTuple = collections.namedtuple('_StructTuple', 'length count character')

class _StructElement(_StructTuple, _StructBase):
    _ENDIANNESS = ''
    __slots__ = ()
    _STRUCT_SEQUENCES = {
        '' : _StructSequence,
        '>' : _BigEndianStructSequence,
        '<' : _LittleEndianStructSequence
    }

    @property
    def format(self):
        count = str(self.count) if self.count != 1 else '' 
        return ''.join([self.endianness, count, self.character])

    def _check_add(self, other):
        if not isinstance(other, _StructElement):
            return NotImplemented
        if not self.same_endianness(other):
            return NotImplemented
            # raise NotImplementedError('Cannot add format strings with different endiannesses.')


class _StructString(_StructElement):
    __slots__ = ()
    def __add__(self, other):
        self._check_add(other)
        endianness = max(self.endianness, other.endianness)
        return self._STRUCT_SEQUENCES[endianness]([self, other])

    def __mul__(self, other):
        self._check_mul(other)
        return self._STRUCT_SEQUENCES[self.endianness]([self] * other)

    __rmul__ = __mul__

# class _BigEndianStructString(_StructString):
#     _ENDIANNESS = '>'
#     __slots__ = ()

# class _LittleEndianStructString(_StructString):
#     _ENDIANNESS = '<'
#     __slots__ = ()


class _StructNumber(_StructElement):
    __slots__ = ()

    def __add__(self, other):
        self._check_add(other)
        if self.character == other.character:
            return type(self)(self.length + other.length, self.count + other.count, self.character)
        else:
            endianness = max(self.endianness, other.endianness)
            return self._STRUCT_SEQUENCES[endianness]([self, other])

    def __mul__(self, other):
        self._check_mul(other)
        return type(self)(self.length * other, self.count * other, self.character)

    __rmul__ = __mul__

class _BigEndianStructNumber(_StructNumber):
    _ENDIANNESS = '>'
    __slots__ = ()

class _LittleEndianStructNumber(_StructNumber):
    _ENDIANNESS = '<'
    __slots__ = ()


uint8 = _StructNumber(1, 1, 'B')
sint8 = _StructNumber(1, 1, 'b')

# ubint8 = _BigEndianStructNumber(1, 1, 'B')
# sbint8 = _BigEndianStructNumber(1, 1, 'b')
ubint16 = _BigEndianStructNumber(1, 1, 'H')
sbint16 = _BigEndianStructNumber(1, 1, 'h')
ubint24 = _BigEndianStructNumber(1, 1, 'J')
sbint24 = _BigEndianStructNumber(1, 1, 'j')
ubint32 = _BigEndianStructNumber(1, 1, 'L')
sbint32 = _BigEndianStructNumber(1, 1, 'l')
ubint64 = _BigEndianStructNumber(1, 1, 'Q')
sbint64 = _BigEndianStructNumber(1, 1, 'q')
float32b = _BigEndianStructNumber(1, 1, 'f')
float64b = _BigEndianStructNumber(1, 1, 'd')

# ulint8 = _LittleEndianStructNumber(1, 1, 'B')
# slint8 = _LittleEndianStructNumber(1, 1, 'b')
ulint16 = _LittleEndianStructNumber(1, 1, 'H')
slint16 = _LittleEndianStructNumber(1, 1, 'h')
ulint24 = _LittleEndianStructNumber(1, 1, 'J')
slint24 = _LittleEndianStructNumber(1, 1, 'j')
ulint32 = _LittleEndianStructNumber(1, 1, 'L')
slint32 = _LittleEndianStructNumber(1, 1, 'l')
ulint64 = _LittleEndianStructNumber(1, 1, 'Q')
slint64 = _LittleEndianStructNumber(1, 1, 'q')
float32l = _LittleEndianStructNumber(1, 1, 'f')
float64l = _LittleEndianStructNumber(1, 1, 'd')

def padding(length):
    return _StructNumber(0, length, 'x')

def raw(length):
    return _StructString(1, length, 's')

def fixed_pascal_string(length):
    return _StructString(1, length, 'p')


if __name__ == '__main__':
    print(Sequence(uint8).formats)
    print(Sequence(uint8*4).formats)
    if three_bytes_available: 
        print(Sequence(uint8, uint8, sint8, ubint24).formats)
        print(Sequence(uint8, uint8, sint8, slint24).formats)
    print(Sequence(uint8, ulint32, uint8).formats)
    print(Sequence(uint8, ulint32, uint8, ubint32, sint8).formats)
    print(Sequence(float32b, ulint32, ubint32, padding(10), raw(10), fixed_pascal_string(10), ulint32*4).formats)

    b = bytearray(b'abcdefghijk')
    e = bytearray(11)
    s = Sequence(uint8, ulint32, uint8, ubint32, sint8)
    print(s.formats)
    print(s.unpack_from(b))
    s.pack_into((e, s.unpack_from(b)))
    print(e)

    t = Struct(('a', uint8), ('b', ulint32), ('c', uint8), ('d', ubint32), ('e', sint8))
    print(t.unpack_from(b))
    t.pack_into((e, t.unpack_from(b)))
    print(e)

    # Struct(
    #     ("foo", byte),
    #     ("bar", Struct(
    #         ("spam", int16ul),
    #         ("bacon", int64sb),
    #     )),
    #     ("viking", int32sl),
    # )


    # Construct2 versions
    i2seq = """
import construct as construct2
s = construct2.Sequence(None,
    construct2.UBInt8('a'),
    construct2.ULInt16('b'),
    construct2.SBInt64('c'),
    construct2.SLInt32('d')
)
b = bytearray(b'abcdefghijklmno')
e = bytearray(15)
    """

    i2struct = """
import construct as construct2
s = construct2.Struct(None,
    construct2.UBInt8("foo"),
    construct2.ULInt16("spam"),
    construct2.SBInt64("bacon"),
    construct2.SLInt32("viking")
)
b = bytearray(b'abcdefghijklmno')
e = bytearray(15)
    """


    # Construct3 versions
#     i3seq = """
# import construct3
# s = construct3.Sequence(
#     construct3.uint8,
#     construct3.uint16l,
#     construct3.sint64b,
#     construct3.sint32l
# )
# b = bytearray(b'abcdefghijklmno')
# e = bytearray(15)
#     """

#     i3struct = """
# import construct3
# s = construct3.Struct(
#     ("foo", construct3.uint8),
#     ("spam", construct3.uint16l),
#     ("bacon", construct3.sint64b),
#     ("viking", construct3.sint32l)
# )
# b = bytearray(b'abcdefghijklmno')
# e = bytearray(15)
#     """


    iprotoseq = """
from __main__ import Sequence, uint8, ulint16, sbint64, slint32
s = Sequence(
    uint8,
    ulint16,
    sbint64,
    slint32
)
b = bytearray(b'abcdefghijklmno')
e = bytearray(15)
    """

    iprotostruct = """
from __main__ import Struct, uint8, ulint16, sbint64, slint32
s = Struct(
    ("foo", uint8),
    ("spam", ulint16),
    ("bacon", sbint64),
    ("viking", slint32)
)
b = bytearray(b'abcdefghijklmno')
e = bytearray(15)
    """

    r2 = """
s.build(s.parse(b))
    """

    r3 = """
s.pack(s.unpack(b))
    """

    rproto = """
s.pack_into((e, s.unpack_from(b)))
    """

    print('Construct 2, Sequence', timeit.timeit(r2, i2seq, number=10000))
#    print('Construct 3, Sequence', timeit.timeit(r3, i3seq, number=10000))
    print('Prototype, Sequence', timeit.timeit(rproto, iprotoseq, number=10000))
    print('Construct 2, Struct', timeit.timeit(r2, i2struct, number=10000))    
#    print('Construct 3, Struct', timeit.timeit(r3, i3struct, number=10000))    
    print('Prototype, Struct', timeit.timeit(rproto, iprotostruct, number=10000))
