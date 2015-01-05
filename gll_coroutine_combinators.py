#!/usr/bin/python3

# Python 2 compatibility
from __future__ import absolute_import, division, print_function, unicode_literals
import six

# Is enum on 3.4+, from PyPi as enum34 on 3.3-2.4
try:
    import enum
except ImportError:
    import enum34 as enum

# In functools on 3.4+, from PyPi as singledispatch on 3.3-2.6
try:
    from functools import singledispatch as _singledispatch
except ImportError:
    from singledispatch import singledispatch as _singledispatch

from keyword_only_args.decorator_closure import keyword_only_args


import abc
# Counter is only in 2.7, but Hettinger has a recipe for 2.5 on his page.
import collections
import functools
import itertools
import weakref


from spf import Node, SeqNode, PackedNode, Leaf, SharedPackedForest, TreeView


class ParseError(Exception):
    pass


Success = collections.namedtuple('Success', 'node label offsets')

Failure = collections.namedtuple('Failure', 'msg offset')

Label = collections.namedtuple('Label', 'combinator start end')

DEFAULT_FAILURE = Failure('This is the default failure for combinators that cannot generate their own failures.  It should never be returned.', -1)


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
    class DataType(enum.Enum):
        text = 1
        binary = 2

    if six.PY3:
        _DATA_TYPES = {str: DataType.text,
                      bytes: DataType.binary,
                      bytearray: DataType.binary,
                      memoryview: DataType.binary}
    else:
        _DATA_TYPES = {str: DataType.binary,
                      unicode: DataType.text,
                      memoryview: DataType.binary}

    @abc.abstractmethod
    @keyword_only_args()
    def __init__(self, name=None, *args, **kws):
        self._check_args(args)
        self._data_type = self._inherit_data_type(args)

    def _check_args(self, args, arg_type=Combinator):
        if len(args) == 0:
            raise TypeError('%s takes at least 1 argument.' % type(self).__name__)
        if any(not isinstance(c, arg_type) for c in combinators):
            raise TypeError('Arguments to %s must be %ss.' % (type(self).__name__, arg_type.__name__))

    def _inherit_data_type(self, combinators):
        data_type = combinators[0]._data_type
        if any(c._data_type is not data_type for c in combinators):
            raise TypeError('Combinators passed to %s must handle the same kind of data.' % type(self).__name__)
        return data_type

    @property
    def data_type(self):
        return self._data_type

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
        if self._DATA_TYPES[type(stream)] is not self._data_type:
            raise ParseError('%s expected %s as input, got %s instead' % (type(self), self._data_type.name, self._DATA_TYPES[type(stream)].name))

    @abc.abstractmethod
    def _parse(self, stream, sppf, offset):
        raise NotImplementedError

    def unparse(self, tree, stream, offset=0):
        pass

    def _unparse(self, tree, stream, offset):
        raise NotImplementedError


class Sequence(Combinator):

    @keyword_only_args()
    def __init__(self, name=None, Node=SeqNode, *combinators, **kws):
        super(Sequence, self).__init__(*combinators, **kws)
        self.combinators = self._merge_combinators(combinators)

    def _merge_combinators(self, combinators):
        combinators = iter(combinators)
        merged = [next(combinators)]
        for combinator in combinators:
            if isinstance(merged[-1], Terminal) and isinstance(combinator, Terminal):
                merged[-1] += combinator
            else:
                merged.append(combinator)
        return merged


class Terminal(Sequence):
    """This inherits from Sequence: note that there's no real distinction
    between a sequence of terminals and a single terminal.

    Parsers have a type of input they expect, and you can't mix
    parsers that accept different kinds of input.  Note this is
    different from passing output to a subparser using Act/>>.

    """

    @keyword_only_args()
    def __init__(self, name=None, Node=SeqNode, *combinators, **kws):
        self._check_args(args, arg_type=Terminal)
        self._data_type = self._inherit_data_type(args)
        self.combinators = self._merge_combinators(combinators)
        self._parsers = [p for c in self.combinators for p in c._parsers]

    def _text_binary(self, args):
        if any(type(a) not in self._DATA_TYPES or type(a) is memoryview for a in args):
            raise TypeError('%s only accepts text or binary strings.' % type(self).__name__)
        data_type = self._DATA_TYPES[type(args[0])]
        if any(self._DATA_TYPES[type(a)] != data_type for a in args):
            raise TypeError('Arguments to %s must all be the same kind of data.' % type(self).__name__)
        return data_type

    def _operator_dispatch(self):
        self.__add = _singledispatch(self.__add)
        self.__add.register(Vector, self._dot_product)
        self.__add.register(int, self._scalar_mult)

        self.__mul = _singledispatch(self.__mul)
        self.__mul.register(Vector, self._dot_product)
        self.__mul.register(int, self._scalar_mult)

        self.__or = _singledispatch(self.__or)
        self.__or.register(Vector, self._dot_product)
        self.__or.register(int, self._scalar_mult)


    def __mul__(self, other):
        return self.__mul(other)
    def __mul(self, other):
        return NotImplemented

    def __add__(self, other):
        return self.__add(other)
    def __add(self, other):
        return NotImplemented

    def __or__(self, other):
        return self.__or(other)
    def __or(self, other):
        return NotImplemented


    def _parse(self, stream, sppf, offset):
        start = offset
        results = []
        for parser in self._parsers:
            result = parser(stream, offset)
            if isinstance(result, Failure):
                return result
            else:
                results.extend(result[0])
                offset = result[1]
        label = Label(self, start, end)
        node = self.Node(results)
        sppf._nodes[label] = node
        return Success(node, label, {end})


class Strings(Terminal):
    @keyword_only_args()
    def __init__(self, name=None, Node=SeqNode, *strings, **kws):
        self._data_type = self._text_binary(strings)
        self.__strings_lengths = [(s, len(s)) for s in strings]
        self._parsers = [self.__parse]

    def __parse(self, stream, offset):
        start = offset
        strings = []
        for string, length in self.__strings_lengths:
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
    import re
    # Consider seeing if it's possible to implement a weak-value defaultdict
    __regexes_cache = defaultdict(re.compile) # weakref.WeakValueDictionary()

    @keyword_only_args()
    def __init__(self, name=None, Node=SeqNode, *patterns, **kws):
        """I may want to at some point limit what regular expressions are
        available here---but bizarrely, the easiest way to do that
        might be to write my library and use *it* to parse the regular
        expressions.  Alternately, I may want to use RE2 anyways, to
        prevent people from blowing my performance bounds.

        Also doesn't support flags at the moment---they can be
        included in the regular expression, anyways.  Who wrote this
        API?

        There is more or less no error encapsulation at the moment,
        because I don't even know what the possible errors are.  This
        *must* be fixed.

        Massive API leak all over the place, need to fix.
        """
        self._data_type = self._text_binary(patterns)
        self.__regexes = []
        self._parsers = []
        failure_msg = '"%s" did not match "{}"'
        for pattern in patterns:
            regex = self.__regexes_cache[pattern]
            self.__regexes.append(regex)
            self._parsers.append(self._parser_wrapper(regex.match, failure_msg % pattern))

    @staticmethod
    def _parser_wrapper(matcher, failure_msg):
        def __Regex_parser(stream, offset):
            match = matcher(stream, offset)
            if match:
                return (match.groups(), match.end)
            else:
                return Failure(failure_msg, offset)
        return __Regex_parser


class Binary(Terminal):
    import struct
    import numbers # For format string algebra
    __structs_cache = defaultdict(struct.Struct) # weakref.WeakValueDictionary()
    _data_type = self.DataType.binary

    @keyword_only_args()
    def __init__(self, name=None, Node=SeqNode, *format_strs, **kws):
        self._text_binary(format_strs)
        self.__structs = [self.__structs_cache[f] for f in format_strs]
        self._parsers = [self.__parse]

    def __parse(self, stream, offset):
        data = []
        for struct in self.__structs:
            try:
                data.extend(struct.unpack_from(stream, offset))
                offset += struct.size
            except struct.error as error:
                return Failure(error.args[0], offset)
        return (data, offset)

    class _StructBase(six.with_metaclass(abc.ABCMeta, object)):
        """The _Struct classes implement an algebra of struct format strings.
        _StructAtom is the basic unit making up format strings.  Each
        _StructAtom has four properties, endianness, a count, a
        character, and a length that represents the length of the
        tuple that the _StructAtom would return for unpacking or
        require for packing.  (In the standard library struct module,
        this length is always 1 except for padding, 'x', for which it
        is zero, but I'm leaving it as a parameter for extensibility:
        for instance, an implementation of bits in struct might unpack
        one byte and return a tuple of eight bits, and if I
        implemented C unions, I'd need even more flexibility.)
        _StructSequence is a container for _StructAtoms.  There are
        two kinds of _StructAtoms, numbers and strings.
        _StructSequences and the two kinds of _StructAtoms transform
        in different ways under two operations, concatenation and
        scalar multiplication.

        There are four kinds of endianness of concern, aligned native, big,
        little, and any, but they can be represented internally by three
        codes, '', '>', and '<'.  Concatenation of formats with different
        endianness isn't allowed, but bytes and other data types interpreted
        as arrays of bytes (strings) have no endianness so can be combined
        in _StructSequences of any endianness, otherwise the sum
        _StructSequence has endianness as below.

                 bar
               <  >  ''  
            <  <  X  <
        foo >  X  >  >
            '' <  >  ''

        Since struct defaults to aligned native endianness when no
        endianness character is present, if for some reason someone
        needs that they can use a string endianness ''.

        Scalar multiplication doesn't affect endianness.

        Scalar multiplication examples:

        Number -> number: 2*'2b' = '4b'
        String -> sequence: 2*'2s' = '2s2s'
        Sequence -> sequence: 2*'bh' = 'bhbh'
        Sequence -> sequence: 2*'b2sb' = 'b2s2b2sb'

        Scalar multiplication is commutative.

        Format concatenation examples:

        Same numbers = number: '2b' + 'b' = '3b'
        Different numbers = sequence: 'b' + 'h' = 'bh'
        Number + string = sequence: 'b' + '2s' = 'b2s'
        String + string = sequence: '2s' + '3s' = '2s3s'
        Number + sequence = sequence: '2b' + 'bh' = '3bh'
        String + sequence = sequence: '2s' + 'bh' = '2sbh'
        Sequence + sequence = sequence: 'bh' + 'BH' = 'bhBH'
        Sequence + sequence = sequence: 'bhb' + bh' = 'bh2bh'

        Sequence addition is *not* commutative.

        Length transforms simply: multiply it for scalar multiplication, add
        the lengths for concatenation.

        Padding, 'x', transforms like a number.

        """
        __slots__ = ('_endianness')

        class Endianness(enum.Enum):
            big = '>'
            little = '<'
            none = ''
            def __ge__(self, other):
                if type(self) is type(other):
                    return self.value in other.value or other.value in self.value
                return NotImplemented
            __le__ = __ge__
            def __gt__(self, other):
                if type(self) is type(other):
                    return other.value in self.value
                return NotImplemented
            def __lt__(self, other):
                if type(self) is type(other):
                    return self.value in other.value
                return NotImplemented

        @abc.abstractmethod
        def __init__(self):
            self._add = _singledispatch(self._add)
            self._radd = _singledispatch(self._radd)
            self._mul = _singledispatch(self._mul)
            self._mul.register(int, self._mul_int)
            self._mul.register(numbers.Integral, self._mul_int)

        def __add__(self, other):
            return self._add(other)
        def _add(self, other):
            return NotImplemented
        # def _add_seq(self, other):
        #     raise NotImplementedError
        # @abc.abstractmethod
        # def _add_atom(self, other):
        #     raise NotImplementedError
        def __radd__(self, other):
            return self._radd(other)
        # @abc.abstractmethod
        # def _radd_seq(self, other):
        #     raise NotImplementedError
        # @abc.abstractmethod
        # def _radd_atom(self, other):
        #     raise NotImplementedError
        def __mul__(self, other):
            self._check_mul(other)
            return self._mul(other)
        __rmul__ = __mul__
        def _mul(self, other):
            return NotImplemented
        @abc.abstractmethod
        def _mul_int(self, other):
            raise NotImplementedError

        @property
        def endianness(self):
            return self._endianness.value

        def _check_mul(self, other):
            if other < 0:
                raise TypeError('Cannot multiply a %s by a negative integer.' % type(self).__name__)

    class _StructSequence(_StructBase, collections.MutableSequence):
        __slots__ = ('_atoms')

        def __init__(self, *atoms):
            if len(atoms) < 1:
                raise TypeError('%s takes at least 1 argument.' % type(self).__name__)
            if not all(isinstance(a, _StructAtom) for a in atoms):
                raise TypeError('%s only takes _StructAtoms.' % type(self).__name__)
            counter = collections.Counter(a._endianness for a in atoms)
            if counter[self.Endianness.big] > 0 and counter[self.Endianness.little] > 0:
                raise TypeError('The same format string cannot contain atoms with different endiannesses.')
            elif counter[self.Endianness.big] > 0:
                self._endianness = self.Endianness.big
            elif counter[self.Endianness.little] > 0:
                self._endianness = self.Endianness.little
            else:
                self._endianness = self.Endianness.none
            super(_StructSequence, self).__init__()
            self._add.register(_StructSequence, self._add_seq)
            self._add.register(_StructAtom, self._add_atom)
            self._iadd = _singledispatch(self._iadd)
            self._iadd.register(_StructSequence, self._iadd_seq)
            self._iadd.register(_StructAtom, self._iadd_atom)
            self._imul = _singledispatch(self._imul)

            atoms = iter(atoms)
            self._atoms = [next(atoms)]
            for atom in atoms:
                self._push_atom(atom)

        def _add_seq(self, other):
            return type(self)(*(self._atoms + other._atoms))
        def _add_atom(self, other)
            return type(self)(*(self._atoms + [other]))
        def _radd_seq(self, other):
            return type(self)(*(other._atoms + self._atoms))
        def _radd_atom(self, other)
            return type(self)(*([other] + self._atoms))
        def _iadd_seq(self, other):
            self._atoms.extend(other._atoms)
            return self
        def _iadd_atom(self, other):
            self._push_atom(other)
            return self

        def _push_atom(self, atom):
            tail = self._atoms[-1] + atom
            if isinstance(tail, _StructSequence): 
                self._atoms.append(atom)
            else:
                self._atoms[-1] = tail
            return tail

        def _mul_int(self, other):
            end = self._atoms[-1:]
            tail = self._push_atom(self._atoms[0])
            if isinstance(tail, _StructSequence):
                self._atoms.pop()
                return type(self)(*(self._atoms * other))
            else:
                return type(self)(*(self._atoms + self._atoms[:-1] * (other-1) + end))
        def __imul__(self, other):
            self._check_mul(other)
            return self._imul(other)
        def _imul_int(self, other):
            end = self._atoms[-1:]
            tail = self._push_atom(self._atoms[0])
            if isinstance(tail, _StructSequence):
                self._atoms *= other                
                return self
            else:
                self._atoms *= other - 1

        @property
        def length(self):
            return sum(s.length for s in self._atoms)

        @property
        def format(self):
            return ''.join(str(i) for i in itertools.chain.from_iterable([self._endianness.value] + [(j, k) for i, j, k in self._atoms]) if i != 1)

        def __imul__(self, other):
            _check_mul(self, other)
            if self._atoms == []:
                return self
            if self._atoms[0].character == self._atoms[-1].character:
                begin = self._atoms.pop(0)
                end = self._atoms.pop()
                self._atoms += [begin + end]
                self._atoms *= other - 1
                self._atoms.insert(0, begin).append(end)
                return self
            else:
                self._atoms *= other
                return self

        __rmul__ = __mul__

    class _StructAtom(_StructBase):
        __slots__ = ('length', 'count', 'character')

        @property
        def format(self):
            count = str(self.count) if self.count != 1 else '' 
            return ''.join([self.endianness, count, self.character])

        def _check_add(self, other):
            if not isinstance(other, _StructAtom):
                return NotImplemented
            if not self.same_endianness(other):
                return NotImplemented

    class _StructString(_StructAtom):
        __slots__ = ()
        def __add__(self, other):
            self._check_add(other)
            endianness = max(self.endianness, other.endianness)
            return self.__STRUCTSEQUENCES[endianness]([self, other])

        def __mul__(self, other):
            self._check_mul(other)
            return self.__STRUCTSEQUENCES[self.endianness]([self] * other)

        __rmul__ = __mul__

    class _StructNumber(_StructAtom):
        __slots__ = ()

        def __add__(self, other):
            self._check_add(other)
            if self.character == other.character:
                return type(self)(self.length + other.length, self.count + other.count, self.character)
            else:
                endianness = max(self.endianness, other.endianness)
                return self.__STRUCTSEQUENCES[endianness]([self, other])

        def __mul__(self, other):
            self._check_mul(other)
            return type(self)(self.length * other, self.count * other, self.character)

        __rmul__ = __mul__


class Bits(Terminal):
    import bitstring
    __bitstrings_cache = defaultdict(functools.partial(functools.partial(bitstring.Bits))) # weakref.WeakValueDictionary()
    _data_type = self.DataType.binary

    @keyword_only_args()
    def __init__(self, name=None, Node=SeqNode, *lengths, **kws):
        """This API needs a lot of work, partly because the underlying API is
        complicated, but that's for much later since I don't even know
        if this is the module I want.

        """
        self._text_binary(format_strs)
        self.__bytes_length = sum(lengths)
        if self.__bytes_length % 8 != 0:
            raise TypeError('Sum of length of bit fields must be a multiple of 8.')
        self.__bitstrings_lengths = [self.__structs_cache[l], l for l in lengths]
        self._parsers = [self.__parse]

    def __parse(self, stream, bytes_offset):
        bits_offset = bytes_offset * 8
        data = []
        for bitstring, length in self.__bitstrings_lengths:
            try:
                data.append(bitstring(bytes=stream, offset=bits_offset))
                bits_offset += length
            except bitstring.Error as error:
                return Failure(error.args[0], bits_offset//8 + 1)
        return (data, bytes_offset + self.__bytes_length)
