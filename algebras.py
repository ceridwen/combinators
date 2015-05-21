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

from keyword_only_args import decorator_factory as keyword_only_args


import abc
import collections
import functools
import weakref

import ordered_set


"""The general pattern of these sorts of algebras is that they have
have at least two operations, concatenation and scalar mulitplication,
an underlying atom type or types, and a sequence type.  Combinators
have at least one additional operation, alternation.  Regexes may have
multiple different additional operations.  My goal here is to write a
metaclass that factors out the patterns in the algebras so that I will
only have to write the definitions for the actual operations and then
the metaclass will define all the other operations automagically.

Key features:

1) Type dispatch.  The binary operations all have to single-dispatch
based on the type of their single argument.

2) Handling of noncommutativity.  Create right-operator code from the
left-operator code.

3) Handling of in-place operations.  Create appropriate mutating
operations from the left-operator code.

4) Handling of the init function for the sequence type.

5) Handling nested types: for combinators, Sequence and Terminal share
patterns but each need their own type dispatch.

6) Handling attribute inheritance: combinators need to inherit
text/binary, struct format strings need to inherit endianness, regexes
need to innherit flags.  This also ties with type-dispatch because
objects with the wrong attributes can't be concatenated.


The formal definition of an algebra is one or more atomic types, one
more or more collection types, and one or more operations on those
types.  Operations must be defined for all possible combinations of
types but not necessarily for all instances of types: for example,
it's not possible to concatenate struct format strings with different
endianness or combinators with different text/binary attributes.  For
each binary operation, there is a corresponding collection type, and
properties of the operation define some of the properties of the
collection type: commutative collections aren't ordered while
noncommutative operations are.  Some collections allow duplicates
while others don't.  Concatenation generates a collection type with
duplicates allowed.  For CFGs, alternation generates an unordered
collection with duplicates not allowed, PEGs or regular expressions,
alternation is not commutative and generates an ordered collection.


Code sharing:

1) init, iadd, and imul: the code that minimizes one of the sequence
types in init is the same as the code that handles in-place add, and
for some algebras, also occurs when atoms can merge during scalar
multiplication.

2) individual operations: type checking, type dispatch, and operation
code.

3) binary operators: code for handling right operations.

4) init and everything: type checking.

"""


# The isinstance check here ensures that even if
# __operation is set on a function by something else,
# this metaclass won't interpret it as part of its API

Operation = collections.namedtuple('Operation', 'name commutative idempotent')
Implementation = collections.namedtuple('Implementation', 'type operation')

def decorator_factory(namedtuple, *args):
    attribute = namedtuple(*args)
    def decorator(method):
        setattr(method, '__%s' % namedtuple.__name__.lower(), attribute)
        return method
    return decorator

operation = functools.partial(decorator_factory, Operation)
implementation = functools.partial(decorator_factory, Implementation)

Collection = collections.namedtuple('Collection', 'name operation')


class Algebra(abc.ABCMeta):
    _registry = set() # weakref.WeakValueDictionary()
    # Operation-collection type taxonomy:

    # Commutative, non-idempotent: multiset
    # Noncommutative, non-idempotent: list
    # Noncommutative, idempotent: ordered set
    # Commutative, idempotent: set
    _OPERATIONS_TYPES = {(True, False): collections.Counter,
                         (False, False): list,
                         (False, True): ordered_set.OrderedSet,
                         (True, True): set}

    def __new__(metaclass, name, bases, namespace):
        ancestors = {c for b in bases for c in b.__mro__}
        base_class = ancestors.isdisjoint(metaclass._registry):
        if base_class:
            operations = {}            
            for value in namespace.values():
                operation = getattr(value, '__operation', None)
                if (callable(value) and isinstance(operation, Operation)):
                    operations[operation.name] = (operation.commutative, value, metaclass._OPERATIONS_TYPES[operation[1:3]])
                    namespace['__%s__' % operation.name] = lambda s, o: getattr(s, '_%s' % operation.name)(o)
                    namespace['__r%s__' % operation.name] = namespace['__%s__' % operation.name] if operation.commutative else lambda s, o: getattr(s, '_r%s' % operation.name)(o)
            namespace['_OPERATIONS'] = operations
            namespace['default_implementation'] = lambda s, o: NotImplemented
        
        implementations = collections.defaultdict
        for value in namespace.values():
            implementation = getattr(value, '__implementation', None)
            if (callable(value) and isinstance(implementation, Implementation)):
                implementations[implementation.operation].append((implementation.type, value))
        namespace['_IMPLEMENTATIONS'] = implementations

        cls = super(Algebra, metaclass).__new__(name, bases, namespace)
        if base_class
            cls._BASE_TYPE = cls
            metaclass._registry.add(cls)
        return cls

    def __call__(self, *args, **kws):
        """Sets up dispatching.

        The combination of special methods always being called on the
        class, not the instance, and single-dispatch always
        dispatching on the type of the first argument requires
        multiple levels of indirection.

        1) Call a bound non-special method from the special method on
        the class.

        2) Single-dispatch from the bound non-special method to
        another bound method.

        3) The other bound method calls a static method.

        """
        for operation, (commutative, base_method, _) in self._OPERATIONS.values():
            dispatcher = _singledispatch(self.default_implementation)
            setattr(self, '_%s' % operation, dispatcher)
            dispatcher.register(self._BASE_TYPE, base_method)
            for cls, implementation in self._IMPLEMENTATIONS[operation]:
                dispatcher.register(cls, (lambda s, o: implementation(s, o)).__get__(self))

        # Found a collection type.
        if hasattr(self, '_collection') and isinstance(self._collection, Collection):
            name, operation = self._collection
            setattr(self, name, self._OPERATIONS[operation][2]())

        self.__init__(*args, **kws)


class _StructBase(six.with_metaclass(abc.ABCMeta, object)):
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
    def _add_atom(self, other):
        return type(self)(*(self._atoms + [other]))
    def _radd_seq(self, other):
        return type(self)(*(other._atoms + self._atoms))
    def _radd_atom(self, other):
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

    # __rmul__ = __mul__

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



# The pattern of an algebra is an abstract class that serves as the
# common parent for all of the specific types in the algebra and
# contains any common code.  Stripping out all the code that needs to
# be automatically generated, a simple algebra with only one
# operation, concatenation, and one atomic type looks like:

class AbstractBase(six.with_metaclass(abc.ABCMeta, object)):
    def __init__(self):
        self._op = _singledispatch(self._op)
        self._op.register(self._op_base)
        self._rop = _singledispatch(self._rop)
        self._rop.register(self._op_base)

    # Prototype for an overloaded noncommutative operation.
    def __op__(self, other):
        return self._op(other)
    def _op(self, other):
        return NotImplemented
    # @operation('op', False, True)
    @staticmethod
    def _op_base(left, right):
        return NotImplemented

    def __rop__(self, other):
        return self._rop(other)
    def _rop(self, other):
        return NotImplemented
    def _rop_base(self, other):
        return NotImplemented


class Monoid(AbstractBase):
    """A sketch of what these types of monoids look like.

    Following Fran Mota (http://fmota.eu/blog/monoids-in-python.html),
    rather than defining a monoid as a set, an identity, and an
    associative operation, I'm defining a monoid over a set of atoms
    as a collection with an operation lift which takes an atom and
    puts it in the collection, the binary operation which takes an
    atom or collection and returns an atom or a collection, and an
    identity.  Properly speaking, these things can only be said to be
    monoids with respect to both the atom and the collection because
    the operation on the atoms alone is not closed (it can return a
    collection, not just another atom).  Because there's a unique
    collection that corresponds to each atom, Monoid(atom), I can
    identify (or confuse) an atom with Monoid(atom), define the
    operation over the whole set of atoms and collections of atoms,
    and end up with a monoid for all intents and purposes.

    """
    def __init__(self, collection, lift, op):
        self.collection = collection
        self.identity = collection()
        self.op = op

    def __call__(self, *args):
        return functools.reduce(self.op, map(self.collection, args), self.identity)

    @staticmethod
    def _op_monoid(left: 'Monoid', right: 'Monoid') -> 'Monoid | NotImplemented':
        return left.op(right)

    # This needs some arg-juggling during dispatch to write in a generic way
    @staticmethod
    def _op_atom(left: 'Monoid', right: 'Atom') -> 'Monoid | NotImplemented':
        return left.op(self.collection(right))


class Atom(AbstractBase):
    @staticmethod
    def _op_atom(left: 'Atom', right: 'Atom') -> 'Atom | NotImplemented':
        if oppable(left, right):
            return left.op(right) # -> Atom
        else:
            return NotImplemented
