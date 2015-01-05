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

from keyword_only_args_decorator import keyword_only_args


import abc
import collections


"""The general pattern of these sorts of algebras is that they have
have at least two operations, concatenation and scalar mulitplication,
an underlying atom type or types, and a sequence type.  Combinators
have at least one additional operation, alternation.  Regexes may have
multiple different additional operations.  My goal here is to write a
metaclass or a class decorator that factors out the patterns in the
algebras so that I will only have to write the definitions for the
actual operations and then the decorator/metaclass will define all the
other operations automagically.

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


# Rough model of an abstract algebra class, simply factoring out the
# common code.

class Algebra(six.with_metaclass(abc.ABCMeta, object)):

    def __init__(self, *subtypes):
        self._check_types(subtypes)
        self._
        subtypes = iter(subtypes)
        self._subtypes = [next(subtypes)]
        for subtype in subtypes:
            self._push_subtype(subtype)
