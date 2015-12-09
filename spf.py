#!/usr/bin/python3

from __future__ import absolute_import, print_function

import collections
import functools
import itertools
import numbers
import platform
import pprint
import weakref

CPYTHON = platform.python_implementation() == 'CPython'


class LabeledForest(dict):
    '''A map-of-lists digraph representation of a forest.'''
    __slots__ = ()

    # The keys are labels for nodes in the forest, and the values are
    # the nodes themselves.  Individual trees are represented by nested
    # singly-linked lists, and the natural structure of singly-linked
    # lists ensures that no node has out-degree of more than two.  Trees
    # share nodes through linked-list tail sharing.  Or nodes
    # represent ambiguities in the forest and are represented by builtin
    # lists because the out-degree of a packed node can be unlimited.
    # The map is a weak dictionary so that when the forest is
    # incrementally constructed, nodes that don't end up connected to
    # the eventual root will be garbage-collected.  Nodes contain strong
    # references to other nodes and the forest as a whole contains a
    # strong reference to the root node to anchor the entire structure.

    # '''
    # def __init__(self, root=None, *args, **kws):
    #     self.root = root
    #     super(LabeledForest, self).__init__(*args, **kws)

    def __str__(self):
        # '''Using WeakValueDictionary's __repr__ creates infinite recursion in
        # pprint, so I still have to use .items() to avoid it.

        # '''
        return pprint.pformat(dict(self.items()), width=80)

    def __repr__(self):
        return '%s(%s)' % (type(self).__name__, pprint.pformat(dict(self.items()), width=80))

    # def is_tree(self):
    #     '''If this SPF is a tree (has no or nodes), returns True.'''
    #     if any(isinstance(n, OrForest) for n in self.values()):
    #         return False
    #     else:
    #         return True



# if CPYTHON:
#     import pyximport; pyximport.install()
#     from _node import _BaseForest
# else:
#     class _BaseForest(tuple):
#         def __new__(cls, child, sibling):
#             return super(_BaseForest, cls).__new__(cls, (child, sibling))
#         @property
#         def _child(self):
#             return super(_BaseForest, self).__getitem__(0)
#         @property
#         def _sibling(self):
#             return super(_BaseForest, self).__getitem__(1)


class BaseForest(tuple):
    '''Abstract parent class for SharedPackedForest nodes.'''
    # At the moment this class inherits from tuple because all node
    # implementations are based on tuple.  This may change depending
    # on future testing.
    __slots__ = ()

    # def leaves(self):
    #     '''Traversal of this SPF's leaves.'''
    #     to_visit = [self.root]
    #     while to_visit:
    #         node = to_visit.pop()
    #         if isinstance(node, OrForest):
    #             to_visit.append(node[0])
    #         elif isinstance(node, BaseForest):
    #             to_visit.extend(node.reverse_values())
    #         else:
    #             yield node

    def trees(self):
        '''Iterate over the trees in this forest.

        This algorithm uses two stacks, to_visit to keep track of
        ordinary nodes and or_nodes to keep track of places where the
        SPF branches into multiple trees.  Over ordinary nodes, the
        algorithm is identical to preorder tree traversal.  When it
        encounters a or node, it saves the current state of the
        traversal by pushing the or node, an iterator over the
        nodes in the or node, a copy of the current state of the
        stack of ordinary nodes (to_visit), and a record of the
        choices made at previous or nodes onto the or_nodes stack.
        It then proceeds along the first possible choice in the or
        node.  This record, called or_choices, is a dictionary
        that's designed to shadow all the or nodes in the SPF that
        occur in one particular tree.  After it's completed a full
        ordinary tree traversal (to_visit is empty), it yields a
        TreeView for that tree and then backtracks to the last place
        it made a choice at a or node, reading the state off the
        top of the or_nodes stack.  Once it's checked all possible
        choices at all or nodes (or_nodes is empty), it
        terminates.  Looking at the stack of stacks, it should be
        obvious why in the case of highly-ambiguous grammars it will
        become unbounded polynomial, and I'm concerned it may actually
        be exponential in the number of or nodes.  It also won't
        terminate in the case of an SPF with cycles.

        '''
        or_nodes = []
        or_choices = {}
        to_visit = [self]
        while True:
            while to_visit:
                node = to_visit.pop()
                while isinstance(node, OrForest):
                    nodes = iter(node)
                    or_node = node
                    or_nodes.append((or_node, nodes, to_visit[:], or_choices.copy()))
                    node = next(nodes)
                    or_choices[or_node] = node
                if isinstance(node, BaseForest):
                    to_visit.extend(node.reverse_values())
            node = self
            while isinstance(node, OrForest):
                node = or_choices[node]
            if isinstance(node, BaseForest):
                yield TreeView(self, or_choices)
            else:
                yield node
            while or_nodes:
                or_node, nodes, to_visit, or_choices = or_nodes[-1]
                try:
                    node = next(nodes)
                    or_choices[or_node] = node
                    to_visit.append(node)
                    break
                except StopIteration:
                    or_nodes.pop()
            else:
                return


@functools.total_ordering
class Forest(BaseForest):
    '''Parse tree node for unstructured data that implements (most of)
    the immutable sequence ABC.

    This is implemented as a singly-linked list with the attendant
    asymptotic performance.

    '''
    __slots__ = ()

    def __new__(cls, *values):
        new = super(Forest, cls).__new__
        tail = new(cls)
        for value in reversed(values):
            tail = new(cls, (value, tail))
        return tail

    def cons(self, value):
        return super(Forest, self).__new__(type(self), (value, self))

    def concatenate(self, iterable):
        tail = self
        for value in reversed(list(iterable)):
            tail = super(Forest, self).__new__(type(self), (value, tail))
        return tail

    def tail(self):
        return super(Forest, type(self)).__getitem__(self, 1)

    def __iter__(self):
        getitem = super(Forest, type(self)).__getitem__
        node = self
        while node:
            yield getitem(node, 0)
            node = getitem(node, 1)

    def __reversed__(self):
        for n in reversed(list(self)):
            yield n
    reverse_values = __reversed__
    
    # TODO: arguably this should allow slicing and/or negative indices
    def __getitem__(self, index):
        for i, v in enumerate(self):
            if i == index:
                return v
        raise IndexError

    def __len__(self):
        return sum(1 for _ in self)

    def __bool__(self):
        if super(Forest, type(self)).__len__(self):
            return True
        else:
            return False
    __nonzero__ = __bool__

    def __contains__(self, value):
        return any(value == v for v in self)

    def __add__(self, other):
        if not isinstance(other, type(self)):
            return NotImplemented
        else:
            return other.concatenate(self)

    def __radd__(self, other):
        if not isinstance(other, type(self)):
            return NotImplemented
        else:
            return self.concatenate(other)

    def __mul__(self, other):
        if not isinstance(other, numbers.Integral):
            return NotImplemented
        else:
            return type(self)(*itertools.chain.from_iterable(itertools.repeat(self, other)))
    __rmul__ = __mul__

    def __repr__(self):
        return 'Forest(' + ', '.join(repr(v) for v in self) + ')'

    def count(self, value):
        return sum(value == v for v in self)

    # TODO: arguably this should support negative indices and optional
    # arguments
    def index(self, value):
        for i, v in enumerate(self):
            if value == v:
                return i
        raise ValueError

    def __lt__(self, other):
        return all(v1 < v2 for v1, v2 in zip(self, other))


class NamedForest(BaseForest, collections.Mapping):
    '''Parse tree node for structured data that implements the immutable
    mapping ABC.

    Like Forest, this is implemented as an immutable linked list.
    '''
    # This has the same problem as Construct's implementation of its
    # associative data structures: the key is stored in each instance,
    # which wastes a lot of memory.  It should probably be fixed with
    # maps:
    # http://morepypy.blogspot.com/2010/11/efficiently-implementing-python-objects.html
    # . Since NamedForest is an API convenience rather than fundamental,
    # though, fixing it is a low priority.
    __slots__ = ()
    
    def __new__(cls, iterable=()):
        new = super(NamedForest, cls).__new__
        tail = new(cls)
        for key, value in reversed(list(iterable)):
            tail = new(cls, (key, value, tail))
        return tail

    def tail(self):
        return super(NamedForest, type(self)).__getitem__(self, 2)

    def __iter__(self):
        getitem = super(NamedForest, type(self)).__getitem__
        node = self
        while node:
            yield getitem(node, 0)
            node = getitem(node, 2)

    def __reversed__(self):
        for n in reversed(list(self)):
            yield n

    def reverse_values(self):
        for v in self.values():
            yield v

    def __len__(self):
        return sum(1 for _ in self)

    def __bool__(self):
        if super(NamedForest, type(self)).__len__(self):
            return True
        else:
            return False
    __nonzero__ = __bool__

    def __getitem__(self, key):
        getitem = super(NamedForest, type(self)).__getitem__
        node = self
        while node:
            if key == getitem(node, 0):
                return getitem(node, 1)
            node = getitem(node, 2)
        raise KeyError(key)

    def __repr__(self):
        return 'NamedForest([' + ', '.join(repr((k, v)) for k, v in self.items()) + '])'

    def __lt__(self, other):
        return NotImplemented
    __gt__ = __lt__
    __ge__ = __lt__
    __le__ = __lt__
    __add__ = __lt__
    __radd__ = __lt__
    __mul__ = __lt__
    __rmul__ = __lt__


class OrForest(BaseForest):
    '''OrForests represent multiple derivations in a parse forest.'''
    __slots__ = ()


class TreeView(collections.Sized, collections.Iterable, collections.Container):
    '''For __len__ and __contains__, we have to traverse the tree because
    there may be nodes unreachable from the root in _nodes.  The root
    node itself should never be a or node.

    '''
    __slots__ = ('root', '_or_choices')

    def __init__(self, root, or_choices=None):
        self._or_choices = {} if or_choices is None else or_choices
        while isinstance(root, OrForest):
            root = or_choices[root]
        self.root = root

    def __len__(self):
        return sum(1 for i in self.leaves())

    def __contains__(self, item):
        return any(item == i for i in self.leaves())

    def __iter__(self):
        '''Iterates over subtrees.'''
        for i in self.root:
            if isinstance(i, OrForest):
                yield TreeView(i, self._or_choices)
            elif isinstance(i, BaseForest):
                yield TreeView(i)
            else:
                yield i

    def leaves(self):
        '''Iterator over this tree's leaves.

        Returns the objects the tree contains as a flat iterator.
        Preorder and postorder traversal will return the leaves in the
        same order, and preorder is simpler, so I use it here.

        '''
        to_visit = list(self.root.reverse_values())
        while to_visit:
            node = to_visit.pop()
            if isinstance(node, OrForest):
                to_visit.append(self._or_choices[node])
            elif isinstance(node, BaseForest):
                to_visit.extend(node.reverse_values())
            else:
                yield node

    def __getitem__(self, key):
        item = self.root[key]
        while isinstance(item, OrForest):
            item = self._or_choices[item]
        if isinstance(item, BaseForest):
            return TreeView(item, self._or_choices)
        else:
            return item

    def __len__(self):
        return sum(1 for n in self.leaves())

    def __contains__(self, item):
        return any(item == n for n in self.leaves())

    def __str__(self):
        '''Because this is for debugging/output, efficiency doesn't matter so
        I use the much simpler recursive implementation for postorder
        traversing the tree and building a nested representation.

        '''
        def nested(root):
            if isinstance(root, OrForest):
                return nested(self._or_choices[root])
            elif isinstance(root, Forest):
                return [nested(n) for n in root]
            elif isinstance(root, NamedForest):
                return {k: nested(v) for k, v in root.items()}
            else:
                return root
        return pprint.pformat(nested(self.root))

    def __getitem__(self, key):
        item = self.root[key]
        if isinstance(item, BaseForest):
            return TreeView(item)
        else:
            return item


if __name__ == '__main__':
    # Only CPython has sys.getsizeof().
    if CPYTHON:
        import hettinger_total_size

    print(Forest())
    print(Forest(0))
    print(Forest(0, 1, 2))

    n = Forest(1, 2, 3, 4)
    print('Cons, 0:', n.cons(0))
    print('Cons onto empty:', Forest().cons(0))
    print('Iterator:', list(n))
    print('Reverse iterator:', list(reversed(n)))
    print('Get item, 1 and 4:', n[0], n[3])
    print('Length:', len(n))
    print('Add:', n + Forest(5, 6))
    print('Add:', Forest(0) + n)
    print('Left multiply:', 3*n)
    print('Right multiply:', n*3)
    print('Contains, False:', 0 in n)
    print('Contains, True:', 3 in n)
    print('Count 4s, 1:', n.count(4))
    print('Equality, True:', n == n)
    print('Inequality, False:', n != n)
    print('Less than, True:', Forest(0, 1, 2) < n)
    print('Greater than, False:', n > Forest(1, 2, 3, 5))

    if CPYTHON:
        total_size = functools.partial(hettinger_total_size.total_size, handlers = {Forest: lambda n: iter((n, tuple.__getitem__(n, 0), tuple.__getitem__(n, 1))) if n else n, NamedForest: lambda n: iter((n, tuple.__getitem__(n, 0), tuple.__getitem__(n, 1), tuple.__getitem__(n, 2))) if n else n})

        print('Memory, 4-element tuple versus Forest:', total_size((1, 2, 3, 4)), total_size(n))
        print('Memory, 100-element tuple versus Forest:', total_size(tuple(range(100))), total_size(Forest(*range(100))))

    print(NamedForest())
    print(NamedForest([('a', 0)]))
    m = NamedForest([('a', 0), ('b', 1), ('c', 1)])
    print(m)

    print('Iterator:', list(m))
    print('Reverse iterator:', list(reversed(m)))
    print('Get item, 0 and 1:', m['a'], m['b'])
    print('.get(), 1:', m.get('c', None))
    print('.get(), None:', m.get('d', None))
    print('Length:', len(m))
    print('Keys:', list(m.keys()))
    print('Values:', list(m.values()))
    print('Items:', list(m.items()))
    print('Contains, False:', None in m)
    print('Contains, True:', 'a' in m)
    print('Equality, True:', m == m)
    print('Inequality, False:', m != m)
    try:
        m < m
    except TypeError as error:
        print('Less than:', error)
    try:
        m > m
    except TypeError as error:
        print('Greater than:', error)

    if CPYTHON:
        print('Memory, 3-element dict versus NamedForest:', total_size({'a': 1, 'b': 2, 'c': 3}), total_size(m))
        print('Memory, 100-element dict versus NamedForest:', total_size(dict(zip(range(100), range(100)))), total_size(NamedForest(zip(range(100), range(100)))))

    # total_size = functools.partial(hettinger_total_size.total_size, handlers = {LabeledForest: lambda t: itertools.chain((t.root,), itertools.chain.from_iterable(t._nodes.items())), Forest: lambda l: iter(l)})

    #       /             \
    #     /   \       /   |   \
    #    'a' /  \    'd' 'e'  'f'
    #       'b' 'c'

    #                       (0, 21)
    #              /                             \
    #           (0, 3)                         (3, 21)
    #        /          \                /        |         \
    #      (0, 1)     (1, 3)         (3, 15)   (15, 17)   (17, 21)
    #        |       /      \           |         |          |
    #       'a'   (1, 2)   (2, 3)      'd'       'e'        'f'
    #               |        |
    #              'b'      'c'

    def make_tree(spf, seqnode, mapnode):
        a = 'a'
        b = 'b'
        c = 'c'
        d = 'd'
        e = 'e'
        f = 'f'
        t0 = seqnode(b, c)
        t1 = seqnode(a, t0)
        t2 = mapnode([('foo', d), ('bar', e), ('baz', f)])
        t3 = seqnode(t1, t2)
        return spf({
            (0, 1): a,
            (1, 2): b,
            (2, 3): c,
            (3, 15): d,
            (15, 17): e,
            (17, 21): f,
            (0, 21): t3,
            (0, 3): t1,
            (1, 3): t0,
            (3, 21): t2,
            (15, 21): t2.tail()})

    tree = make_tree(LabeledForest, Forest, NamedForest)
    print('Forest tests.')
    print(tree)
    root = tree[(0, 21)]
    print('%s, length %s' % (root, len(root)))
    print('Iterate over subtrees.')
    for i in root:
        print(i)
    # print('Leaves.')
    # for i in root.leaves():
    #     print(i)
    # print('Contains: "a", %s; 0, %s' % ('a' in tree, 0 in tree))

    tree_view = next(root.trees())[1]
    print('tree[1]: %s; tree[1]["bar"]: %s' % (tree_view, tree_view['bar']))
    print('TreeView tests.')
    print('%s, length %s' % (tree_view.root, len(tree_view)))    
    print('Leaves.')
    for i in tree_view.leaves():
        print(i)
    print('Contains: "a", %s; "f", %s' % ('a' in tree_view, 'f' in tree_view))
    if CPYTHON:
        print('Input, memory in bytes:', total_size('abcdef'))
        print('Forest with labels, memory:', total_size(tree))
        print('Forest without labels, memory:', total_size(root))

    def make_ambiguous_trees(spf, node):
        '''Grammar: S -> AB, S -> SC, B -> BC, A -> a, B -> b, C -> c

        S -> aB | Sc
        B -> Bc

        Input: 'abcc'

        These trees already share a subtree.

        '''
        a = 'a'
        b = 'b'
        c0 = 'c'
        c1 = 'd'
        bc = node(b, c0)
        ab = node(a, b)
        bbc = node(bc, c1)
        n0 = node(a, bbc)
        abc0 = node(a, bc)
        n1 = node(abc0, c1)
        abc1 = node(ab, c0)
        n2 = node(abc1, c1)
        leaves =  {(0, 1): a,
                   (1, 2): b,
                   (2, 3): c0,
                   (3, 4): c1
        }
        t0 = {(0, 4): n0,
              (1, 4): bbc,
              (1, 3): bc}
        t0.update(leaves)
        t1 = {(0, 4): n1,
              (0, 3): abc0,
              (1, 3): bc}
        t1.update(leaves)
        t2 = {(0, 4): n2,
              (0, 3): abc1,
              (0, 2): ab}
        t2.update(leaves)
        return [spf(t0), spf(t1), spf(t2)]

    ambiguous_trees = make_ambiguous_trees(LabeledForest, Forest)
    pprint.pprint(ambiguous_trees)
    if CPYTHON:
        print('Ambiguous trees, memory:', total_size(ambiguous_trees))
        print('Ambiguous trees without labels, memory:', total_size([t[(0, 4)] for t in ambiguous_trees]))

    def make_spf(node):
        a = 'a'
        b = 'b'
        c0 = 'c'
        c1 = 'c'
        bc = node(b, c0)
        ab = node(a, b)
        bbc = node(bc, c1)
        n0 = node(a, bbc)
        abc0 = node(a, bc)
        n1 = node(abc0, c1)
        abc1 = node(ab, c0)
        abc = OrForest([abc0, abc1])
        abcc = OrForest([node(a, bbc), node(abc, c1)])
        nodes = {(0, 1): a,
                 (1, 2): b,
                 (2, 3): c0,
                 (3, 4): c1,
                 (0, 2): ab,
                 (1, 3): bc,
                 (0, 3): abc,
                 (1, 4): bbc,
                 (0, 4): abcc}
        return LabeledForest(nodes)

    spf = make_spf(Forest)
    print('SPF tests.')
    print(spf)
    root = spf[(0, 4)]
    print('%s, length %s' % (root, len(root)))
    # print('Leaves.')
    # for i in spf.leaves():
    #     print(i)
    # print('Is this a tree?', spf.istree())
    for t in root.trees():
        print('Tree:', t)
        for s in t:
            print('Subtree:', s)
        for i in t.leaves():
            print('Leaf:', i)

    if CPYTHON:
        print('Input, memory in bytes:', total_size('abcc'))
        print('SPF with labels, memory:', total_size(spf))
        print('SPF without labels, memory:', total_size(root))
