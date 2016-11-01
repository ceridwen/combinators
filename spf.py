from __future__ import absolute_import, division, print_function#, unicode_literals

import collections
# A bug in pprint on 2.7 prevents it from working correctly with
# frozensets: http://bugs.python.org/issue20192.
import pprint
import weakref

try:
    from functools import singledispatch as _singledispatch
except ImportError:
    from singledispatch import singledispatch as _singledispatch


class Tree(collections.Sized, collections.Iterable, collections.Container):
    """A dict-of-nodes digraph representation of a tree.

    _nodes is a weak dictionary mapping labels to node objects, a weak
    dictionary so that combinators can add nodes to the dictionary in
    parse branches that eventually fail and have those nodes
    garbage-collected.  Node objects contain strong references to
    other node objects, and self.root is a strong reference to the
    root node to anchor the entire tree.  Trees shouldn't contain
    packed nodes or nodes with more than one parent (more than one
    strong reference from another node), but shared packed forests
    can.  The references to packed nodes in Tree methods are intended
    for TreeViews, which inherit the methods.

    """
    __slots__ = ('root', '_nodes')

    def __init__(self, root=None, nodes=None):
        self.root = root
        self._nodes = weakref.WeakValueDictionary() if nodes is None else nodes

    def __getitem__(self, key):
        item = self.root[key]
        if isinstance(item, Node):
            return TreeView(item, self._nodes)
        else:
            return item

    def __len__(self):
        return sum(1 for i in self.leaves())

    def __contains__(self, item):
        return any(item == i for i in self.leaves())

    def __iter__(self):
        """Iterates over subtrees."""
        for i in self.root:
            if isinstance(i, PackedNode):
                yield TreeView(i, self._nodes, self._unpacked_nodes)
            elif isinstance(i, Node):
                yield TreeView(i, self._nodes)
            else:
                yield i

    def leaves(self):
        """Iterator over this tree's leaves.

        Returns the objects the tree contains as a flat iterator.
        Preorder and postorder traversal will return the leaves in the
        same order, and preorder is simpler, so I use it here.

        """
        to_visit = list(reversed(self.root))
        while to_visit:
            node = to_visit.pop()
            if isinstance(node, PackedNode):
                to_visit.append(self._unpacked_nodes[node])
            elif isinstance(node, Node):
                to_visit.extend(reversed(node))
            else:
                yield node

    def __str__(self):
        """Using WeakValueDictionary's __repr__ creates infinite recursion in
        pprint, so I still have to use .items() to avoid it.

        """
        return pprint.pformat(dict(self._nodes.items()), width=80)

    def __repr__(self):
        return '%s(%s)' % (type(self).__name__, pprint.pformat(dict(self._nodes.items()), width=80))


class SharedPackedForest(Tree):
    """The only differences between the digraph representation of a tree
    and of an SPF is that a tree iterates over its leaves while an
    SPF iterates over its trees, and SPFs can contain packed nodes
    and nodes with more than one parent.

    """
    __slots__ = ()

    def __iter__(self):
        return self.trees(self.root)

    def trees(self, root):
        """This algorithm uses two stacks, to_visit to keep track of ordinary
        nodes and packed_nodes to keep track of places where the SPF
        branches into multiple trees.  Over ordinary nodes, the
        algorithm is identical to preorder tree traversal.  When it
        encounters a packed node, it saves the current state of the
        traversal by pushing the packed node, an iterator over the
        nodes in the packed node, a copy of the current state of the
        stack of ordinary nodes (to_visit), and a record of the
        choices made at previous packed nodes onto the packed_nodes
        stack.  It then proceeds along the first possible choice in
        the packed node.  This record, called unpacked_nodes, is a
        dictionary that's designed to shadow all the packed nodes in
        the SPF that occur in one particular tree.  After it's
        completed a full ordinary tree traversal (to_visit is empty),
        it yields a TreeView for that tree and then backtracks to the
        last place it made a choice at a packed node, reading the
        state off the top of the packed_nodes stack.  Once it's
        checked all possible choices at all packed nodes (packed_nodes
        is empty), it terminates.  Looking at the stack of stacks, it
        should be obvious why in the case of highly-ambiguous grammars
        it will become unbounded polynomial, and I'm concerned it may
        actually be exponential in the number of packed nodes.  It
        also won't terminate in the case of an SPF with cycles.

        """
        packed_nodes = []
        unpacked_nodes = weakref.WeakValueDictionary()
        to_visit = [root]
        while True:
            while to_visit:
                node = to_visit.pop()
                while isinstance(node, PackedNode):
                    nodes = iter(node)
                    packed_node = node
                    packed_nodes.append((packed_node, nodes, to_visit[:], unpacked_nodes.copy()))
                    node = next(nodes)
                    unpacked_nodes[packed_node] = node
                if isinstance(node, Node):
                    to_visit.extend(reversed(node))
            node = root
            while isinstance(node, PackedNode):
                node = unpacked_nodes[node]
            if isinstance(node, Node):
                yield TreeView(root, self._nodes, unpacked_nodes)
            else:
                yield node
            while packed_nodes:
                packed_node, nodes, to_visit, unpacked_nodes = packed_nodes[-1]
                try:
                    node = next(nodes)
                    unpacked_nodes[packed_node] = node
                    to_visit.append(node)
                    break
                except StopIteration:
                    packed_nodes.pop()
            else:
                return

    def leaves(self):
        """Traversal of this SPF's leaves."""
        to_visit = [self.root]
        while to_visit:
            node = to_visit.pop()
            if isinstance(node, PackedNode):
                to_visit.append(next(iter(node)))
            elif isinstance(node, Node):
                to_visit.extend(reversed(node))
            else:
                yield node

    def istree(self):
        """If this SPF is a tree (has no packed nodes), returns True."""
        if any(isinstance(n, PackedNode) for n in self._nodes.values()):
            return False
        else:
            return True


class TreeView(Tree):
    """For __len__ and __contains__, we have to traverse the tree because
    there may be nodes unreachable from the root in _nodes.  The root
    node itself should never be a packed node.

    """
    __slots__ = ('_unpacked_nodes')

    def __init__(self, root, nodes, unpacked_nodes=None):
        self._unpacked_nodes = weakref.WeakValueDictionary() if unpacked_nodes is None else unpacked_nodes
        while isinstance(root, PackedNode):
            root = unpacked_nodes[root]
        self.root = root
        self._nodes = nodes

    def __getitem__(self, key):
        item = self.root[key]
        while isinstance(item, PackedNode):
            item = self._unpacked_nodes[item]
        if isinstance(item, Node):
            return TreeView(item, self._nodes, self._unpacked_nodes)
        else:
            return item

    def __len__(self):
        return sum(1 for n in self.leaves())

    def __contains__(self, item):
        return any(item == n for n in self.leaves())

    def __str__(self):
        """Because this is for debugging/output, efficiency doesn't matter so
        I use the much simpler recursive implementation for postorder
        traversing the tree and building a nested representation.

        """
        def nested(root):
            if isinstance(root, PackedNode):
                return nested(self._unpacked_nodes[root])
            elif isinstance(root, SeqNode):
                return [nested(n) for n in root]
            elif isinstance(root, _MapNode):
                return {k: nested(v) for k, v in root.items()}
            else:
                return root
        return pprint.pformat(nested(self.root))


class Node(object):
    """Abstract parent class for nodes.

    All nodes have to be weak-referencable, and PackedNodes must be
    both weak-referencable and hashable---and since PackedNodes
    contain other nodes, other nodes also have to be hashable.  As the
    only built-in Python type with both properties is frozenset and I
    need types with intrinsic order, I have to make my own.
    Unfortunately, it's impossible to set _hash and __weakref__ on
    Node, for some reason, they don't inherit correctly.

    """
    __slots__ = ()


class SeqNode(list, Node):
    """Tree node for unstructured data.

    This would be a tuple subclass except that subclasses of tuple
    can't be weak-referenced.  Unfortunately, this means it claims to
    be a MutableSequence even though I've overridden the methods that
    mutate it.  The other possible solution is creating some kind of
    tuple proxy class, but it's substantially slower to create all
    possible tuple proxies (more than an order of magnitude slower in
    CPython, slower in PyPy too thought not as slow), basic operations
    like __getitem__ are slower too, and the class adds more memory
    overhead.  While I might investigate wrapt's transparent object
    proxy implemented in C, probably I will need to write a
    weak-referencable tuple-like type in Cython, to change the
    underlying tree representation, or both.

    """
    __slots__ = ('_hash', '__weakref__')

    def __hash__(self):
        if not hasattr(self, '_hash'):
            self._hash = hash(tuple(self))
        return self._hash

    def __setitem__(self, *args, **kws):
        raise TypeError("'%s' object does not support item assignment" % type(self))

    def __delitem__(self, *args, **kws):
        raise TypeError("'%s' object does not support item deletion" % type(self))

    def __immutable(self, *args, **kws):
        raise TypeError("'%s' object is immutable" % type(self))

    append = __immutable
    clear = __immutable
    extend = __immutable
    insert = __immutable
    pop = __immutable
    remove = __immutable
    reverse = __immutable
    sort = __immutable
    __iadd__ = __immutable
    __imul__ = __immutable


class _MapNode(Node):
    """Tree node that maps names to objects for structured data.

    This is another work-around for the inability of tuples, including
    namedtuples, to be weak-referenced.  A hash table has memory
    overhead that's too high for the small numbers of objects that
    often occur in parse tree nodes of structured data.  Instead, this
    class acts something like a namedtuple or C struct, with a fixed
    set of names defined in __slots__.  While it implements most of
    the mapping interface, it is not a proper mapping because it
    iterates (and reverse iterates) over values, not keys.  Note that
    attribute access by map_node.a is faster in CPython than access
    through __getitem__, map_node['a'].

    """
    __slots__ = ('_hash', '__weakref__')

    def __init__(self, iterable):
        if len(iterable) < self._length:
            raise TypeError('Expected %d arguments, got %d' % (len(self.__slots__), len(iterable)))
        for key, value in zip(self.__slots__, iterable):
            super(_MapNode, self).__setattr__(key, value)

    def __setattr__(self, key, value):
        raise AttributeError('%s object does not support item assignment.' % type(self).__name__)

    def __delattr__(self, key):
        raise AttributeError('%s object does not support item deletion.' % type(self).__name__)

    def __getitem__(self, key):
        try:
            if key in self.__slots__:
                return getattr(self, key)
            else:
                raise KeyError(key)
        except AttributeError:
            raise KeyError(key)

    def __len__(self):
        return len(self.__slots__)

    def __iter__(self):
        for key in self.__slots__:
            yield getattr(self, key)

    def __reversed__(self):
        for key in reversed(self.__slots__):
            yield getattr(self, key)

    def __contains__(self, key):
        return key in self.__slots__

    def get(self, key, default = None):
        if key in self.__slots__:
            return getattr(self, key, default)
        else:
            return default

    def copy(self):
        return type(self)(self)

    def __hash__(self):
        if not hasattr(self, '_hash'):
            super(_MapNode, self).__setattr__('_hash', hash(frozenset(self.items()))) 
        return self._hash

    def __eq__(self, other):
        if not isinstance(other, collections.Mapping):
            return NotImplemented
        return dict(self.items()) == dict(other.items())

    def __ne__(self, other):
        return not (self == other)

    def __repr__(self):
        return pprint.pformat(dict(self.items()))

    def keys(self):
        return self.KeysView(self)
    def items(self):
        return self.ItemsView(self)
    def values(self):
        return self.ValuesView(self)

    class MappingView:
        """For some reason the View objects in collections don't define
        __slots__, so I have to redefine them here.  Given this, I
        specialize these views to _MapNode.  In 2.7, collections.Set
        doesn't have __slots__ so KeyView and ItemView will be less
        memory efficient there, but it's not worth reimplementing all
        the set functions to avoid that problem.

        """
        __slots__ = ('_mapping')
        def __init__(self, mapping):
            self._mapping = mapping
        def __len__(self):
            return self._mapping._length
        def __repr__(self):
            return '{0.__class__.__name__}({0._mapping!r})'.format(self)

    class KeysView(MappingView, collections.Set):
        __slots__ = ()
        def __contains__(self, key):
            return key in self._mapping.__slots__
        def __iter__(self):
            for key in self._mapping.__slots__:
                yield key

    class ItemsView(MappingView, collections.Set):
        __slots__ = ()
        def __contains__(self, item):
            key, value = item
            if key in self._mapping.__slots__ and getattr(self._mapping, key) == value:
                return True
            else:
                return False
        def __iter__(self):
            for key in self._mapping.__slots__:
                yield (key, getattr(self._mapping, key))

    class ValuesView(MappingView):
        __slots__ = ()
        def __contains__(self, value):
            for key in self._mapping.__slots__:
                if getattr(self._mapping, key) == value:
                    return True
            return False
        def __iter__(self):
            for key in self._mapping.__slots__:
                yield getattr(self._mapping, key)

def MapNode(names, name=None):
    name = 'MapNode_%s' % '_'.join(names) if not name else name
    return type(name, (_MapNode,), {'__slots__': names, '_length': len(names)})


class PackedNode(frozenset, Node):
    """PackedNodes hold other nodes, including in some cases PackedNodes.

    Inheriting from frozenset prevents PackedNodes from duplicating
    nodes but costs memory and means that nodes are returned in an
    arbitrary order.  It may be better to use a list instead.
    (Alternately, I should check and see if Hettinger ever actually
    added memory-efficient hash tables to set.)  Note that frozensets
    can be weak-referenced so I don't need to add __weakref__ to
    slots.  This is a candidate for replacement with Hettinger's
    more memory-efficient hash table implementation.

    """
    __slots__ = ()


if __name__ == '__main__':
    # PyPy doesn't have sys.getsizeof().
    import platform
    pypy = True if platform.python_implementation() == 'PyPy' else False
    if not pypy:
        import hettinger_total_size
        import functools
        import itertools

        total_size = functools.partial(hettinger_total_size.total_size, handlers = {Tree: lambda t: itertools.chain((t.root,), itertools.chain.from_iterable(t._nodes.items()))})

    #                       (0, 21)
    #              /                             \
    #           (0, 3)                         (3, 21)
    #        /          \                /        |         \
    #      (0, 1)     (1, 3)         (3, 15)   (15, 17)   (17, 21)
    #        |       /      \           |         |          |
    #       'a'   (1, 2)   (2, 3)      'd'       'e'        'f'
    #               |        |
    #              'b'      'c'

    def make_tree(tree, seqnode, mapnode):
        a = 'a'
        b = 'b'
        c = 'c'
        d = 'd'
        e = 'e'
        f = 'f'
        t0 = seqnode([b, c])
        t1 = seqnode([a, t0])
        t2 = mapnode([d, e, f])
        t3 = seqnode([t1, t2])
        nodes = {# (0, 1): a,
                 # (1, 2): b,
                 # (2, 3): c,
                 # (3, 15): d,
                 # (15, 17): e,
                 # (17, 21): f,
                 (0, 21): t3,
                 (0, 3): t1,
                 (1, 3): t0,
                 (3, 21): t2}
        return Tree(t3, weakref.WeakValueDictionary(nodes))

    tree = make_tree(Tree, SeqNode, MapNode(['eeny', 'meeny', 'miny']))
    print('Tree tests.')
    print(tree)
    print('Root %s, length %s' % (tree.root, len(tree)))
    print('Iterate over subtrees.')
    for i in tree:
        print(i)
    print('Leaves.')
    for i in tree.leaves():
        print(i)
    print('Contains: "a", %s; 0, %s' % ('a' in tree, 0 in tree))

    tree_view = tree[1]
    print('tree[1]: %s; tree[1]["eeny"]: %s' % (tree_view, tree_view['eeny']))
    print('Tree view tests.')
    print('Root %s, length %s' % (tree_view.root, len(tree_view)))    
    print('Iterate over subtrees.')
    for i in tree_view:
        print(i)
    print('Leaves.')
    for i in tree_view.leaves():
        print(i)
    print('Contains: "a", %s; "f", %s' % ('a' in tree_view, 'f' in tree_view))
    if not pypy:
        print('Input, memory in bytes:', total_size('abcdef'))
        print('Tree, memory:', total_size(tree))

    def make_ambiguous_trees(tree, node):
        """Grammar: S -> AB, S -> SC, B -> BC, A -> a, B -> b, C -> c

        S -> aB | Sc
        B -> Bc

        Input: 'abcc'

        These trees already share a subtree.

        """
        a = 'a'
        b = 'b'
        c0 = 'c'
        c1 = 'd'
        bc = node([b, c0])
        ab = node([a, b])
        bbc = node([bc, c1])
        n0 = node([a, bbc])
        abc0 = node([a, bc])
        n1 = node([abc0, c1])
        abc1 = node([ab, c0])
        n2 = node([abc1, c1])
        leaves =  {# (0, 1): a,
                   # (1, 2): b,
                   # (2, 3): c0,
                   # (3, 4): c1
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
        return [tree(n0, weakref.WeakValueDictionary(t0)),
                tree(n1, weakref.WeakValueDictionary(t1)),
                tree(n2, weakref.WeakValueDictionary(t2))]

    ambiguous_trees = make_ambiguous_trees(Tree, SeqNode)
    pprint.pprint(ambiguous_trees)
    if not pypy:
        print('Ambiguous trees, memory:', total_size(ambiguous_trees))

    def make_spf(node):
        a = 'a'
        b = 'b'
        c0 = 'c'
        c1 = 'c'
        bc = node([b, c0])
        ab = node([a, b])
        bbc = node([bc, c1])
        n0 = node([a, bbc])
        abc0 = node([a, bc])
        n1 = node([abc0, c1])
        abc1 = node([ab, c0])
        abc = PackedNode([abc0, abc1])
        abcc = PackedNode([node([a, bbc]), node([abc, c1])])
        nodes = {# (0, 1): a,
                 # (1, 2): b,
                 # (2, 3): c0,
                 # (3, 4): c1,
                 (0, 2): ab,
                 (1, 3): bc,
                 (0, 3): abc,
                 (1, 4): bbc,
                 (0, 4): abcc}
        return SharedPackedForest(abcc, weakref.WeakValueDictionary(nodes))

    spf = make_spf(SeqNode)
    print('SPF tests.')
    print(spf)
    print('Leaves.')
    for i in spf.leaves():
        print(i)
    print('Is this a tree?', spf.istree())
    for t in spf:
        print('Tree:', t)
        for s in t:
            print('Subtree:', s)
        for i in t.leaves():
            print('Leaf:', i)

    if not pypy:
        print('Input, memory in bytes:', total_size('abcc'))
        print('SPF, memory:', total_size(spf))
