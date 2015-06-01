#!/usr/bin/python3

from __future__ import print_function

import collections
import itertools
import weakref
import pprint
try:
    from functools import singledispatch
except ImportError:
    from singledispatch import singledispatch


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
        if isinstance(item, Leaf):
            return item.value
        else:
            return TreeView(item, self._nodes)

    def __len__(self):
        return sum(1 for n in self._nodes.values() if not isinstance(n, Node))

    def __contains__(self, item):
        return any(item == n for n in self._nodes.values())

    def __iter__(self):
        """Iterates over subtrees."""
        for i in self.root:
            if isinstance(i, PackedNode):
                yield TreeView(i, self._nodes, self._unpacked_nodes)
            elif isinstance(i, Node):
                yield TreeView(i, self._nodes)
            else:
                yield i.value

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
                yield node.value

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
            # Note to self: the problem is here.  What happens is that
            # root is a PackedNode with a Leaf inside it.  The
            # iterator creates TreeView objects with the PackedNode as
            # their root, and the constructor then turns that into a
            # TreeView with only a Leaf as its root.
            node = root
            while isinstance(node, PackedNode):
                node = unpacked_nodes[node]
            if isinstance(node, Leaf):
                yield node
            else:
                yield TreeView(root, self._nodes, unpacked_nodes)
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
        visited = set()
        while to_visit:
            node = to_visit.pop()
            if isinstance(node, PackedNode):
                to_visit.extend(node)
            elif isinstance(node, Node):
                to_visit.extend(reversed(node))
            else:
                if node not in visited:
                    yield node.value
                    visited.add(node)

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
        if isinstance(item, Leaf):
            return item.value
        else:
            return TreeView(item, self._nodes, self._unpacked_nodes)

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
            elif isinstance(root, MapNode):
                return {k: nested(v) for k, v in root.items()}
            else:
                return root.value
        return pprint.pformat(nested(self.root))


class Visitor(object):
    def __init__(self):
        self.visit = singledispatch(self.visit)
        self.visit.register(Node, self.visit_node)
        self.visit.register(PackedNode, self.visit_packed_node)

    def __call__(self, tree, node = None):
        self.to_visit = [tree.root] if not node else [node]
        result = None
        while self.to_visit:
            node = self.to_visit.pop()
            result = self.visit(node, result)
        return result
            
    def visit(self, node, result):
        pass

    def visit_node(self, node, result):
        self.to_visit.extend(reversed(node))

    def visit_packed_node(self, node, result):
        self.to_visit.extend(node)


class Node(object):
    """Abstract parent class for nodes.

    All nodes have to be weak-referencable, and PackedNodes must be
    both weak-referencable and hashable---and since PackedNodes
    contain other nodes, other nodes also have to be hashable.  As the
    only built-in Python type with both properties is frozenset and I
    need types with intrinsic order, I have to make my own.
    Unfortunately, it's impossible to set __hash and __weakref__ on
    Node, for some reason, they don't inherit correctly.

    """
    __slots__ = ()


class SeqNode(list, Node):
    """ Holds nodes and other Python objects."""
    __slots__ = ('__hash', '__weakref__')

    def __hash__(self):
        if not hasattr(self, '__hash'):
            self.__hash = hash(tuple(self))
        return self.__hash

    def __setitem__(self, *args, **kws):
        raise TypeError("'%s' object does not support item assignment" % type(self))

    def __delitem__(self, *args, **kws):
        raise TypeError("'%s' object does not support item deletion" % type(self))

    def __immutable(self, *args, **kws):
        raise TypeError("'%s' object is immutable" % type(self))

    append = __immutable
    clear = __immutable
    copy = __immutable
    extend = __immutable
    insert = __immutable
    pop = __immutable
    remove = __immutable
    reverse = __immutable


class PackedNode(frozenset, Node):
    """PackedNodes hold other nodes, including in some cases PackedNodes.

    Inheriting from frozenset prevents PackedNodes from duplicating
    nodes but costs memory and means that nodes are returned in an
    arbitrary order.  It may be better to use a list instead.
    (Alternately, I should check and see if Hettinger ever actually
    added memory-efficient hash tables to set.)  Note that frozensets
    can be weak-referenced so I don't need to add __weakref__ to
    slots.

    """
    __slots__ = ()


class MapNode(dict, Node):
    """Node that is a non-standard mapping of names to nodes and other
    objects.

    Unlike collections.OrderedDict, this doesn't need a linked list
    because it doesn't need to handle insertions or deletions.
    Unfortunately, this class claims to be a MutableMapping because it
    subclasses dict even though it's immutable.  This is an ideal
    candidate for replacement with Hettinger's memory-efficient dict.

    """
    __slots__ = ('__values', '__hash', '__weakref__')

    def __init__(self, other):
        if isinstance(other, collections.Mapping):
            self.__values = tuple(other.values())
            super(MapNode, self).__init__(other)
        else:
            super(MapNode, self).__init__()
            values = []
            for k, v in other:
                values.append(v)
                super(MapNode, self).__setitem__(k, v)
            self.__values = tuple(values)

    @classmethod
    def fromkeys(iterable, value = None):
        super(MapNode, self).__init__(zip(iterable, itertools.repeat(value)))
        self.__values = (value,)*len(self)

    def __iter__(self):
        return iter(self.__values)

    def __reversed__(self):
        return reversed(self.__values)

    def copy(self):
        return MapNode(self)

    def __hash__(self):
        if not hasattr(self, self.__hash):
            self.__hash = hash(frozenset(self.items()))
        return self.__hash

    def __contains__(self, value):
        return (value in self.__values)

    def __setitem__(self, *args, **kws):
        raise TypeError("'%s' object does not support item assignment" % type(self))

    def __delitem__(self, *args, **kws):
        raise TypeError("'%s' object does not support item deletion" % type(self))

    def __immutable(self, *args, **kws):
        raise TypeError("'%s' object is immutable" % type(self))

    clear = __immutable
    update = __immutable
    setdefault = __immutable
    pop = __immutable
    popitem = __immutable


class Leaf(object):
    """This is a minimal container for other Python objects, allowing them
    to be weak-referenced.  Because nodes contain Leaves, Leaves also
    must be hashable.

    """
    __slots__ = ('value', '__weakref__') # '__hash', 
    def __init__(self, value):
        self.value = value
        # self.__hash = hash(self.value)
        
    # def __hash__(self):
    #     return self.__hash

    # def __eq__(self, other):
    #     if isinstance(other, Leaf):
    #         return self.value == other.value
    #     else:
    #         return self.value == other

    def __str__(self):
        return str(self.value)

    __repr__ = __str__


if __name__ == '__main__':
    # PyPy doesn't have sys.getsizeof().
    import platform
    pypy = True if platform.python_implementation() == 'PyPy' else False
    if not pypy:
        import hettinger_total_size
        import functools
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
        a = Leaf('a')
        b = Leaf('b')
        c = Leaf('c')
        d = Leaf('d')
        e = Leaf('e')
        f = Leaf('f')
        t0 = seqnode([b, c])
        t1 = seqnode([a, t0])
        t2 = mapnode([('eeny', d), ('meeny', e), ('miny', f)])
        t3 = seqnode([t1, t2])
        nodes = {(0, 1): a,
                 (1, 2): b,
                 (2, 3): c,
                 (3, 15): d,
                 (15, 17): e,
                 (17, 21): f,
                 (0, 21): t3,
                 (0, 3): t1,
                 (1, 3): t0,
                 (3, 21): t2}
        return Tree(t3, weakref.WeakValueDictionary(nodes))

    tree = make_tree(Tree, SeqNode, MapNode)
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
        a = Leaf('a')
        b = Leaf('b')
        c0 = Leaf('c')
        c1 = Leaf('d')
        bc = node([b, c0])
        ab = node([a, b])
        bbc = node([bc, c1])
        n0 = node([a, bbc])
        abc0 = node([a, bc])
        n1 = node([abc0, c1])
        abc1 = node([ab, c0])
        n2 = node([abc1, c1])
        leaves =  {(0, 1): a,
                   (1, 2): b,
                   (2, 3): c0,
                   (3, 4): c1}
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
        a = Leaf('a')
        b = Leaf('b')
        c0 = Leaf('c')
        c1 = Leaf('c')
        bc = node([b, c0])
        ab = node([a, b])
        bbc = node([bc, c1])
        n0 = node([a, bbc])
        abc0 = node([a, bc])
        n1 = node([abc0, c1])
        abc1 = node([ab, c0])
        abc = PackedNode([abc0, abc1])
        abcc = PackedNode([node([a, bbc]), node([abc, c1])])
        nodes = {(0, 1): a,
                 (1, 2): b,
                 (2, 3): c0,
                 (3, 4): c1,
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
    visitor = Visitor()
    visitor(spf)

    if not pypy:
        print('Input, memory in bytes:', total_size('abcc'))
        print('SPF, memory:', total_size(spf))
