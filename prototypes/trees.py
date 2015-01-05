#!/usr/bin/python3

from __future__ import print_function

import types
import pprint
import collections
from hettinger_total_size import total_size


# The following Tree classes are intended to encapsulate the
# representation of the parse tree away from the combinators.  Only
# four methods are required.  __init__ should accept an
# iterable/sequence of trees of the same type or two arguments, any
# object and a tuple representing a partition.  __iter__ should
# return a preorder traversal.  nested_lists() should return a nested
# lists representation of the tree.  __str__ should pretty-print a
# nested lists representation.  Almost certainly overriding __init__
# here means all of these representations will be too slow.

# All of the implementations of methods in these classes are somewhat
# sloppy and have not been rigorously tested.  At the moment, also,
# it's possible to badly destroy the representation by assigning
# dictionary keys, changing self.root(), or doing anything, really,
# except using the provided interface.
class TreeAL(dict):
    """Adjacency list representation of a tree.

    Up to this point, I've used nested lists to represent the parse
    tree.  A tree is just a directed acyclic graph, and it ought to be
    possible to represent it as an adjacency matrix or an adjacency
    list instead.  The main reason to do this is that an SPPF probably
    can't be represented with nested lists.  It doesn't seem to me
    that a straightforward representation of the tree as a dictionary
    or orderedDict mapping to lists is going to save memory: while in
    nested lists each node costs either an empty list (64 bytes,
    nonterminals) or the cost of the output terminal, in the
    dictionary-to-lists representation each node costs 64 bytes (a
    two-integer tuple) plus either the overhead of a list and the
    representation of the other nodes or the cost of the output
    terminal.  That said, the additional flexibility might open up
    possibilities for memory savings, e.g. using the array module to
    replace the list describing edges or economizing on the
    introduction of node labels by avoiding the overhead of
    dictionaries.

    The right way to label the nodes for a parse tree is to observe
    that a parse tree without any semantic actions *partitions* the
    input.  The leaf nodes at the bottom of the tree read in order
    must cover the entire input.  The rest of the tree nodes represent
    partitions further subdivided into subpartitions.  The root node
    corresponds to (0, len(input) - 1).  Thus, formally, the key in
    the dictionary corresponding to a node in this representation is a
    tuple of two integers.  The value of a node is either a list of
    keys for other nodes, for non-leaf nodes, or any other kind of
    Python object, for leaf nodes.  There are two ways to represent
    the list of keys, a list of two-tuples of integers directly
    mapping to the keys for the other nodes or a list of single
    integers representing the subpartitions for that node.  For
    instance:

    (1, 8) : [(1, 2), (2, 5), (5, 8)]

    (1, 8) : [2, 5]

    The latter obviously costs a lot less memory but node operations
    will be slower because correct tuples will have to be generated
    from the partitions when they're needed.  This is a classic
    space-time tradeoff and will take profiling.

    This subclasses dict for performance.

    """
    __slots__ = ('root')

    class Node(list):
        __slots__ = ()

    def __init__(self, *args):
        def type_error(args):
            raise BlahTypeError('__init__ takes 0, 1, or 2 arguments, %n given' % len(args))
        dispatch = {
            0: lambda: None,
            1: self.add_subtrees,
            2: self.add_leaf
        }
        super(dict, self).__init__()
        dispatch.get(len(args), type_error)(*args)

    def add_subtrees(self, subtrees):
        if isinstance(subtrees, collections.Iterable):
            tree_type = type(self)
            partitions = self.Node()
            for subtree in subtrees:
                if not isinstance(subtree, tree_type):
                    raise BlahTypeError("Can't construct a %s except from the same type, got %s instead" % (tree_type.__name__, subtree))
                else:
                    partitions.append(subtree.root)
                    self.update(subtree)
            self.root = (min(self.keys())[0], max(self.keys())[1])
            self.setdefault(self.root, partitions)
        else:
            raise BlahTypeError("Argument must be an iterable of subtrees, got %s instead" % subtrees)

    def add_leaf(self, partition, terminal):
        self[partition] = terminal
        self.root = partition

    def __iter__(self):
        """Pre order traversal of the tree.  

        Assuming this is a correctly-formed tree, the right offset of
        the root node is the same as the highest right offset of any
        node.  It may be faster to filter first, though.  There are
        other possible efficiency improvements, as well.

        """
        if not self:
            return
        to_visit = [self.root]
        while to_visit:
            node = to_visit.pop()
            yield node
            if isinstance(self[node], self.Node):
                to_visit.extend(reversed(self[node]))
            # else:
            #     yield self[node]

    def post_order(self):
        """Post order traversal of the tree based on the algorithm from the
        link below.  This is only used as a model for creating the
        nested-lists representation below.

        http://blogs.msdn.com/b/daveremy/archive/2010/03/16/non-recursive-post-order-depth-first-traversal.aspx
        """
        if not self:
            return
        to_visit = [self.root]
        visited_ancestors = []
        while to_visit:
            node = to_visit[-1]
            if isinstance(self[node], self.Node):
                if not visited_ancestors or visited_ancestors[-1] != node:
                    visited_ancestors.append(node)
                    to_visit.extend(reversed(self[node]))
                    continue
                visited_ancestors.pop()
            yield node
            to_visit.pop()

    def level_order(self):
        """Level order traversal of the tree.

        Implementation to traverse it in bottom-up level order.
        http://www.guiguan.net/leetcode-in-swift-binary-tree-level-order-traversal-ii/
        """
        if not self:
            return
        queue = collections.deque()
        queue.append(self.root)
        while queue:
            node = queue.popleft()
            yield node
            if isinstance(self[node], self.Node):
                queue.extend(self[node])

    def nested_lists(self):
        """Traverse the tree in post order and convert it to a nested list
        representation.

        This is probably faster than the recursive implementation but
        probably not fast enough.

        """
        lists_stack = []
        if not self:
            return lists_stack
        to_visit = [self.root]
        visited_ancestors = []
        while to_visit:
            node = to_visit[-1]
            if isinstance(self[node], self.Node):
                if not visited_ancestors or visited_ancestors[-1] != node:
                    visited_ancestors.append(node)
                    to_visit.extend(reversed(self[node]))
                    continue
                visited_ancestors.pop()
                result = lists_stack[-len(self[node]):]
                del lists_stack[-len(self[node]):]
                lists_stack.append(result)
            else:
                lists_stack.append(self[node])
            to_visit.pop()
        return lists_stack[0]

    def nested_lists_recursive(self):
        """Recursive implementation of nested lists."""
        def recurse(tree, node):
            if isinstance(tree[node], self.Node):
                return [recurse(tree, n) for n in tree[node]]
            else:
                return tree[node]
        return recurse(self, self.root)

    def __str__(self):
        # return pprint.pformat(self)
        return pprint.pformat(self.nested_lists())


class TreeND(dict):
    """Nested dicts representation of a tree.

    The primary purpose of using dicts rather than lists is to
    preserve the same information as the adjacency list representation
    uses.

    """
    __slots__ = ('root')

    def __init__(self, *args):
        def type_error(args):
            raise BlahTypeError('__init__ takes 0, 1, or 2 arguments, %n given' % len(args))
        dispatch = {
            0: lambda: None,
            1: self.add_subtrees,
            2: self.add_leaf
        }
        super(dict, self).__init__()
        dispatch.get(len(args), type_error)(*args)

    def add_subtrees(self, subtrees):
        if isinstance(subtrees, collections.Iterable):
            tree_type = type(self)
            tree = tree_type()
            for subtree in subtrees:
                if not isinstance(subtree, tree_type):
                    raise BlahTypeError("Can't construct a %s except from the same type, got %s instead" % (tree_type.__name__, subtree))
                else:
                    tree.update(subtree)
            self.root = (min(tree.keys())[0], max(tree.keys())[1])
            self[self.root] = tree
        else:
            raise BlahTypeError("Argument must be an iterable of subtrees, got %s instead" % subtrees)

    def add_leaf(self, partition, terminal):
        self[partition] = terminal
        self.root = partition

    def __iter__(self):
        """ Pre order traversal of the tree."""
        to_visit = [(self.root, self[self.root])] # list(self.items())
        while to_visit:
            key, node = to_visit.pop()
            yield key
            if isinstance(node, type(self)):
                to_visit.extend(sorted(node.items(), reverse=True))
            # else:
            #     yield node

    def nested_lists(self):
        """Recursive implementation of nested lists."""
        if not self:
            return []
        def recurse(node):
            if isinstance(node, type(self)):
                return [recurse(n[1]) for n in sorted(node.items())]
            else:
                return node
        return recurse(self)[0]

    def __str__(self):
        return pprint.pformat(self.nested_lists())


class TreeNL(list):
    """Nested lists representation of a tree."""
    __slots__ = ()
    root = None

    def nested_lists(self):
        return self

    def __str__(self):
        return pprint.pformat(self)

def TreeNLConstructor(*args):
        length = len(args)
        if length == 0:
            return TreeNL()
        elif length == 1:
            return TreeNL(args[0])
        elif length == 2:
            return args[1]
        else:
            raise BlahTypeError('__init__ takes 0, 1, or 2 arguments, %n given' % length)


if __name__ == '__main__':
    #                       (0, 21)
    #              /                             \
    #           (0, 3)                         (3, 21)
    #        /          \                /        |         \
    #      (0, 1)     (1, 3)         (3, 15)   (15, 17)   (17, 21)
    #        |       /      \           |         |          |
    #       'a'   (1, 2)   (2, 3)      'd'       'e'        'f'
    #               |        |
    #              'b'      'c'

    def make_tree(tree):
        t0 = tree((0, 1), 'a')
        t1 = tree((1, 2), 'b')
        t2 = tree((2, 3), 'c')
        t3 = tree((3, 15), 'd')
        t4 = tree((15, 17), 'e')
        t5 = tree((17, 21), 'f')
        t6 = tree([t1, t2])
        t7 = tree([t0, t6])
        t8 = tree([t3, t4, t5])
        return tree([t7, t8])

    tree_al = make_tree(TreeAL)
    print('Adjacency list, pre order.')
    for n in tree_al:
        print(n)
    print('Adjacency list, post order.')
    for n in tree_al.post_order():
        print(n)
    print('Adjacency list, level order.')
    for n in tree_al.level_order():
        print(n)
    print('Adjacency list, nested lists.')
    print(tree_al)

    # tree = TreeAL({(0, 21): [(0, 3), (3, 21)],
    #             (0, 3): [(0, 1), (1, 3)],
    #             (0, 1): 'a',
    #             (1, 3): [(1, 2), (2, 3)],
    #             (1, 2): 'b',
    #             (2, 3): 'c',
    #             (3, 21): [(3, 15), (15, 17), (17, 21)],
    #             (3, 15): 'd',
    #             (15, 17): 'e',
    #             (17, 21): 'f'})

    tree_nd = make_tree(TreeND)
    print('Nested dicts, pre order.')
    for n in tree_nd:
        print(n)
    print('Nested dicts, nested lists.')
    print(tree_nd)

    print('Memory in bytes')
    print('Adjacency List:', total_size(tree_al))
    print('Nested Dicts:', total_size(tree_nd))
    print('Nested Lists:', total_size(tree_al.nested_lists()))

    # d0 = {(0, 1): 'a'}
    # d1 = {(1, 2): 'b', (2, 3): 'c'}
    # d2 = {(1, 3): d1}
    # d3 = d0.copy()
    # d3.update(d2)
    # d4 = {(0, 3): d3}
    # d5 = {(3, 15): 'd', (15, 17): 'e', (17, 21): 'f'}
    # d6 = {(3, 21): d5}
    # d7 = d4.copy()
    # d7.update(d6)
    # d8 = {(0, 21) : d7}
    # pprint.pprint(d8)
    # tree = TreeND(d8)

    # tree = TreeND()
    # tree[(0, 21)][(0, 3)][(0, 1)] = 'a'
    # tree[(0, 21)][(0, 3)][(1, 3)][(1, 2)] = 'b'
    # tree[(0, 21)][(0, 3)][(1, 3)][(2, 3)] = 'c'
    # tree[(0, 21)][(3, 21)][(3, 15)] = 'd'
    # tree[(0, 21)][(3, 21)][(15, 17)] = 'e'
    # tree[(0, 21)][(3, 21)][(17, 21)] = 'f'
