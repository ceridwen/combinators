#!/usr/bin/python3

"""v through is an arbitrary type corresponding to a node value in a
tree.  label is another arbitrary type representing all labels."""

""" root = None | info"""

""" info = collections.namedtuple(label, v, [branchings])"""

""" branching = root | (root, root)"""

# The major differences between my current implementation and the Yakker implementation are:

# 1.  The non-packed nodes in their tree are binarized.

# 2.  Nodes embed both packing and the normal branching in the same data structure.  Each node contains a list of pairs of children.

# 3. Every node contains, in addition to its children, an arbitrary semantic value and a label.

# 4.  Both have a weak hash table containing links to all the partial trees in the forest.  However, their implementation seems to use the hash table like a set, with a function that checks directly if a partial tree is in the table and returning it if so.
