## Synopsis

This is a parser combinator library intended to run faster and use
less memory than other parser combinators, handle a broader class of
grammars and languages, and parse text, binary, and mixed formats.

## Motivation

Construct (https://pypi.python.org/pypi/construct) and other parser
combinator libraries in Python are traditional recursive-descent
parsers.  Almost all of them, like Construct, ignore ambiguities by
using prioritized choice and can't handle left-recursive grammars at
all.  LEPL (https://pypi.python.org/pypi/LEPL/) can handle ambiguous
grammars and uses memoization for left-recursion but is no longer
actively maintained; moreover, memoization takes a lot of memory.  By
using new parsing algorithms published in the last decade, I intend to
build a parser combinator library that:

* Can parse all context-free grammars, including ambiguous and
  left-recursive grammars.

* Parses inputs of size n in a maximum of O(n^3) time and space and
  typical grammars with limited ambiguity in O(n) time.

* Can parse some context-sensitive languages using the
  previously-parsed input without violating the above performance
  bounds.

* Has improved error reporting.

* May do just-in-time compilation for improved speed.
