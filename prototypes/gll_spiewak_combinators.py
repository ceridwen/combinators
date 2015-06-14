#!/usr/bin/python3

# There are major unfinished elements of this implementation, some
# left unfinished by Spiewak, some by me.

# Me: Lazy evaluation.  The problem is that this is just very hard to
# do in Python and will require major rearchitecting that I'm not sure
# is worthwhile for a learning project.

# Spiewak: There's no discussion in his paper of semantic actions
# for GLL combinators in particular, and I'm not immediately sure how
# to modify the algorithm to accommodate them.

# Spiewak: I'm not sure how Spiewak is using pattern-matching in the
# last block that implements longest match, and it isn't complete
# anyways.

import pprint


class Result:
    pass
class Success(Result):
    def __init__(self, value, tail):
        self.value = value
        self.tail = tail
    def copy(self):
        return Success(self.value, self.tail)
    def __str__(self):
        return "Success: %s, '%s'" % (self.value, self.tail)
    __repr__ = __str__

class Failure(Result):
    def __init__(self, msg):
        self.msg = msg
    def copy(self):
        return Failure(self.msg)
    def __str__(self):
        return 'Failure: ' + self.msg
    __repr__ = __str__

class Trampoline:
    def __init__(self):
        self.stack = []
        self.remove = self.stack.pop
        # A map of a stream position to a map mapping parser instances
        # to sets of continuations.
        self.backlinks = {}
        # A map of a stream position to sets of parsers.
        self.done = {}
        # A map of a stream position to a map mapping parser instances
        # to sets of successes.
        self.popped = {}
        # A map of Results to sets of functions.
        self.saved = {}

    def __str__(self):
        return '\n'.join(['Trampoline', 'Stack', pprint.pformat(self.stack), 'Backlinks', pprint.pformat(self.backlinks), 'Done', pprint.pformat(self.done), 'Popped', pprint.pformat(self.popped), 'Saved', pprint.pformat(self.saved)])

    def run(self):
        while self.stack:
            # print(self)
            parser, stream = self.remove()
            def trampoline_continuation(res, stream=stream, parser=parser):
                # print('Trampoline:', res)
                if stream not in self.popped:
                    self.popped[stream] = {}
                if parser not in self.popped[stream]:
                    self.popped[stream][parser] = set()
                if isinstance(res, Success):
                    self.popped[stream][parser].add(res)
                    # print('Trampoline success: ', res, pprint.pformat(self.backlinks), pprint.pformat(self.saved), sep='\n')
                # else:
                #     return None
                if res not in self.saved:
                    self.saved[res] = set()
                for f in self.backlinks[stream][parser]:
                    if f not in self.saved[res]:
                        self.saved[res].add(f)
                        f(res)
            # print(inspect.getclosurevars(trampoline_continuation))
            parser.chain(self, stream, trampoline_continuation)

    def add(self, p, stream, f):
        if stream not in self.backlinks:
            self.backlinks[stream] = {}
        if p not in self.backlinks[stream]:
            self.backlinks[stream][p] = set()
        if f not in self.backlinks[stream][p]:
            self.backlinks[stream][p].add(f)
        if stream in self.popped and p in self.popped[stream]:
            for res in self.popped[stream][p].copy():
                f(res)
        else:
            if stream not in self.done:
                self.done[stream] = set()
            if p not in self.done[stream]:
                self.stack.append((p, stream))
                self.done[stream].add(p)


# The concept of terminal and nonterminal parsers is closely related
# to the concept of static and dynamic parsers I've discussed in my
# prototype.

# I follow Spiewak in using t for Trampoline class variables and f for
# continuations.  apply is a method that takes a stream and returns a
# Result, as in recursive-descent parser combinators.  While working
# out the basic parser combinators I used __call__ instead of apply
# because apply was a built-in function in Python 2 (deprecated since
# 2.3), but with two central methods I decided to follow Spiewak's
# convention.  The other method is chain.  Scala allows you to declare
# functions with multiple lists of parameters, like:

# def chain(t: Trampoline, in: Stream[Char])(f: Result[A] => Unit)

# If you call a Scala function with fewer than the full number of
# parameters, Scala will automatically return a function that takes
# the remaining parameters (currying).  Chain takes a Trampoline, a
# stream, and a function/continuation taking a Result and returning
# the Unit algebraic type and returns the Unit algebraic type (this is
# equivalent to None in Python or void in Java), or, it takes a
# Trampoline and a stream and returns a function/continuation that
# takes a function taking a Result and returning the Unit algebraic
# type.  The only ways to simulate this currying in Python are with
# functools.partial and closures, though neither are quite the same
# thing.

# I need to close over the continuations that Spiewak implements as
# anonymous pattern-matching methods using currying.  The syntax for
# this in Scala is an example of how bizarre Scala is: you can call a
# function using parentheses as normal, but there's also a special
# notation called infix notation that allows eliding of parentheses
# when calling higher-order functions.  (This is a *convention* not a
# hard rule, which makes it possible to write even more confusing
# code, but the general complications of infix notation are beyond
# this short note.)  In the Scala code, the partial version of chain
# returns a function that then acts on an anonymous pattern-matching
# function.

# I've figured out a couple of different approaches, all bad.

# Use lambdas as anonymous functions as in Scala.  The main trick with
# this is that I have to use ternary if-else or nested and-or (see
# https://www.inkling.com/read/learning-python-mark-lutz-4th/chapter-19/anonymous-functions-lambda)
# to handle the selection logic, one of,

# b if a else c

# ((a and b) or c)

# instead of,

# if a:
#     b
# else:
#     c

# Use nested named closures with conditional logic in chain.  The
# amount of function creation and calls will make this very slow,
# though probably not much slower than the lambda method.

# Use named single-dispatch methods and pass in the continuation
# received by chain.  This avoids some of the horrible slowness by
# setting up the single-dispatch logic in __init__, but requires
# changing the signature of the continuations from what it is in
# Scala.

# Use nested named single-dispatch closures in chain.  The main
# problem with this is the amount of logic that has to be stuffed into
# the chain method: the continuations have to be created, registered
# as single-dispatch functions, closed, and then passed along.  This
# is horribly slow and completely unsuitable for the real
# implementation.

# For the real implementation, it's not clear to me which of
# single-dispatch methods, simple conditionals (like I used for the
# basic combinators), or dictionaries are the fatest way to branch
# based on whether a function receives Success or Failure, where
# pattern matching is used in Scala.  This will require profiling.

# There's an important difference between how Python handles closures
# and Scala (apparently) handles its anonymous methods: each time a
# function/method creating a closure is called, it creates a new
# function object that is not considered equal to previous instances
# of that closure, while in Scala, testing for equality/containment on
# the anonymous methods works correctly.  The way I found to
# circumvent this is that each closure object contains a .__code__
# attribute that is unique across all instances, so I always have to
# do equality/containment testing on that.  This would not be fast
# enough for a real implementation.

# The other problem with Python's closures in this particular design
# is that they're late-binding, which means that for instance if the
# same chain method is called with one closure and then called with
# another before the first closure is executed, the first closure will
# never be executed.  While the easiest way to fix this is to use
# default arguments, this is a hack.  Instead, I use a function
# factory where necessary.

# Important note from Spiewak: "The point is that apply can be relied
# upon to perform initialization operations for a specific parse.
# Since GLL initialization is the same for any non-terminal parser,
# these operations can be implemented safely in the superclass." (14)


class Parser:
    def apply(self, stream):
        # returns a list of Results
        raise NotImplementedError
    def chain(self, t, stream, f):
        # returns None
        raise NotImplementedError
    # No binary ~ operator in Python so I overloaded + instead
    def __add__(self, other):
        raise NotImplementedError
    def __radd__(self, other):
        raise NotImplementedError
    def __or__(self, other):
        return DisjunctiveParser(self, other)
    def __ror__(self, other):
        return DisjunctiveParser(other, self)
    # No ^^ operator in Python so I overloaded ^ instead
    # def __xor__(self, f):
    #     return Parser(self, f)


class NonTerminalParser(Parser):
    def apply(self, stream):
        t = Trampoline()
        successes = set()
        failures = set()
        def nonterminal_continuation(res):
            if isinstance(res, Success):
                # Disabled for now.
                # if res.tail:
                #     failures.add(Failure('Unexpected trailing characters: "{}"'.format(str(res.tail))))
                # else:
                successes.add(res)
            else:
                failures.add(res)
        self.chain(t, stream, nonterminal_continuation)
        t.run()
        if successes:
            return list(successes)
        else:
            return list(failures)

    def __add__(self, other):
        return NonTerminalSequentialParser(self, other)
    def __radd__(self, other):
        return NonTerminalSequentialParser(other, self)


# Non-terminal version
class NonTerminalSequentialParser(NonTerminalParser):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def chain(self, t, stream, f):
        def continuation1(res1):
            if isinstance(res1, Success):
                def continuation2(res2):
                    if isinstance(res2, Success):
                        # print('Sequence:', res1, res2)
                        return f(Success((res1.value, res2.value), res2.tail))
                    else:
                        return f(res2)
                return self.right.chain(t, res1.tail, continuation2)
            else:
                return f(res1)
        self.left.chain(t, stream, continuation1)


class DisjunctiveParser(NonTerminalParser):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        # Walk the parser tree to gather alternates
        def gather(seen):
            seen.add(self)
            def process(p):
                nonlocal seen
                if isinstance(p, DisjunctiveParser):
                    if p not in seen:
                        seen |= set(p.alternates)
                        return seen
                    else:
                        return set()
                else:
                    seen.add(p)
                    return seen
            return process(self.left) | process(self.right)
        self.alternates = list(gather(set()))

    def chain(self, t, stream, f):
        results = set()
        def disjunctive_continuation(res):
            # print('Disjunction:', res)
            if res not in results:
                f(res)
                results.add(res)
        for p in self.alternates:
            t.add(p, stream, disjunctive_continuation)


class TerminalParser(Parser):
    def chain(self, t, stream, f):
        f(self.apply(stream))

    def __add__(self, other):
        if isinstance(other, TerminalParser):
            return TerminalSequentialParser(self, other)
        else:
            return NonTerminalSequentialParser(self, other)
    def __radd__(self, other):
        # I can't use the lazy evaluation trick I used in the basic
        # combinators here because I don't know what kind of parser
        # the other parser will be at instantiation time.
        if isinstance(other, TerminalParser):
            return TerminalSequentialParser(other, self)
        else:
            return NonTerminalSequentialParser(other, self)


class TerminalSequentialParser(TerminalParser):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    # Spiewak defines this method inside the method for the overloaded
    # sequence operator in TerminalParser in Scala, but there's no
    # easy way to bind methods to instances in Python so I defined it
    # this way instead.  There may be a more elegant way to do this.
    # @tracecalls.TraceCalls(show_ret=True)
    def apply(self, stream):
        # logging.debug(apply_log.format(type(self).__name__, stream))
        result1 = self.left.apply(stream)
        if isinstance(result1, Success):
            result2 = self.right.apply(result1.tail)
            if isinstance(result2, Success):
                return Success((result1.value, result2.value), result2.tail)
            else:
                return result2.copy()
        else:
            return result1.copy()

class LiteralParser(TerminalParser):
    def __init__(self, value):
        self.value = value

    def apply(self, stream):
        length = len(self.value)
        if (length > len(stream)):
            return Failure('Unexpected end of stream (expected "{}")'.format(self.value))
        else:
            # Spiewak uses a different approach here but this will be
            # faster in Python
            if stream.startswith(self.value):
                return Success(self.value, stream[length:])
            else:
                return Failure('Expected "{}" got "{}"'.format(self.value, stream[:length]))

if __name__ == '__main__':
    # The implementation in Spiewak's paper doesn't seem to be
    # complete because the only parser that will ever return
    # "Unexpected trailing characters" is a non-terminal parser.
    import cProfile
    import platform
    import six
    import sys
    import time
    import timeit

    CPYTHON = True if platform.python_implementation() == 'CPython' else False
    if six.PY3 and CPYTHON:
        import tracemalloc
        tracemalloc.start()

    def tracefunc(frame, event, arg, indent=[0]):
        if event == "call":
            if frame.f_code.co_filename == 'gll_spiewak_combinators.py':
                indent[0] += 2
                name = frame.f_code.co_name
                print("-" * indent[0] + "> call", frame.f_code.co_filename, name)
                if not (name == '__str__' or name == '__init__' or name == '<lambda>'):
                    pprint.pprint(frame.f_locals)
        elif event == "return":
            if frame.f_code.co_filename == 'continuation_gll_combinators.py':
                print("<" + "-" * indent[0], "exit", frame.f_code.co_name)
                indent[0] -= 2
        return tracefunc

    # The implementation in Spiewak's paper doesn't seem to be
    # complete because the only parser that will ever return
    # "Unexpected trailing characters" is a non-terminal parser.
    strings = LiteralParser('ab')
    print('Literal success,', strings.apply('ababab'))
    print('Literal failure,', strings.apply('bcbcbc'))
    terminal = LiteralParser('a') + LiteralParser('b')
    print('Terminal success,', terminal.apply('ababab'))
    terminal = TerminalSequentialParser(LiteralParser('a'), LiteralParser('b'))
    print('Terminal success,', terminal.apply('ababab'))

    alternation = LiteralParser('ab') | LiteralParser('bc')
    print('Disjunction success,', alternation.apply('bcbcbc'))
    alternation = DisjunctiveParser(LiteralParser('ab'), LiteralParser('bc'))
    print('Disjunction success,', alternation.apply('bcbcbc'))

    sequence = LiteralParser('a') + (LiteralParser('b') | LiteralParser('c'))
    print('Sequence alternation success,', sequence.apply('abc'))
    print('Sequence alternation success,', sequence.apply('acb'))
    print('Sequence alternation failure,', sequence.apply('cba'))
    sequence = NonTerminalSequentialParser(LiteralParser('a'), DisjunctiveParser(LiteralParser('b'), LiteralParser('c')))
    print('Sequence alternation success,', sequence.apply('abc'))
    print('Sequence alternation success,', sequence.apply('acb'))
    print('Sequence alternation failure,', sequence.apply('cba'))
    
    # sys.settrace(tracefunc)

    # Ugly hack to work around the lack of forward reference support
    # in this prototype
    ambiguous = DisjunctiveParser(0, 0)
    ambiguous.alternates = [ambiguous + ambiguous + ambiguous, ambiguous + ambiguous, LiteralParser('a')]
    print('Highly ambiguous,', ambiguous.apply('aaa'))

#     def time_ambiguous(max_length):
#         for i in range(2, max_length):
#             print(i, timeit.timeit('ambiguous.apply("' + i * 'a' + '")', 'gc.enable(); from __main__ import ambiguous', number=1000))

#     cProfile.run('''
# time_ambiguous(9)
# ''')

    # alpha = Regex('([a-zA-Z])')
    # hex_char = Regex('([a-fA-F0-9])')

    # alpha_or_hex = alpha | hex_char
    # print('Alpha success,', alpha_or_hex.apply('xyz'))
    # print('Alpha and hex success,', alpha_or_hex.apply('ABC'))
    # print('Hex success,', alpha_or_hex.apply('123'))

    # a = LiteralParser('a')
    # l = Lazy('a')
    # print('Lazy,', l.apply('a'))
    # print('Lazy,', l.apply('a'))
    
    # word = Regex('([a-zA-Z]+)')
    # sentence = ((word + LiteralParser('.')) >> (lambda t: t[0])) | ((word + Regex(r'[\s,]') + Lazy('sentence')) >> (lambda t: t[0][0] + t[1]))
    # print('Sentence success,', sentence.apply('The quick brown fox jumps over the lazy dog.'))

#     dictionary = [w.strip().replace("'", '') for w in open('/usr/share/dict/words', 'r').read().splitlines() if w.strip().isalpha()]
#     sample = ' '.join(dictionary[:10000]) + '.'

#     start = time.clock()
#     sentence.apply(sample)
#     end = time.clock()
#     print('Dictionary, %.4f seconds' % (end - start,))

    if six.PY3 and CPYTHON:
        snapshot = tracemalloc.take_snapshot()
        top_stats = snapshot.statistics('lineno')

        print("[ Top 10 ]")
        for stat in top_stats[:10]:
            print(stat)
