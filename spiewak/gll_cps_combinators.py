#!/usr/bin/python3

# import logging
# logging.basicConfig(filename='temp.txt', level=logging.DEBUG)

# apply_log = '{} apply() called: {}'
# chain_log = '{} chain() called:\n    {}\n    Stream: {}\n    Continuation: {}'

# import tracecalls

# {j.__closure__[0].cell_contents for i in t.backlinks[b'121212'].values() for j in i}

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


class Result:
    pass

class Success(Result):
    def __init__(self, value, tail):
        self.value = value
        self.tail = tail
    def copy(self):
        return Success(self.value, self.tail)
    def __str__(self):
      return 'Success: ' + str(self.value) + ', ' + str(self.tail)
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
        return 'Trampoline:\n    Stack: {}\n    Backlinks: {}\n    Done: {}\n    Popped {}\n    Saved {}'.format(str(self.stack), str(self.backlinks), str(self.done), str(self.popped), str(self.saved))

    # @tracecalls.TraceCalls(show_ret=True)
    def run(self):
        # logging.debug('Trampoline run() called.')
        while self.stack:
            # logging.debug('Trampoline run() looped.')
            # print(self.stack)
            parser, stream = self.remove()
            def continuation_factory(p, s):
                # @tracecalls.TraceCalls(show_ret=True)
                def trampoline_continuation(res):
                    if s not in self.popped:
                        self.popped[s] = {}
                    if p not in self.popped[s]:
                        self.popped[s][p] = set()
                    if isinstance(res, Success):
                        self.popped[s][p].add(res)
                    # else:
                    #     return None
                    if res not in self.saved:
                        self.saved[res] = set()
                    for f in self.backlinks[s][p]:
                        if f.__code__ not in {i.__code__ for i in self.saved[res]}:
                            self.saved[res].add(f)
                            f(res)
                return trampoline_continuation
            parser.chain(self, stream, continuation_factory(parser, stream))

    # @tracecalls.TraceCalls(show_ret=True)
    def add(self, p, stream, f):
        # logging.debug('Trampoline add() called:\n    Parser: {}\n    Stream: {}\n    Continuation: {}'.format(p, stream, f))
        if stream not in self.backlinks:
            self.backlinks[stream] = {}
        if p not in self.backlinks[stream]:
            self.backlinks[stream][p] = set()
        if f.__code__ not in {i.__code__ for i in self.backlinks[stream][p]}:
            self.backlinks[stream][p].add(f)
        if stream in self.popped and p in self.popped[stream]:
            for res in self.popped[stream][p]:
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
    # @tracecalls.TraceCalls(show_ret=True)
    def apply(self, stream):
        # logging.debug(apply_log.format(type(self).__name__, stream))
        t = Trampoline()
        successes = set()
        failures = set()
        # @tracecalls.TraceCalls(show_ret=True)
        def nonterminal_continuation(res):
            nonlocal successes, failures
            if isinstance(res, Success):
                if res.tail:
                    failures.add(Failure('Unexpected trailing characters: "{}"'.format(str(res.tail))))
                else:
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

    # @tracecalls.TraceCalls(show_ret=True)
    def chain(t, stream, f):
        # logging.debug(chain_log.format(type(self).__name__, str(t), stream, f))
        def continuation_factory(continuation):
            # @tracecalls.TraceCalls(show_ret=True)
            def continuation1(res1):
                if isinstance(res1, Success):
                    # @tracecalls.TraceCalls(show_ret=True)
                    def continuation2(res2):
                        if instance(res2, Success):
                            return continuation(Success(res1.value, res2.value), res2.tail)
                        else:
                            return continuation(res2)
                    return self.right.chain(t, res1.tail, continuation2)
                else:
                    return continuation(res1)
            return continuation1
        self.left.chain(t, stream, continuation_factory(f))


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

    # @tracecalls.TraceCalls(show_ret=True)
    def chain(self, t, stream, f):
        # logging.debug(chain_log.format(type(self).__name__, str(t), stream, f))
        results = set()
        def continuation_factory(continuation):
            nonlocal results
            # @tracecalls.TraceCalls(show_ret=True)
            def disjunctive_continuation(res):
                nonlocal results
                if res not in results:
                    continuation(res)
                    results.add(res)
            return disjunctive_continuation
        for p in self.alternates:
            t.add(p, stream, continuation_factory(f))


class TerminalParser(Parser):
    # @tracecalls.TraceCalls(show_ret=True)
    def chain(self, t, stream, f):
        # logging.debug(chain_log.format(type(self).__name__, str(t), stream, f))
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

    # @tracecalls.TraceCalls(show_ret=True)
    def apply(self, stream):
        # logging.debug(apply_log.format(type(self).__name__, stream))
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
    # l =
    # '====================================================================='

    # The implementation in Spiewak's paper doesn't seem to be
    # complete because the only parser that will ever return
    # "Unexpected trailing characters" is a non-terminal parser.
    parser = LiteralParser(b'01')
    print('Combinators,', parser.apply(b'010101'))
    print('Combinators,', parser.apply(b'121212'))
    # logging.debug(l)
    parser = TerminalSequentialParser(LiteralParser(b'0'), LiteralParser(b'1'))
    print('Combinators,', parser.apply(b'010101'))
    # logging.debug(l)
    parser = DisjunctiveParser(LiteralParser(b'01'), LiteralParser(b'12'))
    print('Combinators,', parser.apply(b'121212'))
    # logging.debug(l)
    parser = LiteralParser(b'0') + LiteralParser(b'1')
    print('Combinators,', parser.apply(b'010101'))
    # logging.debug(l)
    parser = LiteralParser(b'01') | LiteralParser(b'12')
    print('Combinators,', parser.apply(b'121212'))


# SINGLE-DISPATCH SETUP

# class Parser:
#     def apply(self, stream):
#         raise NotImplementedError
#     def chain(self, t, stream, f):
#         raise NotImplementedError

# class NonTerminalParser(Parser):
#     def __init__():
#         self._continuation = singledispatch(self._continuation)
#         self._continuation.register(Success, self._success)
#         self._continuation.register(Failure, self._failure)
#     def apply(self, stream):
#         t = Trampoline()
#         # Spiewak implements this method using currying and an
#         # anonymous pattern-matching function.  The syntax for this in
#         # Scala is an example of how bizarre Scala is: you can call a
#         # function using parentheses as normal, but there's also a
#         # special notation called infix notation that allows eliding
#         # of parentheses when calling higher-order functions.  (This
#         # is a *convention* not a hard rule, which makes it possible
#         # to write even more confusing code, but the general
#         # complications of infix notation are beyond this short note.)
#         # In the Scala code, the partial version of chain returns a
#         # function that then acts on an anonymous pattern-matching
#         # function.  Python lambdas aren't powerful enough compared to
#         # Scala anonymous functions so I need to name the function.
#         functools.partial(self.chain, t, stream)(self._continuation)
#         t.run()
#         # somehow creates a list of Results
#     def _continuation(self, result):
#         raise NotImplementedError
#     def _success(self, result):
#         raise NotImplementedError
#     def _failure(self, result):
#         raise NotImplementedError

# # Non-terminal version
# class NonTerminalSequentialParser(NonTerminalParser):
#     def __init__(self, left, right):
#         super().__init__()
#         self._success_continuation = singledispatch(self._succcess_continuation)
#         self._success_continuation.register(Success, self._success_success)
#         self._success_continuation.register(Failure, self._failure)
#         self.left = left
#         self.right = right
#     def chain(t, stream, f):
#         functools.partial(self.left.chain, t, stream)(self._continuation)
#     def _continuation(self, result):
#         raise NotImplementedError
#     def _success(self, result):
#         functools.partial(self.right.chain, t, stream)(self._success_continuation)
#     def _success_continuation(self, result):
#         raise NotImplementedError
#     def _success_success(self, result):
        
#     def _failure(self, result):
#         return f(result)

