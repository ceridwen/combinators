#!/usr/bin/python3

# This is a reimplementation of the parser combinators from Koopman
# and Plasmeijer, "Efficient Parser Combinators," to test if the use
# of continuations to avoid creating intermediate data structures will
# improve performance in Python.

import abc
import string
import sys
import time
import traceback
import types

import six

# Because Python does not like recursion.
class Trampoline(object, six.with_metaclass(abc.ABCMeta)):
    def begin(self, stream):
        stack = []
        result = self(stream)
        error = None
        while True:
            if isinstance(result, types.GeneratorType):
                # Push a new coroutine onto the stack and prepare to
                # initialize it.
                stack.append(result)
                result = None
            elif stack:
                # Send results into a coroutine until it raises
                # StopIteration; when it does, remove it from the
                # stack.  The last result it returned was its
                # final output.
                try:
                    if error:
                        result = stack[-1].throw(error[1])
                    else:
                        result = stack[-1].send(result)
                except Exception as e:
                    stack.pop()
                    error = None if isinstance(e, StopIteration) else sys.exc_info()
            elif error:
                traceback.print_exception(*error)
                break
            else:
                return result


# Basic monadic parser combinators.
class Parser(Trampoline):
    @abc.abstractmethod
    def __init__(self):
        raise NotImplementedError
    @abc.abstractmethod
    def __call__(self, stream):
        raise NotImplementedError        
    def __or__(self, other):
        return Alternation(self, other)
    def __ror__(self, other):
        return Alternation(other, self)
    def __add__(self, other):
        return Bind(self, other)
    def __radd__(self, other):
        return Bind(other, self)

class String(Parser):
    def __init__(self, string):
        self.string = string
    def __call__(self, stream):
        if stream.startswith(self.string):
            return [(self.string, stream[len(self.string):])]
        else:
            return []

class Satisfy(Parser):
    def __init__(self, predicate):
        self.predicate = predicate
    def __call__(self, stream):
        if self.predicate(stream[0]):
            return [(stream[0], stream[1:])]
        else:
            return []

class Fail(Parser):
    def __init__(self):
        pass
    def __call__(self, stream):
        return []

class Yield(Parser):
    def __init__(self, result):
        self.result = result
    def __call__(self, stream):
        return [(self.result, stream)]

class Bind(Parser):
    def __init__(self, parser, func):
        self.parser = parser
        self.func = func
    def __call__(self, stream):
        result1 = yield self.parser(stream)
        results = []
        for result2, rest in result1:
            result3 = yield self.func(result2)(rest)
            results.extend(result3)
        yield results

class Alternation(Parser):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __call__(self, stream):
        result1 = yield self.left(stream)
        result2 = yield self.right(stream)
        yield result1 + result2

def Action(parser, func):
    return parser + (lambda x: Yield(func(x)))

def Repetition(parser):
    return (parser + (lambda r: Action(Repetition(parser), lambda rs: r + rs))) | Yield('')

def Plus(parser):
    return parser + (lambda r: Action(Repetition(parser), lambda rs: r + rs))


# The monadic parsers with continuations.  The trampoline here has to
# function differently because we're no longer using generators.
class ParserC(object, six.with_metaclass(abc.ABCMeta)):
    @abc.abstractmethod
    def __init__(self):
        raise NotImplementedError
    def begin(self, stream):
        # This is a perversion of functional style, passing down a
        # mutable object rather than returning, but the necessity of
        # trampolining requires something like this without
        # rearchitecting how the parsers work.
        results = []
        def begin_cont():
            def success_cont(r, nc, ss):
                results.append((r, ss))
                return nc
            return self(success_cont, None, stream)
        continuation = begin_cont
        while callable(continuation):
            # print(continuation)
            continuation = continuation()
        return results
    @abc.abstractmethod
    def __call__(self, success, next_cont, stream):
        raise NotImplementedError
    def __or__(self, other):
        return AlternationC(self, other)
    def __ror__(self, other):
        return AlternationC(other, self)
    def __add__(self, other):
        return BindC(self, other)
    def __radd__(self, other):
        return BindC(other, self)

class FailC(ParserC):
    def __init__(self):
        pass
    def __call__(self, success, next_cont, stream):
        def fail_cont():
            return next_cont
        return fail_cont

class YieldC(ParserC):
    def __init__(self, result):
        self.result = result
    def __call__(self, success, next_cont, stream):
        def yield_cont():
            return success(self.result, next_cont, stream)
        return yield_cont

class StringC(ParserC):
    def __init__(self, string):
        self.string = string
    def __call__(self, success, next_cont, stream):
        if stream.startswith(self.string):
            def string_cont():
                return success(self.string, next_cont, stream[len(self.string):])
            return string_cont
        else:
            return next_cont

class SatisfyC(ParserC):
    def __init__(self, predicate):
        self.predicate = predicate
    def __call__(self, success, next_cont, stream):
        if self.predicate(stream[0]):
            def satisfy_cont():
                return success(stream[0], next_cont, stream[1:])
            return satisfy_cont
        else:
            return next_cont

# Taken from Okasaki's paper, this is not intended to be functional
# but to be a model for figuring out the uncurried version of BindC.
class SeqC(ParserC):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __call__(self, success, next_cont, stream):
        def seq_cont():
            def left_cont(x, next_cont, stream):
                def right_cont(y, next_cont, stream):
                    return success((x,y), next_cont, stream)
                return self.right(right_cont, next_cont, stream)
            return self.left(left_cont, next_cont, stream)
        return seq_cont

class BindC(ParserC):
    def __init__(self, parser, func):
        self.parser = parser
        self.func = func
    def __call__(self, success, next_cont, stream):
        def bind_cont():
            def left_cont(result1, next_cont, stream):
                def right_cont(result2, next_cont, stream):
                    return success(result2, next_cont, stream)
                return self.func(result1)(right_cont, next_cont, stream)
            return self.parser(left_cont, next_cont, stream)
        return bind_cont
    
class AlternationC(ParserC):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __call__(self, success, next_cont, stream):
        def alternation_cont():
            def left_cont():
                def right_cont():
                    return next_cont
                return self.right(success, right_cont, stream)
            return self.left(success, left_cont, stream)
        return alternation_cont

def ActionC(parser, func):
    return parser + (lambda x: YieldC(func(x)))

def RepetitionC(parser):
    return (parser + (lambda r: ActionC(RepetitionC(parser), lambda rs: r + rs))) | YieldC('')

def PlusC(parser):
    return parser + (lambda r: ActionC(RepetitionC(parser), lambda rs: r + rs))


# The hand-written recursive descent parser.
class AdHocParser(Trampoline):
    def __call__(self, stream):
        return self.word('', [], stream)
    def word(self, w, s, stream):
        if stream and stream[0].isalpha():
            result = yield self.word(w + stream[0], s, stream[1:])
        else:
            result = yield self.sep('', [w] + s, stream)
        yield result
    def sep(self, l, s, stream):
        if stream and (stream[0].isspace() or stream[0] == ','):
            result = yield self.sep(stream[0] + l, s, stream[1:])
        elif l == '':
            result = yield self.dot(s, stream)
        else:
            result = yield self.word('', s, stream)
        result
    @staticmethod
    def dot(s, stream):
        if s and stream and stream[0] == '.':
            s.reverse()
            return [(s, stream[1:])]
        else:
            return []


if __name__ == '__main__':
    print('Generator combinators')
    def b_or_c(x):
        return Action(String('b') | String('c'), lambda y: (x, y))

    a_and_b_or_c = String('a') + b_or_c
    print(a_and_b_or_c.begin('abc'))
    print(a_and_b_or_c.begin('acb'))
    print(a_and_b_or_c.begin('cba'))

    print('Continuation combinators')
    def b_or_c_cont(x):
        return ActionC(StringC('b') | StringC('c'), lambda y: (x, y))

    a_and_b_or_c_cont = StringC('a') + b_or_c_cont
    print(a_and_b_or_c_cont.begin('abc'))
    print(a_and_b_or_c_cont.begin('acb'))
    print(a_and_b_or_c_cont.begin('cba'))

    print('Generator combinators')
    alpha = Satisfy(str.isalpha)
    hex_char = Satisfy(lambda c: c in string.hexdigits)

    alpha_or_hex = alpha | hex_char
    # Changed example because hexdigits includes lower-case letters.
    print(alpha_or_hex.begin('xyz'))
    print(alpha_or_hex.begin('ABC'))
    print(alpha_or_hex.begin('123'))

    word = Plus(alpha) # No need for Action with str here.
    print(word.begin('Hello world'))

    sep = Plus(Satisfy(str.isspace) | String(','))
    
    print('Continuation combinators')
    alpha_cont = SatisfyC(str.isalpha)
    hex_char_cont = SatisfyC(lambda c: c in string.hexdigits)

    alpha_or_hex_cont = alpha_cont | hex_char_cont
    # Changed example because hexdigits includes lower-case letters.
    print(alpha_or_hex_cont.begin('xyz'))
    print(alpha_or_hex_cont.begin('ABC'))
    print(alpha_or_hex_cont.begin('123'))
    
    word_cont = PlusC(alpha_cont) # No need for Action with str here.
    print(word_cont.begin('Hello world'))

    sep_cont = PlusC(SatisfyC(str.isspace) | StringC(','))

    # Is there a cleaner way to do this?  I don't know, but I think
    # not, because the implicit fold over the strings is hard-coded in
    # the Action's lambda.  It would probably make more sense if it
    # was parametrized somehow, but because the second lambda is a
    # closure over r, it's not obvious how to do this.
    def RepeatWords(parser):
        return (parser + (lambda r: Action(RepeatWords(parser), lambda rs: [r] + rs))) | Yield([])

    def PlusWords(parser):
        return parser + (lambda r: Action(RepeatWords(parser), lambda rs: [r] + rs))
    
    def more_words(w):
        def period(r):
            return Action(String('.'), lambda _: [w] + r)
        return PlusWords(sep + (lambda _: word)) + period

    def RepeatWordsC(parser):
        return (parser + (lambda r: ActionC(RepeatWordsC(parser), lambda rs: [r] + rs))) | YieldC([])

    def PlusWordsC(parser):
        return parser + (lambda r: ActionC(RepeatWordsC(parser), lambda rs: [r] + rs))
    
    def more_words_cont(w):
        def period(r):
            return ActionC(StringC('.'), lambda _: [w] + r)
        return PlusWordsC(sep_cont + (lambda _: word_cont)) + period

    sentence = word + more_words
    print(sentence.begin('The quick brown fox jumps over the lazy dog.'))
    sentence_cont = word_cont + more_words_cont
    print(sentence_cont.begin('The quick brown fox jumps over the lazy dog.'))
    ad_hoc_parser = AdHocParser()
    print(ad_hoc_parser.begin('The quick brown fox jumps over the lazy dog.'))

    dictionary = [w.strip().replace("'", '') for w in open('/usr/share/dict/words', 'r').read().splitlines() if w.strip().isalpha()]
    sample = ' '.join(dictionary[:1000]) + '.'

    for name, parser in (('Generator Combinators', sentence), ('Continuation Combinators', sentence_cont), ('Ad hoc', ad_hoc_parser)):
        start = time.clock()
        parser.begin(sample)
        end = time.clock()
        print('%s: %.4f seconds' % (name, end - start))
 
