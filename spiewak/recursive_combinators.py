#!/usr/bin/python3

# This code isn't really intended for release, it's just me trying to
# work out parser combinators using algebraic data types and pattern
# matching in Python so I can understand what the algorithm is doing.
# As much as possible, I'm going to follow Spiewak's variable naming
# conventions even where I don't like them.  It's always possible to
# turn recursion into iteration, so a better recursive-descent parser
# in Python would have its own call stack.  That said, what GLL does
# is to create a much more sophisticated version of a call stack, so
# there's no reason for me to implement that separately here.  Spiewak
# even observes, "GLL is really much like a tail-recursive form of
# recursive-descent."

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

class Failure(Result):
    def __init__(self, msg):
        self.msg = msg
        # print(self)
    def copy(self):
        return Failure(self.msg)
    def __str__(self):
        return 'Failure: ' + self.msg

# A Parser is a callable class (Spiewak creates a method called apply,
# but that's a reserved word in Python) that takes a stream (Spiewak
# calls it in, but that's another reserved word) and returns a Result.
# Some Parsers need to be able to act on other Parsers.  There has to
# be a better method for handling pattern matching than if/else, but I
# don't see it immediately.

# In a function definition the => Spiewak uses causes Scala to lazily
# evalute the parameter of the function in question.  This occurs in
# outer and f in the final definition of Parser, in the function
# taking the stream to the output, and in the second Parser taken by
# SequentialParser.  To get lazy evaluation/recursion in Python, I use
# strings in recursive definitions and defer evaluation until
# __call__, then use globals() to look up the variable corresponding
# to the string to set the parser to call itself.

class Parser:
    def __init__(self, inner, f):
        self.inner = inner
        self.f = f
    def __call__(self, stream):
        result = self.inner(stream)
        if isinstance(result, Success):
            return Success(self.f(result.value), result.tail)
        else:
            return result
    # No binary ~ operator in Python so I overloaded + instead
    def __add__(self, other):
        return SequentialParser(self, other)
    def __radd__(self, other):
        return SequentialParser(other, self)
    def __or__(self, other):
        return DisjunctiveParser(self, other)
    def __ror__(self, other):
        return DisjunctiveParser(other, self)
    # No ^^ operator in Python so I overloaded ^ instead
    def __xor__(self, f):
        return Parser(self, f)

class DisjunctiveParser(Parser):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __call__(self, stream):
        if isinstance(self.left, str):
            self.left = globals()[self.left]
        if isinstance(self.right, str):
            self.right = globals()[self.right]
        result = self.left(stream)
        if isinstance(result, Failure):
            result = self.right(stream)
        return result.copy()

class SequentialParser(Parser):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __call__(self, stream):
        if isinstance(self.left, str):
            self.left = globals()[self.left]
        if isinstance(self.right, str):
            self.right = globals()[self.right]
        result1 = self.left(stream)
        if isinstance(result1, Success):
            result2 = self.right(result1.tail)
            if isinstance(result2, Success):
                return Success((result1.value, result2.value), result2.tail)
            else:
                return result2.copy()
        else:
            return result1.copy()

class LiteralParser(Parser):
    def __init__(self, value):
        self.value = value
    def __call__(self, stream):
        length = len(self.value)
        if (length > len(stream)):
            return Failure('Unexpected end of stream')
        else:
            head = stream[:length]
            tail = stream[length:]
            if self.value == head:
                return Success(self.value, tail)
            else:
                return Failure('Expected "' + str(self.value) + '" got "' + str(head) + '"')

parser = LiteralParser(b'01')
print('Combinators,', parser(b'010101'))
print('Combinators,', parser(b'121212'))
parser = SequentialParser(LiteralParser(b'0'), LiteralParser(b'1'))
print('Combinators,', parser(b'010101'))
parser = DisjunctiveParser(LiteralParser(b'01'), LiteralParser(b'12'))
print('Combinators,', parser(b'121212'))
parser = LiteralParser(b'0') + LiteralParser(b'1')
print('Combinators,', parser(b'010101'))
parser = LiteralParser(b'01') | LiteralParser(b'12')
print('Combinators,', parser(b'121212'))

# Spiewak defines these as functions but I think this is unnecessary
# in Python.  I don't know how Scala handles instantiation.

# It turns out that *this* is the right method, because in the GLL
# algorithm parsers need to have identity.
num = (LiteralParser(b'0') | LiteralParser(b'1')) ^ int
print('Combinators,', num(b'010101'))
print('Combinators,', num(b'101010'))

expr = (num + LiteralParser(b'+') + 'expr') ^ (lambda x: x[1] + x[0][0]) | (num + LiteralParser(b'-') + 'expr') ^ (lambda x: x[1] - x[0][0]) | num

print('Combinators,', expr(b'0+1-1+1+1'))
print('Combinators,', expr(b'1+1+1+1+1'))
print('Combinators,', expr(b'0-1-1-1-1'))
print('Combinators,', expr(b'1-1-2'))
print('Combinators,', expr(b'3'))


# This is a hard-coded version of the same recursive-descent parser
# implemented in expr using combinators.

class HardCodedParser:
    def __init__(self, values, operations):
        self.values = values
        self.operations = operations
    def __call__(self, stream):
        return self.expr(stream)
    def num(self, stream):
        if (1 > len(stream)):
            Failure('Unexpected end of stream')
        else:
            head = stream[:1]
            tail = stream[1:]
            if head in self.values:
                return Success(int(head), tail)
            else:
                return Failure('Expected one of "' + str(self.values) + '" got "' + str(head) + '"')
    def expr(self, stream):
        result1 = self.num(stream)
        # print('Combinators,', result1)
        if isinstance(result1, Failure) or result1.tail == b'':
            return result1
        else:
            head = result1.tail[:1]
            tail = result1.tail[1:]
            if head in self.operations:
                result2 = self.expr(tail)
                # print('Combinators,', result2)
                if isinstance(result2, Failure):
                    return result2
                else:
                    return Success(self.operations[head](result2.value, result1.value), result2.tail)
            else:
                return Failure('Expected one of "' + str(self.operations.keys()) + '" got "' + str(head) + '"')

calculator = HardCodedParser(b'01', {b'+' : lambda x, y: x + y, b'-' : lambda x, y: x - y})
print('Hard-coded,', calculator(b'0+1-1+1+1'))
print('Hard-coded,', calculator(b'1+1+1+1+1'))
print('Hard-coded,', calculator(b'0-1-1-1-1'))
print('Hard-coded,', calculator(b'1-1-2'))
print('Hard-coded,', calculator(b'3'))
