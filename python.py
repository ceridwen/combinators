import argparse
import token
import tokenize
import types

import continuation_gll_combinators as combinators


class Token(combinators.Terminal):
    def __init__(self, token, **kws):
        vars(self)['token'] = token
        vars(self)['combinators'] = (self,)

    def _parse(self, trampoline, success, failure, stream, index):
        if len(stream) - index <= 0:
            return failure('Unexpected end of stream (expected %r)' % self.token, index)
        elif stream[index][0] != self.token:
            return failure('Expected %r got %r' % (self.token, stream[index]), index)
        else:
            result = stream[index]
            index += 1
            return success(result, failure, index)

    def __str__(self):
        return 'Token(%s)' % self.token
    __repr__ = __str__


class KeywordOrOp(Token):
    def __init__(self, token, string, **kws):
        super(KeywordOrOp, self).__init__(token, **kws)
        vars(self)['string'] = string

    def _parse(self, trampoline, success, failure, stream, index):
        # Because this is in continuation-passing style, there's not
        # really a good way to reuse the code from Token._parse().
        if len(stream) - index <= 0:
            return failure('Unexpected end of stream (expected %r)' % self.token, index)
        elif stream[index][0] != self.token or stream[index][1] != self.string:
            return failure('Expected (%r, %r) got %r' % (self.token, self.string, stream[index]), index)
        else:
            result = stream[index]
            index += 1
            return success(result, failure, index)

    def __str__(self):
        return 'KeywordOrOp(%s, %s)' % (self.token, self.string)
    __repr__ = __str__


# The names of all the tokens are taken from the token.tok_name
# dictionary.  Note that Token(op) is never used.  As the tokenize
# documentation states, "To simplify token stream handling, all
# Operators and Delimiters tokens are returned using the generic
# token.OP token type."  (It certainly doesn't simplify parsing.)
# Keywords also always have token.NAME as their first value.  The
# lists of lists of keywords, delimiters, and operators are taken from
# https://docs.python.org/2/reference/lexical_analysis.html
TOKENS = types.MappingProxyType({name: Token(name) for name in token.tok_name.values()})

KEYWORDS = types.MappingProxyType({keyword: KeywordOrOp('NAME', keyword) for keyword in ('and', 'del', 'from', 'not', 'while', 'as', 'elif', 'global', 'or', 'with', 'assert', 'else', 'if', 'pass', 'yield', 'break', 'except', 'import', 'print', 'class', 'exec', 'in', 'raise', 'continue', 'finally', 'is', 'return', 'def', 'for', 'lambda', 'try')})

OPS = types.MappingProxyType({op: KeywordOrOp('OP', op) for op in ('+', '-', '*', '**', '/', '//', '%', '<<', '>>', '&', '|', '^', '~', '<', '>', '<=', '>=', '==', '!=', '<>', '(', ')', '[', ']', '{', '}', '@', ',', ':', '.', '`', '=', ';', '+=', '-=', '*=', '/=', '//=', '%=', '&=', '|=', '^=', '>>=', '<<=', '**=')})

# tokenize generates a different token for newlines that occur after
# code and newlines on blank lines, but the grammar doesn't take
# account of the difference, so this combinator represents both.

newline = TOKENS['NEWLINE'] | TOKENS['NL']


class Succeed(combinators.Combinator):
    def __init__(self, **kws):
        pass

    def _parse(self, trampoline, success, failure, stream, index):
        return success('', failure, index)

def star(combinator):
    _ = Succeed() | (combinator + combinators.Lazy(lambda: _))
    return _

def plus(combinator):
    _ = combinator + (combinators.Lazy(lambda: _) | Succeed())
    return _

def option(combinator):
    return (combinator | Succeed())

def ops_alternation(*strings):
    return combinators.Alternation(*[OPS[string] for string in strings])

def max_result(results):
    max_result = combinators.Success('', '', -1)
    for result in results:
        if result.index > max_result.index:
            max_result = result
    return max_result


# The grammar is taken from
# https://docs.python.org/2/reference/grammar.html .  The order is
# changed because Python's eager evaluation means that lower-level
# entries have to occur first.

# comp_op = OPS['<'] | OPS['>'] | OPS['=='] | OPS['>='] | OPS['<='] | OPS['<>'] | OPS['!='] | KEYWORDS['in'] | KEYWORDS['not'] + KEYWORDS['in'] | KEYWORDS['is'] | KEYWORDS['is'] + KEYWORDS['not']

single_input = newline | combinators.Lazy(lambda: simple_stmt) | combinators.Lazy(lambda: compound_stmt + newline)
file_input = star(newline | combinators.Lazy(lambda: stmt)) + TOKENS['ENDMARKER']
eval_input = combinators.Lazy(lambda: testlist) + star(newline) + TOKENS['ENDMARKER']

augassign = ops_alternation('+=', '-=', '*=', '/=', '%=', '&=', '|=',
                            '^=', '<<=', '>>=', '**=', '//=')
expr_stmt = combinators.Lazy(lambda: testlist) + (augassign + (combinators.Lazy(lambda: yield_expr) | combinators.Lazy(lambda: testlist)) | star(OPS['='] + (combinators.Lazy(lambda: yield_expr) | combinators.Lazy(lambda: testlist))))

print_stmt = KEYWORDS['print'] + option(combinators.Lazy(lambda: test) + star(OPS[',']) + option(OPS[','])) | OPS['>>'] + combinators.Lazy(lambda: test) + option(plus(OPS[','] + combinators.Lazy(lambda: test)) + option(OPS[',']))
del_stmt = KEYWORDS['del'] + combinators.Lazy(lambda: exprlist)
pass_stmt = KEYWORDS['pass']
break_stmt = KEYWORDS['break']
continue_stmt = KEYWORDS['continue']
return_stmt = KEYWORDS['return'] + option(combinators.Lazy(lambda: testlist))
yield_expr = KEYWORDS['yield'] + option(combinators.Lazy(lambda: testlist))
yield_stmt = yield_expr
raise_stmt = KEYWORDS['raise'] + option(combinators.Lazy(lambda: test) + option(OPS[','] + option(OPS[','] + combinators.Lazy(lambda: test))))
flow_stmt = break_stmt | continue_stmt | return_stmt | yield_stmt

dotted_name = TOKENS['NAME'] + star(OPS['.'] + TOKENS['NAME'])
dotted_as_name = dotted_name + option(KEYWORDS['as'] + TOKENS['NAME'])
dotted_as_names = dotted_as_name + star(OPS[','] + dotted_as_name)

decorator = OPS['@'] + dotted_name + option(OPS['('] + option(combinators.Lazy(lambda: arglist)) + OPS[')']) + newline
decorators = plus(decorator)
decorated = decorators + (combinators.Lazy(lambda: classdef) | combinators.Lazy(lambda: funcdef))
parameters = OPS['('] + option(combinators.Lazy(lambda: varargslist)) + OPS[')']
funcdef = KEYWORDS['def'] + TOKENS['NAME'] + parameters + OPS[':'] + combinators.Lazy(lambda: suite)
fpdef = TOKENS['NAME'] |  OPS['('] + combinators.Lazy(lambda: fplist) + OPS[')']
fplist = fpdef + star(OPS[','] + fpdef) + option(OPS[','])
varargslist = (star(fpdef + option(OPS['='] + combinators.Lazy(lambda: test)) + OPS[',']) +
               (OPS['*'] + TOKENS['NAME'] + option(OPS[','] + OPS['**'] + TOKENS['NAME']) | OPS['**'] + TOKENS['NAME'] |
                fpdef + option(OPS['='] + combinators.Lazy(lambda: test)) + star(OPS[','] + fpdef + option(OPS['='] + combinators.Lazy(lambda: test))) + OPS[',']))

import_name = KEYWORDS['import'] + dotted_as_names
import_as_name = TOKENS['NAME'] + option(KEYWORDS['as'] + TOKENS['NAME'])
import_as_names = import_as_name + star(OPS[','] + import_as_name) + option(OPS[','])
import_from = (KEYWORDS['from'] + (star(OPS['.']) + dotted_name | plus(OPS['.'])) +
               KEYWORDS['import'] + (OPS['*'] | OPS['('] + import_as_names + OPS[')']) | import_as_names)
import_stmt = import_name | import_from
global_stmt = KEYWORDS['global'] + TOKENS['NAME'] + option(OPS[','] + TOKENS['NAME'])
exec_stmt = KEYWORDS['exec'] + combinators.Lazy(lambda: expr) + KEYWORDS['in'] + option(KEYWORDS['in'] + combinators.Lazy(lambda: test) + option(OPS[','] + combinators.Lazy(lambda: test)))
assert_stmt = KEYWORDS['assert'] + combinators.Lazy(lambda: test) + option(OPS[','] + combinators.Lazy(lambda: test))

small_stmt = expr_stmt | print_stmt | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | exec_stmt | assert_stmt
simple_stmt = small_stmt + star(OPS[';'] + small_stmt) + option(OPS[';']) + newline

suite = simple_stmt | newline + TOKENS['INDENT'] + plus(combinators.Lazy(lambda: stmt)) + TOKENS['DEDENT']

if_stmt = KEYWORDS['if'] + combinators.Lazy(lambda: test) + OPS[':'] + suite + star(KEYWORDS['elif'] + combinators.Lazy(lambda: test) + OPS[':'] + suite) + option(KEYWORDS['else'] + OPS[':'] + suite)
while_stmt = KEYWORDS['while'] + combinators.Lazy(lambda: test) + OPS[':'] + suite + option(KEYWORDS['else'] + OPS[':'] + suite)
for_stmt = KEYWORDS['for'] + combinators.Lazy(lambda: exprlist) + KEYWORDS['in'] + combinators.Lazy(lambda: testlist) + OPS[':'] + suite + option(KEYWORDS['else'] + OPS[':'] + suite)
except_clause = KEYWORDS['except'] + option(combinators.Lazy(lambda: test) + option(KEYWORDS['as'] | OPS[',']) + combinators.Lazy(lambda: test))
try_stmt = (KEYWORDS['try'] + OPS[':'] + suite +
            ((plus(except_clause + OPS[':'] + suite)) +
             option(KEYWORDS['else'] + OPS[':'] + suite) +
             option(KEYWORDS['finally'] + OPS[':'] + suite) |
             KEYWORDS['finally'] + OPS[':'] + suite))
with_item = combinators.Lazy(lambda: test) + option(KEYWORDS['as'] + combinators.Lazy(lambda: expr))
with_stmt = KEYWORDS['with'] + with_item + star(OPS[','] + with_item) + OPS[':'] + suite

compound_stmt = if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | combinators.Lazy(lambda: classdef) | decorated
stmt = simple_stmt | compound_stmt

old_test = combinators.Lazy(lambda: or_test) | combinators.Lazy(lambda: old_lambdef)
testlist_safe = old_test + option(plus(OPS[',']+ old_test) + OPS[','])
old_lambdef = KEYWORDS['lambda'] + option(varargslist) + OPS[':'] + old_test

comp_if = KEYWORDS['if'] + old_test + option(combinators.Lazy(lambda: comp_iter))
comp_for = KEYWORDS['for'] + combinators.Lazy(lambda: exprlist) + KEYWORDS['in'] + combinators.Lazy(lambda: or_test) + option(combinators.Lazy(lambda: comp_iter))
comp_iter = comp_for | comp_if

list_if = KEYWORDS['if'] + old_test + option(combinators.Lazy(lambda: list_iter))
list_for = KEYWORDS['for'] + combinators.Lazy(lambda: exprlist) + KEYWORDS['in'] + testlist_safe + option(combinators.Lazy(lambda: list_iter))
list_iter = list_for | list_if

argument = combinators.Lazy(lambda: test) + option(comp_for) | combinators.Lazy(lambda: test) + OPS['='] + combinators.Lazy(lambda: test)
arglist = star(argument + OPS[',']) + (argument + OPS[','] | OPS['*'] + combinators.Lazy(lambda: test) + star(OPS[','] + argument) + option(OPS[','] + OPS['**'] + combinators.Lazy(lambda: test)) | OPS['**'] + combinators.Lazy(lambda: test))
classdef = KEYWORDS['class'] + TOKENS['NAME'] + option(OPS['('] + option(combinators.Lazy(lambda: testlist))) + OPS[')'] + OPS[':'] + suite
dictorsetmaker = (combinators.Lazy(lambda: test) + OPS[':'] + combinators.Lazy(lambda: test) + (comp_for | (star(OPS[','] + combinators.Lazy(lambda: test)) + OPS[',']))) | (combinators.Lazy(lambda: test) + (comp_for | (star(OPS[','] + combinators.Lazy(lambda: test)) + OPS[','])))
testlist = combinators.Lazy(lambda: test) + star(OPS[','] + combinators.Lazy(lambda: test)) + option(OPS[','])
exprlist = combinators.Lazy(lambda: expr) + star(OPS[','] + combinators.Lazy(lambda: expr)) + option(OPS[','])
sliceop = OPS[':'] + option(combinators.Lazy(lambda: test))
subscript = (OPS['.'] + OPS['.'] + OPS['.']) | combinators.Lazy(lambda: test) | option(combinators.Lazy(lambda: test)) + OPS[':'] + option(combinators.Lazy(lambda: test)) + option(sliceop)
subscriptlist = subscript + star(OPS[','] + subscript) + OPS[',']
trailer = (OPS['('] + option(arglist) + OPS[')']) | (OPS['['] + subscriptlist + OPS[']']) | (OPS['.'] + TOKENS['NAME'])
lambdef = KEYWORDS['lambda'] + option(varargslist) + OPS[':'] + combinators.Lazy(lambda: test)
testlist_comp = combinators.Lazy(lambda: test) + (comp_for | star(OPS[','] + combinators.Lazy(lambda: test)) + OPS[','])
listmaker = combinators.Lazy(lambda: test) + (list_for | (star(OPS[','] + combinators.Lazy(lambda: test)) + OPS[',']))
atom = ((OPS['('] + option(yield_expr | testlist_comp) + OPS[')']) |
        option(OPS['['] + listmaker + OPS[']']) |
        option(OPS['{'] + dictorsetmaker + OPS['}']) |
        OPS['`'] + combinators.Lazy(lambda: testlist1) + OPS['`'] |
        TOKENS['NAME'] | TOKENS['NUMBER'] | plus(TOKENS['STRING']))
factor = (ops_alternation('+', '-', '~') + combinators.Lazy(lambda: factor)) | combinators.Lazy(lambda: power)
power = atom + star(trailer) + option(OPS['**'] + combinators.Lazy(lambda: factor))
term = factor + star(ops_alternation('*', '/', '%', '//') + factor)
arith_expr = term + star((OPS['+'] | OPS['-']) + term)
shift_expr = arith_expr + star((OPS['<<'] | OPS['>>']) + arith_expr)
and_expr = shift_expr + star(OPS['&'] + shift_expr)
xor_expr = and_expr + star(OPS['^'] + and_expr)
expr = xor_expr + star(OPS['|'] + xor_expr)
comparison = expr + star(combinators.Lazy(lambda: comp_op) + expr)
comp_op = ops_alternation('<', '>', '==', '>=', '<=', '<>', '!=') | KEYWORDS['in'] | KEYWORDS['not'] + KEYWORDS['in'] | KEYWORDS['is'] | KEYWORDS['is'] + KEYWORDS['not']
not_test = (KEYWORDS['not'] + combinators.Lazy(lambda: not_test)) | comparison
and_test = not_test + star(KEYWORDS['and'] + not_test)
or_test = and_test + star(KEYWORDS['or'] + and_test)
test = (or_test + option(KEYWORDS['if'] + or_test + KEYWORDS['else'] + combinators.Lazy(lambda: test))) | lambdef

testlist1 = test + star(OPS[','] + test)

if __name__ == '__main__':
    arg_parser = argparse.ArgumentParser(description='Parse Python code.')
    arg_parser.add_argument('file', nargs='?',
                            default='python.py', help='Python file.')
    f = open(arg_parser.parse_args().file, 'r')
    tokens = [(token.tok_name[t[0]], t[1]) for  t in tokenize.generate_tokens(f.readline)]
    # print(tokens)

    result = max_result(file_input.parse(tokens))
    print(result)
    # print(result.index)
    # print(tokens[result.index - 20:result.index + 20])
    # print(''.join(t[1] for t in tokens[result.index - 20:result.index + 20]))
    # if result.index == 0:
    #     print(tokens)
