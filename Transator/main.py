import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'for' : 'FOR',
    'int' : 'INT',
    'string' : "STRING",
    'double' : 'DOUBLE',
    'main' : 'MAIN',
    'cout' : 'COUT',
    'function' : 'FUNCTION',
    'std' : 'STD'
}

tokens = [
    'NUMBER',
    'NAMESPACE',
    'ID',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'LBRACKET',
    'RBRACKET',
    'SEMICOLON',
    'COMMENT',
    'INCLUDE',
    'DLEFT',
    'DRIGHT',
    'EQUAL',
    'NOTEQUAL',
    'GREATER',
    'LESSER',
    'GREATER_EQUAL',
    'LESSER_EQUAL',
    'ASSIGN',
    'STR',
    'COMMA',
    'DOT',

] + list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_DLEFT = r'\<\<'
t_DRIGHT = r'\>\>'
t_EQUAL = r'\=='
t_GREATER = r'\>'
t_LESSER = r'\<'
t_GREATER_EQUAL = r'\>\='
t_LESSER_EQUAL = r'\<\='
t_ASSIGN = r'\='
t_COMMA = r'\,'
t_DOT = r'\.'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_NOTEQUAL = r'!='


t_ignore  = ' \t'

t_NAMESPACE = r'\:\:'
t_INCLUDE = r'\#include.*'
t_COMMENT = r'\/\/.*'
t_STR = r'\".*\"'

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

f = open("ConsoleApplication1.cpp")
# f = open("RurkiHomeTask.cpp")
lines = f.read()
print(lines)
lexer.input(lines)
while True:
    tok = lexer.token()
    if not tok:
        break  # No more input
    print(tok.type, tok.value, tok.lineno, tok.lexpos)

