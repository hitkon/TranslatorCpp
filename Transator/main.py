import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'for' : 'FOR',
    'int' : 'INT',
    'bool' : 'BOOL',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'string' : "STRING",
    'double' : 'DOUBLE',
    'cout' : 'COUT',
    'std' : 'STD',
    'return' : 'RETURN'
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
    'NOT',
    'AND',
    'OR',
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
t_NOT = r'\!'
t_AND = r'&&'
t_OR = r'\|\|'

t_ignore = ' \t'

t_NAMESPACE = r'\:\:'
t_INCLUDE = r'\#include.*'
t_COMMENT = r'\/\/.*'
t_STR = r'\".*\"'


def t_NUMBER(t):
    r'\d+'
    # t.value = int(t.value)
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


def p_start(p):
    '''start : imports start
     | function start
     | comment start
     | empty'''
    p[0] = ""
    if len(p) > 2:
        p[0] = p[1] + p[2]


def p_index(p):
    """index : variable
    | literal
    | expression"""
    p[0] = p[1]


def p_brackets(p):
    """brackets : LBRACKET index RBRACKET brackets
    | LBRACKET index RBRACKET"""
    p[0] = "[" + p[2] + "]"
    if len(p) > 4:
        p[0] = p[0] + p[4]


def p_array(p):
    """array : ID brackets"""
    p[0] = p[1] + p[2]


def p_variable(p):
    """variable : ID
    | array"""
    p[0] = p[1]


def p_var_assign(p):
    """var_assign : variable ASSIGN expression
    | type variable ASSIGN expression"""
    if len(p) == 4:
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[2] + p[3] + p[4]


def p_if_else_statement(p):
    """if_else_statement : if_statement
    | if_statement else_if_statement else_statement
    | if_statement else_statement"""
    p[0] = ""
    for i in range(1, len(p)):
        p[0] = p[0] + p[i]


def p_if_statement(p):
    """if_statement : IF LPAREN boolean_expr RPAREN LBRACE body RBRACE
    | IF LPAREN boolean_expr RPAREN var_assign SEMICOLON"""
    if len(p) == 7:
        p[0] = "if " + p[3] + ":\n{\n" + p[5] + "\n}\n"
    else:
        p[0] = "if " + p[3] + ":\n{\n" + p[6] + "\n}\n"


def p_else_if_statement(p):
    """else_if_statement : ELSE IF LPAREN boolean_expr RPAREN LBRACE body RBRACE else_if_statement
    | ELSE IF LPAREN boolean_expr RPAREN var_assign SEMICOLON
    | empty"""
    p[0] = ""
    if len(p) == 8:
        p[0] = "elif " + p[4] + ":\n{\n" + p[6] + "}\n"
    else:
        p[0] = "elif " + p[4] + ":\n{\n" + p[7] + "}\n"


def p_else_statement(p):
    """else_statement : ELSE LBRACE body RBRACE
    | ELSE var_assign SEMICOLON"""
    p[0] = p[1]
    if len(p) > 3:
        p[0] = p[0] + ":\n{\n" + p[3] + "}\n"


def p_while_loop(p):
    """while_loop : WHILE LPAREN boolean_expr RPAREN LBRACE body RBRACE"""
    p[0] = p[1] + " " + p[3] + ":\n" + p[5] + "\n" + p[6] + p[7] + "\n"


def p_for_assign(p):
    """for_assign : var_assign COMMA for_assign
    | var_assign
    | empty"""
    p[0] = ""
    if p[1] != "":
        p[0] = p[1] + "\n"
    if len(p) == 4:
        p[0] = p[0] + p[3]


def p_increment(p):
    """increment : variable PLUS PLUS"""
    p[0] = p[1] + "+=1\n"


def p_decrement(p):
    """decrement : variable MINUS MINUS"""
    p[0] = p[1] + "-=1\n"


def p_for_iteration(p):
    """for_iteration : expression COMMA for_iteration
    | expression
    | empty"""
    p[0] = ""
    if p[1] != "":
        p[0] = p[1] + "\n"
    if len(p) == 4:
        p[0] = p[0] + p[3]


def p_for_loop(p):
    """for_loop : FOR LPAREN for_assign SEMICOLON boolean_expr SEMICOLON for_iteration RPAREN LBRACE body RBRACE"""
    p[0] = p[3] + "while " + p[5] + ":\n{\n"  + p[10] + p[7]


def p_comment(p):
    "comment : COMMENT"
    p[0] = ""


def p_imports(p):
    """imports : INCLUDE imports
    | empty """
    if(len(p)> 2):
        #p[0] = p[1] + p[2]
        p[0] = "" + p[2]
    else:
        p[0] = ""


def p_operator(p):
    '''operator : PLUS
    | MINUS
    | DIVIDE
    | TIMES'''
    p[0] = p[1]


def p_bool_op(p):
    '''bool_op : AND
    | OR'''
    if p[1] == '||':
        p[0] = 'or'
    else:
        p[0] = 'and'


def p_compare_op(p):
    '''compare_op : GREATER
    | LESSER
    | EQUAL
    | NOTEQUAL
    | GREATER_EQUAL
    | LESSER_EQUAL'''
    p[0] = p[1]


def p_literal(p):
    '''literal : NUMBER
    | STR
    | bool_literal'''
    p[0] = p[1]


def p_bool_literal(p):
    '''bool_literal : TRUE
    | FALSE'''
    if p[1] == 'true':
        p[0] = 'True'
    else:
        p[0] = 'False'


def p_type(p):
    '''type : INT
    | DOUBLE
    | STRING
    | BOOL'''
    p[0] = p[1]


def p_expression(p):
    '''expression : expression operator expression
    | LPAREN expression RPAREN
    | literal
    | function_call
    | variable'''
    if len(p) == 4:
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]


def p_boolean_expr(p):
    """boolean_expr : expression compare_op expression
    | NOT boolean_expr
    | boolean_expr bool_op boolean_expr
    | LPAREN boolean_expr RPAREN
    | bool_literal"""
    p[0] = ""
    for i in range(1, len(p)):
        p[0] = p[0] + p[i]


def p_function(p):
    """function : type ID LPAREN RPAREN LBRACE body RBRACE"""
    p[0] = "def " + p[2] + "():\n{\n"  + p[6] + "}\n"


def p_call_args(p):
    """call_args : variable COMMA call_args
    | empty"""
    p[0] = ""
    for i in range(1, len(p)):
        p[0] = p[0] + p[i]


def p_function_call(p):
    """function_call : ID LPAREN call_args RPAREN"""
    p[0] = p[1] + p[2] + p[3] + p[4]


def p_empty(p):
    """empty : """
    p[0] = ""


def p_print(p):
    """print : STD NAMESPACE COUT DLEFT expression"""
    p[0] = "print(" + p[5] + ")"

def p_body(p):
    """body : while_loop body
    | for_loop body
    | if_else_statement body
    | expression SEMICOLON body
    | var_assign SEMICOLON body
    | print SEMICOLON body
    | increment SEMICOLON body
    | decrement SEMICOLON body
    | empty"""
    p[0] = ""
    if len(p) == 3:
        p[0] = p[1] + p[2]
    if len(p) == 4:
        p[0] = p[1] + "\n" + p[3]


def p_error(p):
    print(parser.token())
    if p:
        print("Syntax error at token", p)
        # Just discard the token and tell the parser it's okay.
        parser.errok()
    else:
        print("Syntax error at EOF")

# Build the lexer
lexer = lex.lex()
parser = yacc.yacc()

f = open("ConsoleApplication1.cpp", "rt")
out = open("out.py", "w")
# f = open("RurkiHomeTask.cpp")
lines = f.read()
print(lines)
lexer.input(lines)
# while True:
#     tok = lexer.token()
#     if not tok:
#         break  # No more input
#     print(tok.type, tok.value, tok.lineno, tok.lexpos)

result = parser.parse(lines)
out.write(result)
print(result)
f.close()
