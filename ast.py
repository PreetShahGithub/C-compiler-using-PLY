import ply.lex as lex
import ply.yacc as yacc
import sys
import math
# List of token names. This is always required
reserved = ('INCLUDE', 'STDIOH','STDLIBH', 'STRINGH', 'MAIN', 'CHAR', 'DOUBLE', 'FLOAT', 'INT', 'LONG', 'SHORT', 'VOID', 'DO', 'WHILE', 'IF', 'ELSE', 'RETURN', 'BREAK', 'CONTINUE','PRINTF',)

tokens = reserved + (
    'ID','ICONST', 'FCONST', 'SCONST',
    'LOR', 'LAND',
    'LE', 'GE', 'EQ', 'NE',
    'TIMESEQUAL', 'DIVEQUAL', 'MODEQUAL', 'PLUSEQUAL', 'MINUSEQUAL',
    'PLUSPLUS', 'MINUSMINUS',
    'ARROW',
)

literals = ['(', ')','[', ']','{', '}', ',', '.', ';','+', '-', '*', '/', '%',
    '|', '&', '~', '^','#','<','>','=']

t_ignore = ' \t\x0c'

# Scope variable
scope = 0
scope_list = {}
multiplier = 10
decimal_places = -1
tindex = -1
symbol_table = {}
quad_table = []
# Comments
def t_singlecomment(t):
    r'//(.)*'
    #t.lexer.lineno += 1

def t_multicomment(t):
    r'/\*(.|\n)*?\*/'
    #t.lexer.lineno += t.value.count('\n')

def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')

def t_FCONST(t):
    #r'(\d+)(\.\d+) | ((\d+)(\.\d+)(e(\+|-)?(\d+))? | (\d+)e(\+|-)?(\d+))([lL]|[fF])?'
    r'\-?\d+\.\d+'
    t.value = float(t.value)
    return t

def t_ICONST(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_TIMESEQUAL(t):
    r'\*='
    t.type = 'TIMESEQUAL'
    return t

def t_DIVEQUAL(t):
    r'/='
    t.type = 'DIVEQUAL'
    return t

def t_MODEQUAL(t):
    r'%='
    t.type = 'MODEQUAL'
    return t

def t_PLUSEQUAL(t):
    r'\+='
    t.type = 'PLUSEQUAL'
    #print('-----------')
    return t

def t_MINUSEQUAL(t):
    r'-='
    t.type = 'MINUSEQUAL'
    return t

def t_MINUSMINUS(t):
    r'--'
    t.type = 'MINUSMINUS'
    return t

def t_PLUSPLUS(t):
    r'\+\+'
    t.type = 'PLUSPLUS'
    return t

def t_ARROW(t):
    r'->'
    t.type = 'ARROW'
    return t

def t_PLUS(t):
    r'\+'
    t.type = '+'
    return t

def t_MINUS(t):
    r'\-'
    t.type = '-'
    return t

def t_TIMES(t):
    r'\*'
    t.type = '*'
    return t

def t_DIVIDE(t):
    r'/'
    t.type = '/'
    return t

def t_MOD(t):
    r'\%'
    t.type = '%'
    return t

def t_OR(t):
    r'\|'
    t.type = '|'
    return t

def t_AND(t):
    r'\&'
    t.type = '&'
    return t

def t_NOT(t):
    r'\~'
    t.type = '~'
    return t

def t_XOR(t):
    r'\^'
    t.type = '^'
    return t

def t_LOR(t):
    r'\|\|'
    t.type = 'LOR'
    return t

def t_LAND(t):
    r'&&'
    t.type = 'LAND'
    return t

def t_LNOT(t):
    r'\!'
    t.type = 'LNOT'
    return t

def t_LT(t):
    r'<'
    t.type = '<'
    return t

def t_GT(t):
    r'>'
    t.type = '>'
    return t

def t_LE(t):
    r'<='
    t.type = 'LE'
    return t

def t_GE(t):
    r'>='
    t.type = 'GE'
    return t

def t_EQ(t):
    r'=='
    t.type = 'EQ'
    return t

def t_NE(t):
    r'!='
    t.type = 'NE'
    return t

def t_EQUALS(t):
    r'='
    t.type = '='
    return t


def t_LPAREN(t):
    r'\('
    t.type = '('
    return t

def t_RPAREN(t):
    r'\)'
    t.type = ')'
    return t

def t_LBRACET(t):
    r'\['
    t.type = '['
    return t

def t_RBRACKET(t):
    r'\]'
    t.type = ']'
    return t

def t_LBRACE(t):
    r'\{'
    t.type = '{'
    global multiplier
    global scope
    global scope_list
    global decimal_places
    decimal_places+=1
    multiplier /= 10
    if(scope in scope_list):
        scope_list[scope] += multiplier
        scope_list[scope] = round(scope_list[scope],decimal_places)
    else:
        scope_list[scope] = scope + multiplier
        scope_list[scope] = round(scope_list[scope],decimal_places)
        scope = scope + multiplier
    if scope not in symbol_table:
        symbol_table[scope]={}
    #print('==============scope=',t,scope,scope_list,multiplier)

    return t

def t_RBRACE(t):
    r'\}'
    t.type = '}'
    global multiplier
    global scope
    global decimal_places
    multiplier*=10
    decimal_places-=1
    scope = scope - scope%(multiplier)
    #print('==============scope=',t,scope,scope_list,multiplier)
    return t

def t_COMMA(t):
    r','
    t.type = ','
    return t

def t_PERIOD(t):
    r'\.'
    t.type = '.'
    return t

def t_SEMI(t):
    r';'
    t.type = ';'
    return t

def t_STDIOH(t):
    r'stdio.h'
    t.type = 'STDIOH'
    return t

def t_STDLIBH(t):
    r'stdlib.h'
    t.type = 'STDLIBH'
    return t

def t_STRINGH(t):
    r'string.h'
    t.type = 'STRINGH'
    return t

def t_INCLUDE(t):
    r'include'
    t.type = 'INCLUDE'
    return t

def t_MAIN(t):
    r'main'
    t.type = 'MAIN'
    return t

def t_PRINTF(t):
    r'printf'
    t.type = 'PRINTF'
    return t

# Identifiers and reserved words
for r in reserved:
    symbol_table[r.lower()] = {}
    symbol_table[r.lower()]['type'] = "keyword"

def t_ID(t):
    r'[A-Za-z$_][A-Za-z$_0-9]*'
    global scope
    global scope_list
    global decimal_places
    if t.value in symbol_table:
        if(symbol_table[t.value]["type"] == "keyword"):
            t.type = t.value.upper()
            symbol_table[t.value]["token"] = t
    elif(scope in symbol_table and t.value in symbol_table[scope] and symbol_table[scope][t.value]["type"] == "identifier"):
            symbol_table[scope][t.value]["line_nos"].append(t.lineno)
    elif(scope in symbol_table):
        if(len(t.value)>32):
            print("ERROR: variable name has to be <= 32 characters\n",t.value)
            return t
        #print("***********",t.value)
        symbol_table[scope][t.value] = {}
        symbol_table[scope][t.value]["type"] = "identifier"
        symbol_table[scope][t.value]["token"] = t
        symbol_table[scope][t.value]["scope"] = scope
        symbol_table[scope][t.value]["value"] = 0
        symbol_table[scope][t.value]["tspecifier"] = "none"
        tempscope = scope
        tempdec = decimal_places
        while(tempscope != 0.0):
            tempscope = truncate(tempscope,tempdec-1)
            if(tempscope in symbol_table and t.value in symbol_table[tempscope]):
                symbol_table[scope][t.value]["tspecifier"] = symbol_table[tempscope][t.value]["tspecifier"]
                symbol_table[scope][t.value]["value"] = symbol_table[tempscope][t.value]["value"]
                break
            tempdec-=1
        symbol_table[scope][t.value]["line_nos"] = [t.lineno]
        #print('========================',t,scope,scope_list)
    return t

t_SCONST = r'\"([^\\\n]|(\\.))*?\"'
#t_CCONST = r'(L)?\'([^\\\n]|(\\.))*?\''


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def flatten(l):
    output = []
    def removeNestings(l):
        for i in l:
            if type(i) == list:
                removeNestings(i)
            else:
                output.append(i)
    if type(l) == list:
        removeNestings(l)
    else:
        output.append(l)
    return output

def retrieve(t):
    global scope
    global decimal_places
    #print("retrieve: ",t,"scope,multiplier,decimal_places:",scope,multiplier,decimal_places)
    tempscope=scope
    tempdec = decimal_places
    while(tempdec!=-1):
        if tempscope in symbol_table and t in symbol_table[tempscope]:
            if symbol_table[scope][t]['tspecifier'] != 'none':
                t = symbol_table[scope][t]['value']
                break
            else:
                print("error line:",symbol_table[scope][t]["token"],"   rhs = ", t)
        tempscope = truncate(tempscope,tempdec-1)
        tempdec-=1
    return t


def truncate(number, digits):
    stepper = pow(10.0, digits)
    return math.trunc(stepper * number) / stepper

def p_error(p):
    stack_state_str = ' '.join([symbol.type for symbol in parser.symstack][1:])
    print('Syntax error in input! Parser State:{} {} . {}'
          .format(parser.state,
                  stack_state_str,
                  p))

###################################################################################################################
'''precedence = (
    ('left','+','-'),
    ('left','*','/'),
    )'''

def p_start(p):
    '''start : '#' INCLUDE '<' lib '>' next'''
    p[0] = p[1:]

def p_translation_unit(p):
    '''translation_unit : external_declaration
    | translation_unit external_declaration'''
    p[0] = p[1:]

def p_next(p):
    '''next : start
    | translation_unit'''
    p[0] = p[1:]

def p_lib(p):
        '''lib : STDIOH
    | STDLIBH
    | STRINGH'''
        p[0] = p[1:]

def p_primary_expression(p):
    '''primary_expression : ID
    | constant
    | string
    | '(' expression ')' '''
    p[0] = p[1:]

def p_constant(p):
    '''constant : ICONST
    | FCONST'''
    p[0] = p[1:]

def p_string(p):
    '''string : SCONST'''
    p[0] = p[1:]

def p_postfix_expression(p):
    '''postfix_expression : primary_expression
    | postfix_expression '[' expression ']'
    | postfix_expression '(' ')'
    | postfix_expression '(' argument_expression_list ')'
    | postfix_expression ARROW ID
    | postfix_expression PLUSPLUS
    | postfix_expression MINUSMINUS
    | '(' type_specifier ')' '{' initializer_list '}'
    | '(' type_specifier ')' '{' initializer_list ',' '}' '''
    #print(len(p),p[1:])
    if(len(p)==3):
        term = flatten(p[1])[0]
        if term in symbol_table[scope]:
            if(flatten(p[2])[0] == '++'):
                symbol_table[scope][term]['value'] += 1
            elif(flatten(p[2])[0] == '--'):
                symbol_table[scope][term]['value'] -= 1
    else:
        p[0] = p[1:]

def p_argument_expression_list(p):
    '''argument_expression_list : assignment_expression
    | argument_expression_list ',' assignment_expression'''
    p[0] = p[1:]

def p_unary_expression(p):
    '''unary_expression : postfix_expression
    | PLUSPLUS unary_expression
    | MINUSMINUS unary_expression
    | unary_operator unary_expression'''
    #print(len(p),p[1:])
    if(len(p)==3):
        term = flatten(p[2])[0]
        if term in symbol_table[scope]:
            if(flatten(p[1])[0] == '++'):
                symbol_table[scope][term]['value'] += 1
            elif(flatten(p[1])[0] == '--'):
                symbol_table[scope][term]['value'] -= 1
    else:
        p[0] = p[1:]
    p[0] = p[1:]

def p_unary_operator(p):
    '''unary_operator : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!' '''
    p[0] = p[1:]

def p_multiplicative_expression(p):
    '''multiplicative_expression : unary_expression
    | multiplicative_expression '*' unary_expression
    | multiplicative_expression '/' unary_expression
    | multiplicative_expression '%' unary_expression'''
    global tindex
    if(len(p)==4):
        if(flatten(p[2])[0] == '*'):
            p[0] = retrieve(flatten(p[1])[0]) * retrieve(flatten(p[3])[0])
        elif(flatten(p[2])[0] == '/'):
            if(type(flatten(p[3])[0]) == tuple):
                tindex+=1
                p[0] = (tindex,retrieve(flatten(p[1])[0]) / retrieve(flatten(p[3])[0][1]))
                if(type(retrieve(flatten(p[1])[0])) == int and type(retrieve(flatten(p[3])[0][1])) == int):
                    p[0] = (tindex,int(retrieve(flatten(p[1])[0]) / retrieve(flatten(p[3])[0][1])))
                #print("{: <20} {: <20} {: <20} {: <20}".format('/',flatten(p[1])[0],'t' + str(flatten(p[3])[0][0]),'t'+str(tindex)))
                ll = ['/',flatten(p[1])[0],'t' + str(flatten(p[3])[0][0]),'t'+str(tindex)]
                quad_table.append(l1)
            else:
                tindex+=1
                #print('t',tindex,' = ',flatten(p[1])[0],' + ',flatten(p[3])[0])
                #print("{: <20} {: <20} {: <20} {: <20}".format('/',flatten(p[1])[0],'t' + str(flatten(p[3])[0]),'t'+str(tindex)))
                ll = ['/',flatten(p[1])[0],'t' + str(flatten(p[3])[0]),'t'+str(tindex)]
                quad_table.append(ll)
                p[0] = (tindex,retrieve(flatten(p[1])[0]) / retrieve(flatten(p[3])[0]))
                if(type(retrieve(flatten(p[1])[0])) == int and type(retrieve(flatten(p[3])[0])) == int):
                    p[0] = (tindex,int(retrieve(flatten(p[1])[0]) / retrieve(flatten(p[3])[0])))
        else:
            p[0] = retrieve(flatten(p[1])[0]) % retrieve(flatten(p[3])[0])
    else:
        p[0] = p[1:]


def p_additive_expression(p):
    '''additive_expression : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression'''
    global tindex
    if(len(p)==4):
        if(flatten(p[2])[0] == '+'):
            if(type(flatten(p[3])[0]) == tuple):
                tindex+=1
                p[0] = (tindex,retrieve(flatten(p[1])[0]) + retrieve(flatten(p[3])[0][1]))
                #print('t',tindex,' = ',flatten(p[1])[0],' + t',flatten(p[3])[0][0])
                #print("{: <20} {: <20} {: <20} {: <20}".format('+',flatten(p[1])[0],'t' + str(flatten(p[3])[0][0]),'t'+str(tindex)))
                ll = ['+',flatten(p[1])[0],'t' + str(flatten(p[3])[0][0]),'t'+str(tindex)]
                quad_table.append(l1)
            else:
                tindex+=1
                #print('t',tindex,' = ',flatten(p[1])[0],' + ',flatten(p[3])[0])
                #print("{: <20} {: <20} {: <20} {: <20}".format('+',flatten(p[1])[0],'t' + str(flatten(p[3])[0]),'t'+str(tindex)))
                ll = ['+',flatten(p[1])[0],'t' + str(flatten(p[3])[0]),'t'+str(tindex)]
                quad_table.append(l1)
                p[0] = (tindex,retrieve(flatten(p[1])[0]) + retrieve(flatten(p[3])[0]))
        else:
            p[0] = retrieve(flatten(p[1])[0]) - retrieve(flatten(p[3])[0])
        #print(p[1:])
    else:
        p[0] = p[1:]

def p_relational_expression(p):
    '''relational_expression : additive_expression
    | relational_expression '<' additive_expression
    | relational_expression '>' additive_expression
    | relational_expression LE additive_expression
    | relational_expression GE additive_expression'''
    if(len(p) == 4):
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])    
        if(flatten(p[2]) =='<'):
            p[0] = term1 < term2
        elif(flatten(p[2]) =='>'):
            p[0] = term1 > term2
        elif(flatten(p[2]) =='LE'):
            p[0] = term1 <= term2
        elif(flatten(p[2]) =='GE'):
            p[0] = term1 >= term2
    else:
        p[0] = p[1:]

def p_equality_expression(p):
    '''equality_expression : relational_expression
    | equality_expression EQ relational_expression
    | equality_expression NE relational_expression'''
    if(len(p) == 4):
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])    
        if(flatten(p[2]) =='EQ'):
            p[0] = term1 == term2
        elif(flatten(p[2]) =='NE'):
            p[0] = term1 != term2
    else:
        p[0] = p[1:]

def p_and_expression(p):
    '''and_expression : equality_expression
    | and_expression '&' equality_expression'''
    if len(list(p))==4:
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])
        p[0] = term1 & term2
    else:
        p[0] = p[1:]

def p_exclusive_or_expression(p):
    '''exclusive_or_expression : and_expression
    | exclusive_or_expression '^' and_expression'''
    if len(list(p))==4:
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])
        p[0] = term1 ^ term2
    else:
        p[0] = p[1:]

def p_inclusive_or_expression(p):
    '''inclusive_or_expression : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression'''
    if len(list(p))==4:
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])
        p[0] = term1 | term2
    else:
        p[0] = p[1:]

def p_logical_and_expression(p):
    '''logical_and_expression : inclusive_or_expression
    | logical_and_expression LAND inclusive_or_expression'''
    if len(list(p))==4:
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])
        p[0] = term1 and term2
    else:
        p[0] = p[1:]
    p[0] = p[1:]

def p_logical_or_expression(p):
    '''logical_or_expression : logical_and_expression
    | logical_or_expression LOR logical_and_expression'''
    if len(list(p))==4:
        term1 = retrieve(flatten(p[1])[0])
        term2 = retrieve(flatten(p[3])[0])
        p[0] = term1 or term2
    else:
        p[0] = p[1:]


def p_assignment_expression(p):
    '''assignment_expression : logical_or_expression
    | unary_expression assignment_operator assignment_expression'''
    #print('p=',len(p),p[1:])
    if(len(p)==4):
        term = flatten(p[1])[0]
        if term in symbol_table[scope]:
            if(flatten(p[2])[0] == '='):
                #print(flatten(p[3])[0],type(flatten(p[3])[0]))
                if(type(flatten(p[3])[0]) == tuple):
                    symbol_table[scope][term]['value'] = flatten(p[3])[0][1]
                    print("{: <20} {: <20} {: <20} {: <20}".format('=','t' + str(flatten(p[3])[0][0])," ",term))
                else:
                    symbol_table[scope][term]['value'] = flatten(p[3])[0]
                    print("{: <20} {: <20} {: <20} {: <20}".format('=',flatten(p[3])[0]," ",term))
            elif(flatten(p[2])[0] == '*='):
                symbol_table[scope][term]['value'] *= flatten(p[3])[0]
            elif(flatten(p[2])[0] == '/='):
                symbol_table[scope][term]['value'] /= flatten(p[3])[0]
            elif(flatten(p[2])[0] == '%='):
                symbol_table[scope][term]['value'] %= flatten(p[3])[0]
            elif(flatten(p[2])[0] == '+='):
                symbol_table[scope][term]['value'] += flatten(p[3])[0]
            elif(flatten(p[2])[0] == '-='):
                symbol_table[scope][term]['value'] -= flatten(p[3])[0]
    p[0] = p[1:]

def p_assignment_operator(p):
    '''assignment_operator : '='
    | TIMESEQUAL
    | DIVEQUAL
    | MODEQUAL
    | PLUSEQUAL
    | MINUSEQUAL'''
    p[0] = p[1:]

def p_expression(p):
    '''expression : assignment_expression
    | expression ',' assignment_expression'''
    p[0] = p[1:]


def p_declaration(p):
    '''declaration : type_specifier ';'
    | type_specifier init_declarator_list ';' '''
    #print('declaration',p[1:])
    tspecifier = flatten(p[1])[0]
    for i in flatten(p[2]):
        if i in symbol_table[scope]:
            symbol_table[scope][i]["tspecifier"] = tspecifier
        
    p[0] = p[1:]

def p_init_declarator_list(p):
    '''init_declarator_list : init_declarator
    | init_declarator_list ',' init_declarator'''
    p[0] = p[1:]

def p_init_declarator(p):
    '''init_declarator : declarator '=' initializer
    | declarator'''
    #print(len(p),'dec',p[1:])
    if(len(p) == 4):
        term = flatten(p[1])[0]
        if term in symbol_table[scope]:
            #print(flatten(p[3])[0],type(flatten(p[3])[0]))
            if(type(flatten(p[3])[0]) == tuple):
                symbol_table[scope][term]['value'] = flatten(p[3])[0][1]
                print("{: <20} {: <20} {: <20} {: <20}".format('=','t' + str(flatten(p[3])[0][0])," ",term))
            else:
                symbol_table[scope][term]['value'] = flatten(p[3])[0]
                #print('= \t\t ',flatten(p[3])[0],"\t\t\t",term)
                print("{: <20} {: <20} {: <20} {: <20}".format('=',flatten(p[3])[0]," ",term))
    p[0] = p[1:]

def p_type_specifier(p):
    '''type_specifier : VOID
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE'''
    p[0] = p[1:]

def p_declarator(p):
    '''declarator : direct_declarator'''
    p[0] = p[1:]

#declarator : pointer direct_declarator
    
def p_direct_declarator(p):
    '''direct_declarator : ID
    | '(' declarator ')'
    | direct_declarator '[' ']'
    | direct_declarator '[' '*' ']'
    | direct_declarator '[' assignment_expression ']'
    
    | direct_declarator '(' ')'
    | direct_declarator '(' identifier_list ')' '''
    p[0] = p[1:]


##| direct_declarator '(' parameter_list ')'
##def p_pointer(p):
##    '''pointer : '*' pointer
##    | '*' '''
##    print('============pointer===========')
##    p[0] = p[1:]
##
##def p_parameter_list(p):
##    '''parameter_list : parameter_declaration
##    | parameter_list ',' parameter_declaration'''
##    print('============parameter list===========')
##    p[0] = p[1:]
##
##def p_parameter_declaration(p):
##    '''parameter_declaration : type_specifier declarator
##    | type_specifier'''
##    print('============parameter declaration===========')
##    p[0] = p[1:]

    
def p_identifier_list(p):
    '''identifier_list : ID
    | identifier_list ',' ID'''
    p[0] = p[1:]

def p_initializer(p):
    '''initializer : '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    | assignment_expression'''
    p[0] = p[1:]

def p_initializer_list(p):
    '''initializer_list : initializer
    | initializer_list ',' initializer'''
    p[0] = p[1:]

def p_statement(p):
    '''statement : compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | print_statement'''
    p[0] = p[1:]

def p_selection_statement(p):
    '''selection_statement : IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement'''
    p[0] = p[1:]
	
def p_print_statement(p):
    '''print_statement : PRINTF '(' SCONST ')'
    '''
    p[0] = p[1:]
    
def p_compound_statement(p):
    '''compound_statement : '{' '}'
    | '{'  block_item_list '}' '''
##    global multiplier
##    global scope
##    multiplier *= 10
    p[0] = p[1:]

def p_block_item_list(p):
    '''block_item_list : block_item
    | block_item_list block_item'''
##    global multiplier
##    global scope
##    global scope_list
##    multiplier /= 10
##    if(scope in scope_list):
##        scope_list[scope] += multiplier*1
##    else:
##        scope_list[scope] = scope + multiplier*1
##        scope = scope + multiplier*1
##    print('==============scope=',t,scope,scope_list)
    p[0] = p[1:]

def p_block_item(p):
    '''block_item : declaration
    | statement'''
    p[0] = p[1:]

def p_expression_statement(p):
    '''expression_statement : ';'
    | expression ';' '''
    p[0] = p[1:]

def p_iteration_statement(p):
    '''iteration_statement : DO statement WHILE '(' expression ')' ';' '''
    p[0] = p[1:]

def p_jump_statement(p):
    '''jump_statement : CONTINUE ';'
    | BREAK ';'
    | RETURN ';'
    | RETURN expression ';' '''
    p[0] = p[1:]



def p_external_declaration(p):
    '''external_declaration : INT MAIN '(' ')' compound_statement
    | declaration'''
    p[0] = p[1:]

#external_declaration : function_definition
##def p_function_definition(p):
##    '''function_definition : type_specifier declarator declaration_list compound_statement
##    | type_specifier declarator compound_statement'''
##    print('============function def===========')
##    p[0] = p[1:]
##
##def p_declaration_list(p):
##    '''declaration_list : declaration
##    | declaration_list declaration'''
##    print('============declaration list===========')
##    p[0] = p[1:]


lex.lex(debug=0)
parser = yacc.yacc(method="SLR",debug=0)


with open('test.c','r') as f:
    input_str = f.read()

#print(input_str)
'''
print("\n\n\n==========Tokens Generated============")
lex.input(input_str)
 # Tokenize
while True:
    tok = lex.token()
    if not tok:
        break      # No more input
    print(tok)
'''
print("Quadruple format")
print("{: <20} {: <20} {: <20} {: <20}".format("operator","arg1","arg2","result"))
x = parser.parse(input_str)
print("\n\n\n============Parser Output============")
print(x)
print('success')
print("\n\n\n============Symbol Table=============")
for symbol in symbol_table:
    if("token" in symbol_table[symbol]):
        print(symbol_table[symbol])
    else:
        for ss in symbol_table[symbol]:
            if("token" in symbol_table[symbol][ss]):
                print(symbol_table[symbol][ss])
