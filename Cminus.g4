grammar Cminus;


// PARSER RULES

program : declaration_list main_declaration EOF; // Valid program ends with a 'main' function.
declaration_list : declaration*;                 // But may have some declarations prior.
declaration : var_declaration
            | fun_declaration;
var_declaration : INT ID SEMICOLON                   
                | INT ID OPEN_BRACKET NUM CLOSE_BRACKET SEMICOLON;
type_specifier : INT 
               | VOID;
fun_declaration : type_specifier ID OPEN_PAREN params CLOSE_PAREN compound_stmt;
params : param_list
       | VOID;
param_list : param ( COMMA param )*;
param : type_specifier ID ( OPEN_BRACKET CLOSE_BRACKET )?;
compound_stmt : OPEN_CURLY local_declarations statement_list CLOSE_CURLY;
local_declarations : var_declaration*;
statement_list : statement*;
statement : expression_stmt
          | compound_stmt 
          | selection_stmt 
          | iteration_stmt 
          | return_stmt
          | assignment_stmt;
expression_stmt : expression SEMICOLON | SEMICOLON;
assignment_stmt: var ASSIGN expression SEMICOLON;
selection_stmt : IF OPEN_PAREN expression CLOSE_PAREN statement
               | IF OPEN_PAREN expression CLOSE_PAREN statement ELSE statement;
iteration_stmt : WHILE OPEN_PAREN expression CLOSE_PAREN statement;
return_stmt : RETURN SEMICOLON
            | RETURN expression SEMICOLON;
var : ID | ID OPEN_BRACKET expression CLOSE_BRACKET;
expression : simple_expression;
simple_expression : additive_expression relop additive_expression | additive_expression; 
relop : LESS_EQUAL
      | LESS
      | GREATER 
      | GREATER_EQUAL 
      | EQUAL 
      | NOT_EQUAL;
additive_expression : term (addop term)*;
addop : ADD | SUBTRACT;
term : factor ( mulop factor)*;
mulop : MULTIPLY | DIVIDE; 
factor : OPEN_PAREN expression CLOSE_PAREN 
       | var 
       | call 
       | NUM;
call : ID OPEN_PAREN args CLOSE_PAREN;
args : arg_list | ();
arg_list : expression (COMMA expression)*;
main_declaration: VOID MAIN OPEN_PAREN VOID CLOSE_PAREN compound_stmt;

// KEYWORDS
ELSE  : 'else'  ;
IF    : 'if'    ;
INT   : 'int'   ;
RETURN: 'return';
VOID  : 'void'  ;
WHILE : 'while' ;
MAIN : 'main' ;

// SYMBOLS
ADD           : '+' ;
SUBTRACT      : '-' ;
MULTIPLY      : '*' ;
DIVIDE        : '/' ;
LESS          : '<' ;
LESS_EQUAL    : '<=';
GREATER       : '>' ;
GREATER_EQUAL : '>=';
EQUAL         : '==';
NOT_EQUAL     : '!=';
ASSIGN        : '=' ;
SEMICOLON     : ';' ;
COMMA         : ',' ;
OPEN_PAREN    : '(' ;
CLOSE_PAREN   : ')' ;
OPEN_BRACKET  : '[' ;
CLOSE_BRACKET : ']' ;
OPEN_CURLY    : '{' ;
CLOSE_CURLY   : '}' ;
OPEN_COMMENT  : '/*';
CLOSE_COMMENT : '*/';

// ALPHA NUMERALS
ID               :  [a-zA-Z]+;
NUM            : [0-9]+;

WHITESPACE: (' ' | '\t' | '\r' | '\n') -> skip;
COMMENT : OPEN_COMMENT .*? CLOSE_COMMENT ('\r' | '\n')? -> skip;
