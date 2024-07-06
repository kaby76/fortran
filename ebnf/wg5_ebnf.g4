grammar wg5_ebnf;

rulelist : rule_* EOF ;
rule_ : RULE_NUMBER? lhs ASSIGN rhs;
lhs : EXCLAMATION_MARK? id_ ;
rhs : alternatives ;
alternatives : alternative (ALT alternative)* ;
alternative : element* ;
element : zero_or_one | zero_or_more | STRING | CHAR | id_ | any ;
id_ : ID ;
zero_or_one : LEFT_SQUARE_BRACKET alternatives RIGHT_SQUARE_BRACKET ;
zero_or_more : LEFT_SQUARE_BRACKET alternatives RIGHT_SQUARE_BRACKET DOT_DOT_DOT;
any : ANY+;

QU : '\'';
ASSIGN : '<b>is</b>';
ALT : '<b>or</b>' ;
CHAR : '\'' ~'\''* '\'' ;
COMMENT : '//' ~[\n\r]* -> channel(HIDDEN) ;
DOT_DOT_DOT : '...';
EXCLAMATION_MARK : '!';
ID : '<i>' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_')+ '</i>';
LEFT_SQUARE_BRACKET : '[' ;
RIGHT_SQUARE_BRACKET : ']' ;
STRING : '"' ~'"'* '"'
 | '\'' .*? '\''
 | '\'\'\''
 | '\uFFFD' '\'' '\uFFFD'
 | '\uFFFD' ~('\uFFFD' | '\n' | '\r')* '\uFFFD'
 ;
WS : [ \t] -> channel(HIDDEN) ;
NL : [\r\n]+ -> channel(HIDDEN);
RULE_NUMBER : 'R' { Functional.Lambda<bool>( () => { return this.Column == 1; })() }? [0-9]+;
ANY : .;
