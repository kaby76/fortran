lexer grammar EBNFLexer;
QU : '\'';
ASSIGN : '<b>is</b>';
ALT : '<b>or</b>' ;
CHAR : '\'' ~'\''* '\'' ;
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
