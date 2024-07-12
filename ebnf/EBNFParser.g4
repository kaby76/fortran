parser grammar EBNFParser;
options { tokenVocab=EBNFLexer; superClass=EBNFParserBase; }
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
any : ANY ( { this.IsNotWS() }? ANY )* ;
