
// $antlr-format alignColons trailing, alignLabels true, alignLexerCommands true, alignSemicolons ownLine, alignTrailers true
// $antlr-format alignTrailingComments true, allowShortBlocksOnASingleLine true, allowShortRulesOnASingleLine true, columnLimit 150
// $antlr-format maxEmptyLinesToKeep 1, minEmptyLines 0, reflowComments false, singleLineOverrulesHangingColon true, useTab false

lexer grammar FortranLexer;

options {
    caseInsensitive = true;
}

LINE_COMMENT: '!' .*? '\r'? '\n' -> skip;

BLOCK_COMMENT: '/*' .*? '*/' -> skip;

SPACE: [ ] -> skip;

WS: [\t\r\n]+ -> skip;

EXTENDS: 'EXTENDS';

PROGRAM: 'PROGRAM';

END: 'END';

COMMA: ',';

FUNCTION: 'FUNCTION';

LPAREN: '(';

RPAREN: ')';

ASTERIK: '*';

SUBROUTINE: 'SUBROUTINE';

MODULE: 'MODULE';

SUBMODULE: 'SUBMODULE';

BLOCK: 'BLOCK';

DATA: 'DATA';

COLON: ':';

INTRINSIC: 'INTRINSIC';

NONINTRINSIC: 'NON_INTRINSIC';

IMPLIES: '=>';

OPERATOR: 'OPERATOR';

POWER: '**';

SLASH: '/';

PLUS: '+';

MINUS: '-';

CONCAT: '//';

EQ: '.EQ.';

NE: '.NE.';

LT: '.LT.';

LE: '.LE.';

GT: '.GT.';

GE: '.GE.';

EQUAL: '==';

NOTEQUAL: '/=';

LESSTHAN: '<';

LESSEQUAL: '<=';

GREATERTHAN: '>';

GREATEREQUAL: '>=';

NOT: '.NOT.';

AND: '.AND.';

OR: '.OR.';

EQV: '.EQV.';

NEQV: '.NEQV.';

READ: 'READ';

FORMATTED: 'FORMATTED';

UNFORMATTED: 'UNFORMATTED';

WRITE: 'WRITE';

ASSIGNMENT: 'ASSIGNMENT';

ASSIGN: '=';

USE: 'USE';

DOUBLECOLON: '::';

ONLY: 'ONLY';

IMPORT: 'IMPORT';

NONE: 'NONE';

ALL: 'ALL';

KIND: 'KIND';

INTEGER: 'INTEGER';

LEN: 'LEN';

REAL: 'REAL';

DOUBLE: 'DOUBLE';

PRECISION: 'PRECISION';

COMPLEX: 'COMPLEX';

CHARACTER: 'CHARACTER';

LOGICAL: 'LOGICAL';

TYPE: 'TYPE';

CLASS: 'CLASS';

EXTERNAL: 'EXTERNAL';

IMPLICIT: 'IMPLICIT';

PARAMETER: 'PARAMETER';

FORMATIN: FORMAT SPACE* LPAREN -> pushMode(FORMAT_MODE);

FORMAT: 'FORMAT';

BINDC: BIND SPACE* LPAREN SPACE* C;

BIND: 'BIND';

NAAM: 'NAME';

RESULT: 'RESULT';

ENTRY: 'ENTRY';

DOT: '.';

TRUE: '.TRUE.';

FALSE: '.FALSE.';

SQUOTE: '\'';

DQUOTE: '"';

STAT: 'STAT';

TEAM: 'TEAM';

TEAMNUMBER: 'TEAM_NUMBER';

LBRACKET: '[';

RBRACKET: ']';

RE: 'RE';

IM: 'IM';

PERCENT: '%';

LPARENSLASH: '(/';

RPARENSLASH: '/)';

// R602 UNDERSCORE -> _
UNDERSCORE: '_';

SEQUENCE: 'SEQUENCE';

PRIVATE: 'PRIVATE';

PROCEDURE: 'PROCEDURE';

NOPASS: 'NOPASS';

PASS: 'PASS';

POINTER: 'POINTER';

ALLOCATABLE: 'ALLOCATABLE';

CODIMENSION: 'CODIMENSION';

CONTIGUOUS: 'CONTIGUOUS';

DIMENSION: 'DIMENSION';

PUBLIC: 'PUBLIC';

CONTAINS: 'CONTAINS';

FINAL: 'FINAL';

GENERIC: 'GENERIC';

DEFERRED: 'DEFERRED';

NONOVERRIDABLE: 'NON_OVERRIDABLE';

INTENT: 'INTENT';

OPTIONAL: 'OPTIONAL';

PROTECTED: 'PROTECTED';

SAVE: 'SAVE';

IN: 'IN';

OUT: 'OUT';

INOUT: 'INOUT';

INTERFACE: 'INTERFACE';

ABSTRACT: 'ABSTRACT';

ENUM: 'ENUM';

ENUMERATOR: 'ENUMERATOR';

ASYNCHRONOUS: 'ASYNCHRONOUS';

TARGET: 'TARGET';

VALUE: 'VALUE';

VOLATILE: 'VOLATILE';

EQUIVALENCE: 'EQUIVALENCE';

COMMON: 'COMMON';

NAMELIST: 'NAMELIST';

DOUBLEDOT: '..';

EVENT: 'EVENT';

WAIT: 'WAIT';

UNTILCOUNT: 'UNTIL_COUNT';

POST: 'POST';

ERRMSG: 'ERRMSG';

ERROR: 'ERROR';

STOP: 'STOP';

QUIET: 'QUIET';

ENDFILE: 'ENDFILE';

DEALLOCATE: 'DEALLOCATE';

CYCLE: 'CYCLE';

CONTINUE: 'CONTINUE';

CLOSE: 'CLOSE';

UNIT: 'UNIT';

IOSTAT: 'IOSTAT';

IOMSG: 'IOMSG';

ERR: 'ERR';

STATUS: 'STATUS';

CALL: 'CALL';

BACKSPACE: 'BACKSPACE';

ALLOCATE: 'ALLOCATE';

MOLD: 'MOLD';

SOURCE: 'SOURCE';

OPEN: 'OPEN';

ACCESS: 'ACCESS';

ACTION: 'ACTION';

BLANK: 'BLANK';

DECIMAL: 'DECIMAL';

DELIM: 'DELIM';

ENCODING: 'ENCODING';

FILE: 'FILE';

FORM: 'FORM';

NEWUNIT: 'NEWUNIT';

PAD: 'PAD';

POSITION: 'POSITION';

RECL: 'RECL';

ROUND: 'ROUND';

SIGN: 'SIGN';

NULLIFY: 'NULLIFY';

LOCK: 'LOCK';

ACQUIREDLOCK: 'ACQUIRED_LOCK';

INQUIRE: 'INQUIRE';

IOLENGTH: 'IOLENGTH';

EXIST: 'EXIST';

ID: 'ID';

NAMED: 'NAMED';

NEXTREC: 'NEXTREC';

NUMBER: 'NUMBER';

OPENED: 'OPENED';

PENDING: 'PENDING';

POS: 'POS';

READWRITE: 'READWRITE';

SEQUENTIAL: 'SEQUENTIAL';

SIZE: 'SIZE';

STREAM: 'STREAM';

IF: 'IF';

GO : 'GO';
TO : 'TO';

NEWINDEX: 'NEW_INDEX';

FLUSH: 'FLUSH';

FAIL: 'FAIL';

IMAGE: 'IMAGE';

EXIT: 'EXIT';

FORALL: 'FORALL';

WHERE: 'WHERE';

EOR: 'EOR';

UNLOCK: 'UNLOCK';

SYNC: 'SYNC';

MEMORY: 'MEMORY';

IMAGES: 'IMAGES';

REWIND: 'REWIND';

RETURN: 'RETURN';

FMT: 'FMT';

NML: 'NML';

ADVANCE: 'ADVANCE';

REC: 'REC';

PRINT: 'PRINT';

CRITICAL: 'CRITICAL';

CHANGE: 'CHANGE';

SELECT: 'SELECT';

CASE: 'CASE';

DEFAULT: 'DEFAULT';

ASSOCIATE: 'ASSOCIATE';

ELSEWHERE: 'ELSEWHERE';

IS: 'IS';

RANK: 'RANK';

ELSE: 'ELSE';

THEN: 'THEN';

DO: 'DO';

CONCURRENT: 'CONCURRENT';

WHILE: 'WHILE';

SHARED: 'SHARED';

LOCAL: 'LOCAL';

LOCALINIT: 'LOCAL_INIT';

RECURSIVE: 'RECURSIVE';

PURE: 'PURE';

NONRECURSIVE: 'NON_RECURSIVE';

IMPURE: 'IMPURE';

ELEMENTAL: 'ELEMENTAL';

ATSYMBOL: '@';

NIL: '.NIL';

QUESTION: '?';

NOTIFY: 'NOTIFY';

TYPEOF: 'TYPEOF';

CLASSOF: 'CLASSOF';

ENUMERATION: 'ENUMERATION';

DIRECT: 'DIRECT';

LEADINGZERO: 'LEADING_ZERO';

REDUCE: 'REDUCE';

SIMPLE: 'SIMPLE';

DEFINEDUNARYBINARYOP: DOT LETTER+? DOT;

// R865 letter-spec -> letter [- letter]
///LEXER RULE: LetterSpec should only be defined as LETTER MINUS LETTER form and not in only LETTER form due to conflict with NAME
LETTER_SPEC: LETTER MINUS LETTER;

// R765 binary-constant -> B ' digit [digit]... ' | B " digit [digit]... "
BINARY_CONSTANT: B SQUOTE DIGIT+? SQUOTE | B DQUOTE DIGIT+? DQUOTE;

// R766 octal-constant -> O ' digit [digit]... ' | O " digit [digit]... "
OCTAL_CONSTANT: O SQUOTE DIGIT+? SQUOTE | O DQUOTE DIGIT+? DQUOTE;

// R767 hex-constant -> Z ' hex-digit [hex-digit]... ' | Z " hex-digit [hex-digit]... "
HEX_CONSTANT: Z SQUOTE HEXDIGIT+? SQUOTE | Z DQUOTE HEXDIGIT+? DQUOTE;

//R0003 RepChar
SQUOTE_REP_CHAR: SQUOTE ~[\u0000-\u001F]*? SQUOTE;

DQUOTE_REP_CHAR: DQUOTE ~[\u0000-\u001F]*? DQUOTE;

REALEXPONENTLETTER:
    DIGITSTRING DOT DIGITSTRING? EXPONENTLETTER
    | DOT DIGITSTRING EXPONENTLETTER
    | DIGITSTRING EXPONENTLETTER
;

// R603 name -> letter [alphanumeric-character]...
NAME: LETTER ALPHANUMERICCHARACTER*;

// R0002 Letter ->
//         A | B | C | D | E | F | G | H | I | J | K | L | M |
//         N | O | P | Q | R | S | T | U | V | W | X | Y | Z
LETTER: 'A' ..'Z';

// R711 digit-string -> digit [digit]...
DIGITSTRING: DIGIT+;

// R0001 Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
DIGIT: '0' ..'9';

// R601 alphanumeric-character -> letter | digit | underscore
ALPHANUMERICCHARACTER: LETTER | DIGIT | UNDERSCORE;

C: 'C';

E: 'E';

D: 'D';

// R716 exponent-letter -> E | D
EXPONENTLETTER: E | D;

B: 'B';

O: 'O';

Z: 'Z';

A: 'A';

F: 'F';

// R768 hex-digit -> digit | A | B | C | D | E | F
HEXDIGIT: DIGIT | A | B | C | D | E | F;

///FORMAT MODE

mode FORMAT_MODE;

FORMAT_LPAREN: '(' -> pushMode(FORMAT_MODE), type(LPAREN);

FORMAT_RPAREN: ')' -> popMode, type(RPAREN);

FORMAT_SPACE: ' ' -> skip;

FORMAT_COMMA: ',' -> type(COMMA);

FORMAT_ASTERIK: '*' -> type(ASTERIK);

FORMAT_UNDERSCORE: '_' -> type(UNDERSCORE);

FORMAT_DIGITSTRING: FORMAT_DIGIT+ -> type(DIGITSTRING);

FORMAT_DIGIT: '0' ..'9' -> type(DIGIT);

FORMAT_APOSTROPHE: '\'' -> type(SQUOTE);

FORMAT_QUOTE: '"' -> type(DQUOTE);

FORMAT_APOSTROPHEREPCHAR:
    FORMAT_APOSTROPHE ~[\u0000-\u001F\u0027]*? FORMAT_APOSTROPHE -> type(SQUOTE_REP_CHAR)
;

FORMAT_QUOTEREPCHAR: FORMAT_QUOTE ~[\u0000-\u001F\u0022]*? FORMAT_QUOTE -> type(DQUOTE_REP_CHAR);

P: 'P';

DC: 'DC';

DP: 'DP';

LZS: 'LZS';

LZP: 'LZP';

LZ: 'LZ';

RU: 'RU';

RD: 'RD';

RZ: 'RZ';

RN: 'RN';

RC: 'RC';

RP: 'RP';

BN: 'BN';

BZ: 'BZ';

SS: 'SS';

SP: 'SP';

S: 'S';

T: 'T';

TL: 'TL';

TR: 'TR';

X: 'X';

I: 'I';

FORMAT_B: 'B' -> type(B);

FORMAT_O: 'O' -> type(O);

FORMAT_Z: 'Z' -> type(Z);

FORMAT_F: 'F' -> type(F);

FORMAT_E: 'E' -> type(E);

EN: 'EN';

ES: 'ES';

EX: 'EX';

G: 'G';

L: 'L';

FORMAT_A: 'A' -> type(A);

AT: 'AT';

FORMAT_D: 'D' -> type(D);

DT: 'DT';

FORMAT_NAME: FORMAT_LETTER FORMAT_ALPHANUMERICCHARACTER* -> type(NAME);

FORMAT_LETTER: 'A' ..'Z' -> type(LETTER);

FORMAT_ALPHANUMERICCHARACTER: (FORMAT_LETTER | FORMAT_DIGIT | FORMAT_UNDERSCORE) -> type(ALPHANUMERICCHARACTER)
;

FORMAT_SLASH: '/' -> type(SLASH);

FORMAT_COLON: ':' -> type(COLON);

FORMAT_PLUS: '+' -> type(PLUS);

FORMAT_MINUS: '-' -> type(MINUS);

FORMAT_DOT: '.' -> type(DOT);