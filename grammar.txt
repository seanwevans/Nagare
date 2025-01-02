grammar Nagare;

@members {
    boolean seenBegin = false;
    boolean seenPositions = false;
    boolean seenZones = false;
}

ELLIPSES: '...' ;
IDEN: [a-zA-Z_][a-zA-Z0-9_]* ;
WHITESPACE: [ \t]+ -> skip ;
LB: '\r'? '\n' ;

NUM: '-'? [0-9]+ ('.' [0-9]+)? ;
STR: '"' ( ~["\\] | '\\' . )* '"' ;
RELATIONAL_OPERATOR: '<=' | '>=' | '<' | '>' | '==' | '!=' ;
BOOLEAN_OPERATOR: '&&' | '||' | '~` ;

SHEBANG: '!' '\s*' '\S+' '\s*' [Nn][Aa][Gg][Aa][Rr][EÉeé] LB ;
SINGLE_LINE_COMMENT: '!' .*? LB -> skip ;
MULTI_LINE_COMMENT: '!!!' .*? '!!!' LB -> skip ;

comment: single_line_comment | multi_line_comment ;

arithmeticExpression: 
    arithmeticExpression '+'  arithmeticExpression
    | arithmeticExpression '-'  arithmeticExpression
    | arithmeticExpression '*'  arithmeticExpression
    | arithmeticExpression '/'  arithmeticExpression
    | arithmeticExpression '**' arithmeticExpression
    | '(' arithmeticExpression ')'
    | IDEN
    | num
    | ELLIPSES
;

booleanExpression: 
    arithmeticExpression relationalOperator arithmeticExpression
    | booleanExpression booleanOperator booleanExpression
    | '(' booleanExpression ')'
    | ELLIPSES
;

stringExpression: STR | ELLIPSES;

vector: '<' arithmeticExpression ( ',' arithmeticExpression )* '>' | ELLIPSES ;

expression: arithmeticExpression | booleanExpression | stringExpression | vector | ELLIPSES ;
assignment: IDEN '=' expression  ;
command:    IDEN     expression? ;
statement:  assignment | command | ELLIPSES ;

importStatement: 'import' STR ( 'as' IDEN )? ;
fromimportStatement 'from' STR importStatement ;

globalStatement:  'global'  IDEN ':=' expression ;
stateStatement:   'state'   IDEN ':=' expression ;
fieldStatement:   'field'   IDEN ':=' vector ;
programStatement: 'program' IDEN ':=' vector ;
returnStatement:  'return' expression ;

globalDefinition: globalStatement | stateStatement | fieldStatement| programStatement | ELLIPSES
beginBlock:     'BEGIN'     comment* '{' ( globalDefinition*   | ELLIPSES ) '}' ;

positionDefinition: IDEN ( '{' returnStatement '}' | ELLIPSES )
positionsBlock: 'POSITIONS' comment* '{' ( positionDefinition* | ELLIPSES ) '}' ;

zoneDefinition: (IDEN|ELLIPSES) (vector|ELLIPSES) (block|ELLIPSES) ;
zonesBlock:     'ZONES'     comment* '{' ( zoneDefinition*     | ELLIPSES ) '}' ;

block: 
    beginBlock       {seenBegin = true;} 
    | positionsBlock {seenPositions = true;} 
    | zonesBlock     {seenZones = true;}
;

program: 
    {seenBegin = False; seenPositions = False; seenZones = False;}
    SHEBANG? comment* 
    block    comment* 
    block    comment* 
    block    comment* 
    {seenBegin and seenPositions and seenZones}? 
    EOF
;
