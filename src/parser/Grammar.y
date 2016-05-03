{
module Grammar(parser) where
import Tokens
}


%name parser
%tokentype { Token      }
%error     { parseError }

%token
    ID        { TkId       _ _ }     
    DATAID    { TkDId      _ _ }
    -- Declarations       
    INTDEC    { TkInt      _ } 
    BOOLDEC   { TkBool     _ }     
    CHARDEC   { TkChar     _ }     
    VOIDDEC   { TkVoid     _ }     
    FLOATDEC  { TkFloat    _ }    
    STRUCTDEC { TkStruct   _ }   
    UNIONDEC  { TkUnion    _ }     
    ENUMDEC   { TkEnum     _ }     
    GLOBAL    { TKGlobal   _ }

    "["       { TkLBracket   _ }     
    "]"       { TkRBracket   _ }     
    "{"       { TkLCurly   _ }     
    "}"       { TkRCurly   _ }     
    "("       { TkLRound   _ }     
    ")"       { TkRRound   _ }     
    "|"       { TkPipe   _ }     
    "::"      { TkDColon   _ }     
    ":"       { TkColon   _ }     
    ";"       { TkSColon   _ }    
    ","       { TkComma    _ }    
    "*="      { TkTEQ      _ }    
    "+="      { TkPEQ      _ }    
    "."       { TkDot      _ }    
    "!"       { TkExcMark  _ }    
    "!="      { TkNEQ      _ }    
    "&&"      { TkDAmp     _ }    
    "||"      { TkPOr      _ }    
    AND       { TkAnd      _ }    
    OR        { TkOr       _ }    
    "?"       { TkDEQ      _ }    
    ">="      { TkGE   _ }    
    "<="      { TkLE   _ }    
    ">"       { TkGT   _ }    
    "<"       { TkLT   _ }    
    "/"       { TkIDiv   _ }    
    "//"      { TkDiv   _ }    
    "+"       { TkSum   _ }    
    "-"       { TkMin   _ }    
    "^"       { TkPower   _ }    
    "*"       { TkTimes   _ }    
    "%"       { TkMod   _ }    
    "=="      { TkEq   _ }    
    "="       { TkAssign   _ }

    FUNC      { TkFunc _ }

    -- Control structures
    IF        { TkIf _ }
    ELIF      { TkElif _ }
    ELSE      { TkElse _ }
    END       { TkEnd _ }
    WHILE     { TkWhile _ }
    FOR       { TkFor _ }
    BEGIN     { TkBegin _ }
    BREAK     { TkBreak _ }
    CONTINUE  { TkContinue _ }
    RETURN    { TkReturn _ }
    EXIT      { TkExit _ }

    -- Built-in functions
    READ      { TkRead _ }
    WRITE     { TkWrite _ }
    PRINT     { TkPrint _ }
    MALLOC    { TkAlloc _ }
    FREE      { TkFree _ }
    SIZEOF    { TkSizeOf _ }
    GET       { TkGet _ }
    -- Primitive types
    TRUE      { TkTrue _ }
    FALSE     { TkFalse   _ }
    CHAR      { TkCharVal _ _ }
    ENUM      { TkEnumCons _ _ }
    STRING    { TkString _ _ }
    INT       { TkNum _ _ }
    FLOAT     { TkFloatVal _ _ }

%%

Prog : Dcls  { $1 }

Ins : PRINT "(" STRING ")"             { [] } -- Mucho mas complejo que esto
    | ID "=" Exp                       { [] }

Dcls: {- empty -}                             { [] }        
    | Dcls IsGlob PrimType "["INT"]" ID  ";"  { [] }
    | Dcls IsGlob PrimType "[""]"    ID  ";"  { [] }
    | Dcls IsGlob PrimType "*"       ID  ";"  { [] }
    | Dcls IsGlob PrimType           ID  ";"  { [] }
    | Dcls IsGlob DataType  DATAID ";"        { [] }
    | Dcls FUNC PrimType ID "(" Parameter ")" ":" Ins END {[]}

IsGlob : {- empty -} { True  }
         | GLOBAL    { False }

PrimType : INTDEC         { [] }
         | BOOLDEC        { [] }
         | CHARDEC        { [] }
         | VOIDDEC        { [] }
         | FLOATDEC       { [] }

DataType : STRUCTDEC      { [] }
         | UNIONDEC       { [] }
         | ENUMDEC        { [] }

Parameter: {- empty -}                    { [] }
         | Parameters PrimType ID         { [] }
         | Parameters DataType DATAID     { [] }

Parameters: {- empty -}                    { [] }
          | Parameters PrimType ID     "," { [] }
          | Parameters DataType DATAID "," { [] }

Exp : ID            { [] }
    | Exp OR  Exp   { [] } -- Hay que agregar predecencias, let the shift/reduce conflicts begin
    -- | Exp AND Exp   { [] }
    -- | Exp "||" Exp  { [] }
    -- | Exp "&&" Exp  { [] }


{

parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Error de parseo en " ++ show (head l)

}