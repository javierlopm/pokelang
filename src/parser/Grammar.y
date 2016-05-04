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
    "!!"      { TkExcArr   _ }    
    "!="      { TkNEQ      _ }    
    "&&"      { TkDAmp     _ }    
    "||"      { TkPOr      _ }    
    AND       { TkAnd      _ }    
    OR        { TkOr       _ }    
    --"?"       { TkDEQ      _ }    
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

    -- Built-in functions/Instructions
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
    STRING    { TkString _ _ }
    INT       { TkNum _ _ }
    FLOAT     { TkFloatVal _ _ }

    -- Composed types
    ENUM { TkEnumCons _ _ }

-- Para las expresiones relacionales.
--%nonassoc '<' <\=' '>' '>\=' '=' '\/=' '..'
%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="

-- Para los booleanos.
%left  OR
%left  AND
%left  "||"
%left  "&&"
%right "!"

-- Para los enteros.
%left "+" "-"
%left "*" "/" "//" "%"
%left "^"
%right NEG      -- Para el - unario.

--Strucs Union y Arreglos
%left "!!"   
%left "."  
%left ARR

--Llamadas a funciones
%left CALL

--Acceso a apuntadores
%right POINT



%%

Prog : Dcls  { $1 }

Ins : {- λ -}                                 { [] }
    | Ins PRINT "(" STRING PrntArgs ")"   ";" { [] } 
    | Ins READ  "("       ID        ")"   ";" { [] }
    | Ins ID "=" Exp        ";"         { [] }
    | Ins BREAK             ";"         { [] }
    | Ins CONTINUE          ";"         { [] }
    | Ins RETURN            ";"         { [] }
    | Ins EXIT              ";"         { [] }
    | Ins FREE "("ID")"     ";"         { [] }
    | Ins FREE "("DATAID")" ";"         { [] }
    | Ins READ "("DATAID")" ";"         { [] }
    | Ins IF ":" SmplDcls Ins NextIf Else END    { [] }
    | Ins WHILE Exp ":" SmplDcls Ins END         { [] }
    | Ins FOR ID "=" INT  "|" INT "|" INT ":" SmplDcls Ins  END { [] }
    | Ins FOR ID "=" INT  "|" INT         ":" SmplDcls Ins  END { [] }
    | Ins FOR ID "=" ENUM "|" ENUM        ":" SmplDcls Ins  END { [] }
    | Ins BEGIN SmplDcls Ins END    { [] } -- No debe aceptar funciones

PrntArgs: {- λ -}             { [] }
        | PrntArgs "," Exp    { [] } -- Siempre es necesaria una coma a la izq

NextIf: {- λ -}             { [] }
      | NextIf ELIF ":" Ins { [] }

Else: {- λ -}      { [] }
    | ELSE ":" Ins { [] }

SmplDcls: {- λ -}                                  { [] }        
    | SmplDcls IsGlob PrimType Ptrs       ID  ";"  { [] }
    | SmplDcls IsGlob PrimType EmptyArrs  ID  ";"  { [] }
    | SmplDcls IsGlob PrimType StaticArrs ID  ";"  { [] }
    | SmplDcls IsGlob PrimType            ID  ";"  { [] }

Dcls:  {- λ -}                                { [] }
    | Dcls FUNC PrimType ID "(" Parameter ")" ":" SmplDcls Ins END { [] }
    | Dcls IsGlob PrimType Ptrs       ID  ";"  { [] }
    | Dcls IsGlob PrimType EmptyArrs  ID  ";"  { [] }
    | Dcls IsGlob PrimType StaticArrs ID  ";"  { [] }
    | Dcls IsGlob PrimType           ID  ";"   { [] }
    | Dcls DataType    DATAID     ";"          { [] }    -- Forward declarations
    | Dcls ENUMDEC DATAID   "{" EnumConsList "}"   { [] }
    | Dcls STRUCTDEC DATAID "{" FieldsList   "}"   { [] }
    | Dcls UNIONDEC DATAID  "{" FieldsList   "}"   { [] }


IsGlob : {- λ -}     { True  }
         | GLOBAL    { False }

PrimType : INTDEC         { [] }
         | BOOLDEC        { [] }
         | CHARDEC        { [] }
         | VOIDDEC        { [] }
         | FLOATDEC       { [] }

DataType : ENUMDEC        { [] }
         | STRUCTDEC      { [] }
         | UNIONDEC       { [] }

Parameter: {- λ -}                        { [] }
         | Parameters PrimType ID         { [] }
         | Parameters DataType DATAID     { [] }

Parameters: {- λ -}                        { [] }
          | Parameters PrimType ID     "," { [] }
          | Parameters DataType DATAID "," { [] }

EnumConsList: ENUM                      { [] }
            | EnumConsList "," ENUM     { [] }

FieldsList  : ID     "::" PrimType                   { [] }
            | DATAID "::" DATAID                     { [] }
            | FieldsList  "," ID     "::" PrimType   { [] }
            | FieldsList  "," DATAID "::" DATAID     { [] }

Ptrs: "*"        { [] }
    | Ptrs "*"   { [] }

EmptyArrs: "[" "]"             { [] }
         |  EmptyArrs "[" "]"  { [] }

StaticArrs: "[" INT "]"             { [] }
          | StaticArrs "[" INT "]"  { [] }

Exp : 
    -- Expresiones Aritméticas.
      Exp "+" Exp       { [] }
    | Exp "-" Exp       { [] }
    | Exp "^" Exp       { [] }
    | Exp "*" Exp       { [] }
    | Exp "/" Exp       { [] }
    | Exp "//" Exp      { [] }
    | Exp "%" Exp       { [] }
    | "-" Exp %prec NEG { [] }
    -- Expresiones Booleanas.
    | Exp OR Exp        { [] }
    | Exp "||" Exp      { [] }
    | Exp AND Exp       { [] }
    | Exp "&&" Exp      { [] }
    | "!" Exp         { [] }
    -- Expresiones relacionales.
    | Exp "<"  Exp      { [] }
    | Exp "<=" Exp      { [] }
    | Exp ">"  Exp      { [] }
    | Exp ">=" Exp      { [] }
    | Exp "==" Exp      { [] }
    | Exp "!=" Exp      { [] }
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp           { [] }
    | Exp "."  Exp           { [] }
    | ID "[" Exp "]" %prec ARR { [] }
    --Llamadas a funciones
    | ID "(" Exp ")"         { [] }
    --Acceso a apuntadores
    | "*" Exp %prec POINT  { [] }
    -- Asociatividad.
    | "(" Exp ")"    { [] }
    -- Constantes.
    | Term           { [] }
    -- Llamadas
    | MALLOC "(" Exp ")"       { [] }
    | SIZEOF "(" Exp ")"       { [] }
    | SIZEOF "(" PrimType ")"  { [] }
    | GET    "(" ENUM ")"      { [] }

Term: TRUE         { [] }
    | FALSE        { [] }
    | ID           { [] }
    | DATAID       { [] }
    | FLOAT        { [] }
    | INT          { [] }
    | CHAR         { [] }

{

parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Error de parseo en " ++ show (head l)

}