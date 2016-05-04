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
    ENUM      { TkEnumCons _ _ }
    STRING    { TkString _ _ }
    INT       { TkNum _ _ }
    FLOAT     { TkFloatVal _ _ }

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
    | Ins PRINT "(" STRING PrntArgs ")"   ";" { [] } -- Mucho mas complejo que esto
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
    | Ins BEGIN SmplDcls Ins END    { [] } -- No deberia aceptar funciones

PrntArgs: {- λ -}             { [] }
        | PrntArgs "," Exp    { [] }

NextIf: {- λ -}             { [] }
      | NextIf ELIF ":" Ins { [] }

Else: {- λ -}      { [] }
    | ELSE ":" Ins { [] }

SmplDcls: {- λ -}                                 { [] }        
    | SmplDcls IsGlob PrimType "["INT"]" ID  ";"  { [] }
    | SmplDcls IsGlob PrimType "[""]"    ID  ";"  { [] }
    | SmplDcls IsGlob PrimType "*"       ID  ";"  { [] }
    | SmplDcls IsGlob PrimType           ID  ";"  { [] }
    | SmplDcls IsGlob DataType    DATAID     ";"  { [] }

Dcls:  {- λ -}                                { [] }
    | Dcls FUNC PrimType ID "(" Parameter ")" ":" SmplDcls Ins END { [] }
    | Dcls IsGlob PrimType "["INT"]" ID  ";"  { [] }
    | Dcls IsGlob PrimType "[""]"    ID  ";"  { [] }
    | Dcls IsGlob PrimType "*"       ID  ";"  { [] }
    | Dcls IsGlob PrimType           ID  ";"  { [] }
    | Dcls IsGlob DataType    DATAID     ";"  { [] }


IsGlob : {- λ -}     { True  }
         | GLOBAL    { False }

PrimType : INTDEC         { [] }
         | BOOLDEC        { [] }
         | CHARDEC        { [] }
         | VOIDDEC        { [] }
         | FLOATDEC       { [] }

DataType : STRUCTDEC      { [] }
         | UNIONDEC       { [] }
         | ENUMDEC        { [] }

Parameter: {- λ -}                        { [] }
         | Parameters PrimType ID         { [] }
         | Parameters DataType DATAID     { [] }

Parameters: {- λ -}                        { [] }
          | Parameters PrimType ID     "," { [] }
          | Parameters DataType DATAID "," { [] }
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

  -- Expresiones de arreglos.
  | Exp "!!" Exp           { [] }
  | Exp "."  Exp           { [] }
    | Exp "[" Exp "]" %prec ARR { [] }

  --Llamadas a funciones
  | ID "(" Exp ")"         { [] }

  --Acceso a apuntadores
    | "*" Exp %prec POINT  { [] }

  -- Asociatividad.
  | "(" Exp ")"    { [] }

  -- Constantes.
  | Term           { [] }

Term:  --Simbolos terminales
    TRUE         { [] }
  | FALSE        { [] }
  | ID           { [] }
  | DATAID       { [] }
  | FLOAT        { [] }
  | INT          { [] }

{

parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Error de parseo en " ++ show (head l)

}