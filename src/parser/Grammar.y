{
module Grammar(parser,insertDeclareInScope) where
import Tokens
import TableTree
import Types
import Control.Monad.RWS.Strict
import qualified Data.Sequence as S



}


%name parser
%tokentype { Token }

%monad { RWS String (S.Seq(Message)) (Zipper Declare) }

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

    "["       { TkLBracket _ }     
    "]"       { TkRBracket _ }     
    "{"       { TkLCurly   _ }     
    "}"       { TkRCurly   _ }     
    "("       { TkLRound   _ }     
    ")"       { TkRRound   _ }     
    "|"       { TkPipe     _ }     
    "::"      { TkDColon   _ }     
    ":"       { TkColon    _ }     
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
    ">="      { TkGE     _ }    
    "<="      { TkLE     _ }    
    ">"       { TkGT     _ }    
    "<"       { TkLT     _ }    
    "/"       { TkIDiv   _ }    
    "//"      { TkDiv    _ }    
    "+"       { TkSum    _ }    
    "-"       { TkMin    _ }    
    "^"       { TkPower  _ }    
    "*"       { TkTimes  _ }    
    "%"       { TkMod    _ }    
    "=="      { TkEq     _ }    
    "="       { TkAssign _ }

    FUNC      { TkFunc   _ }

    -- Control structures
    IF        { TkIf       _ }
    ELIF      { TkElif     _ }
    ELSE      { TkElse     _ }
    END       { TkEnd      _ }
    WHILE     { TkWhile    _ }
    FOR       { TkFor      _ }
    BEGIN     { TkBegin    _ }
    BREAK     { TkBreak    _ }
    CONTINUE  { TkContinue _ }
    RETURN    { TkReturn   _ }
    EXIT      { TkExit     _ }

    -- Built-in functions/Instructions
    READ      { TkRead   _ }
    WRITE     { TkWrite  _ }
    PRINT     { TkPrint  _ }
    MALLOC    { TkAlloc  _ }
    FREE      { TkFree   _ }
    SIZEOF    { TkSizeOf _ }
    GET       { TkGet    _ }
    -- Primitive types
    TRUE      { TkTrue      _   }
    FALSE     { TkFalse     _   }
    CHAR      { TkCharVal   _ _ }
    STRING    { TkString    _ _ }
    INT       { TkNum       _ _ }
    FLOAT     { TkFloatVal  _ _ }

    -- Composed types
    ENUM { TkEnumCons _ _ }

-- Para las expresiones relacionales.
--%nonassoc '<' <\=' '>' '>\=' '=' '\/=' '..'


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

%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="

%%

Prog : Dcls  {% return ()}

Ins : {- λ -}                                 {% return ()}
    | Ins PRINT "(" STRING PrntArgs ")"   ";" {% return ()} 
    | Ins READ  "("       ID        ")"   ";" {% return ()}
    | Ins WRITE "("       ID        ")"   ";" {% return ()}
    | Ins Exp "=" Exp         ";"         {% return ()}
    | Ins Exp "*=" Exp        ";"         {% return ()}
    | Ins Exp "+=" Exp        ";"         {% return ()}
    | Ins BREAK             ";"         {% return ()}
    | Ins CONTINUE          ";"         {% return ()}
    | Ins RETURN   Exp      ";"         {% return ()}
    | Ins EXIT              ";"         {% return ()}
    | Ins FREE "("ID")"     ";"         {% return ()}
    | Ins FREE "("DATAID")" ";"         {% return ()}
    | Ins READ "("DATAID")" ";"         {% return ()}
    | Ins IF Exp    ":" SmplDcls Ins NextIf Else END    {% return ()}
    | Ins WHILE Exp ":" SmplDcls Ins END                {% return ()}
    | Ins FOR ID "=" Exp  "|" Exp "|" Exp ":" SmplDcls Ins  END {% return ()}
    | Ins FOR ID "=" Exp  "|" Exp         ":" SmplDcls Ins  END {% return ()}
    | Ins FOR ID "=" ENUM "|" ENUM        ":" SmplDcls Ins  END {% return ()}
    | Ins BEGIN SmplDcls Ins END    {% return ()} -- No debe aceptar funciones

PrntArgs: {- λ -}             {% return ()}
        | PrntArgs "," Exp    {% return ()} -- Siempre es necesaria una coma a la izq

NextIf: {- λ -}             {% return ()}
      | NextIf ELIF  Exp ":" SmplDcls Ins {% return ()}

Else: {- λ -}      {% return ()}
    | ELSE ":" SmplDcls Ins {% return ()}

SmplDcls: {- λ -}                                     {% return () }        
    | SmplDcls IsGlob PrimType Ptrs          ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $5) $4 Nothing) $5 }
    | SmplDcls IsGlob DataType DATAID  Ptrs  ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $6) $5 (Just (lexeme $4))) $6 }
    | SmplDcls IsGlob PrimType EmptyArrs     ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $5) $4 Nothing) $5 } -- Azucar sintactico? jeje
    | SmplDcls IsGlob PrimType StaticArrs    ID  ";"  {% insertDeclareInScope (makeArr  $3 (position $5) $4) $5 }
    | SmplDcls IsGlob PrimType               ID  ";"  {% insertDeclareInScope (makeDec  $3 (position $4) Nothing) $4 }
    | SmplDcls IsGlob DataType DATAID        ID  ";"  {% insertDeclareInScope (makeDec  $3 (position $5) (Just (lexeme $4))) $5 }

Dcls:  {- λ -}                                {% return ()}
    | Dcls FUNC PrimType ID "(" Parameter ")" ":" SmplDcls Ins END {% return ()}
    | Dcls IsGlob PrimType Ptrs       ID  ";"  {% return ()}
    | Dcls IsGlob PrimType EmptyArrs  ID  ";"  {% return ()}
    | Dcls IsGlob PrimType StaticArrs ID  ";"  {% return ()}
    | Dcls IsGlob PrimType            ID  ";"   {% return ()}
  --| Dcls DataType    DATAID     ";"          {% return ()}    -- Forward declarations
    | Dcls ENUMDEC DATAID   "{" EnumConsList "}"   {% return ()}
    | Dcls STRUCTDEC  DATAID  "{" FieldsList   "}"   {% return ()}
    | Dcls UNIONDEC  DATAID  "{" FieldsList   "}"   {% return ()}

IsGlob : {- λ -}     {% return ()}
         | GLOBAL    {% return ()}

PrimType : INTDEC         { $1 }
         | BOOLDEC        { $1 }
         | CHARDEC        { $1 }
         | VOIDDEC        { $1 }
         | FLOATDEC       { $1 }

DataType : ENUMDEC        { $1 }
         | STRUCTDEC      { $1 }
         | UNIONDEC       { $1 }

Parameter: Parameters PrimType ID         {% return ()}
         | Parameters PrimType Ptrs ID    {% return ()}
         | Parameters PrimType EmptyArrs ID   {% return ()}
         | Parameters DataType DATAID     {% return ()}

ListParam: Parameter                      {% return ()}
         | Parameters PrimType ID     "," {% return ()}
         | Parameters DataType DATAID "," {% return ()}
         | Parameters PrimType Ptrs ID ","   {% return ()}
         | Parameters PrimType EmptyArrs ID ","  {% return ()}


Parameters: {- λ -}       {% return ()}
          | ListParam     {% return ()}

EnumConsList: ENUM                      {% return ()}
            | EnumConsList "," ENUM     {% return ()}

FieldsList  : ID     "::" PrimType                   {% return ()}
            | DATAID "::" DATAID                     {% return ()}
            | FieldsList  "," ID     "::" PrimType   {% return ()}
            | FieldsList  "," DATAID "::" DATAID     {% return ()}

-- Counts nesting levels
Ptrs: "*"        {    1    }
    | Ptrs "*"   { succ $1 } 

-- Counts nesting levels
EmptyArrs: "[" "]"             {    1    }
         |  EmptyArrs "[" "]"  { succ $1 }

-- Counts dimensions
StaticArrs: "[" INT "]"             { [value $2] }
          | StaticArrs "[" INT "]"  { (value $3):$1}

Exp : 
    -- Expresiones Aritméticas.
      Exp "+" Exp       {% return ()}
    | Exp "-" Exp       {% return ()}
    | Exp "^" Exp       {% return ()}
    | Exp "*" Exp       {% return ()}
    | Exp "/" Exp       {% return ()}
    | Exp "//" Exp      {% return ()}
    | Exp "%" Exp       {% return ()}
    | "-" Exp %prec NEG {% return ()}
    -- Expresiones Booleanas.
    | Exp OR Exp        {% return ()}
    | Exp "||" Exp      {% return ()}
    | Exp AND Exp       {% return ()}
    | Exp "&&" Exp      {% return ()}
    | "!" Exp         {% return ()}
    -- Expresiones relacionales.
    | Exp "<"  Exp      {% return ()}
    | Exp "<=" Exp      {% return ()}
    | Exp ">"  Exp      {% return ()}
    | Exp ">=" Exp      {% return ()}
    | Exp "==" Exp      {% return ()}
    | Exp "!=" Exp      {% return ()}
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp           {% return ()}
    | Exp "."  Exp           {% return ()}
    | ID "[" Exp "]" %prec ARR {% return ()}
    --Llamadas a funciones
    | ID "(" Exp ")"         {% return ()}
    --Acceso a apuntadores
    | "*" Exp %prec POINT  {% return ()}
    -- Asociatividad.
    | "(" Exp ")"    {% return ()}
    -- Constantes.
    | Term           {% return ()}
    -- Llamadas
    | MALLOC "(" Exp ")"       {% return ()}
    | SIZEOF "(" Exp ")"       {% return ()}
    | SIZEOF "(" PrimType ")"  {% return ()}
    | GET    "(" ENUM ")"      {% return ()}

Term: TRUE         {   $1   }
    | FALSE        {   $1   }
    | ID           {   $1   }
    | DATAID       {   $1   }
    | FLOAT        {   $1   }
    | INT          {   $1   }
    | CHAR         {   $1   }

{

getVal (TkNum a b ) = b

-- Monadic action: Insert tkId into actual scope and do some checks
insertDeclareInScope Nothing (TkId (l,c) lexeme ) =
    tell $ S.singleton $ Left  $ "Error:" ++show l++":"++show c ++" " 
                                 ++ lexeme ++ 
    " es del tipo VOIDtorb, el cual solo puede ser instanciado como referencia."

insertDeclareInScope (Just dcltype) (TkId (l,c) lexeme ) = do 
    table <- get 
    if isMember table lexeme
        then tell error1
        else do let newtable = apply (insert lexeme dcltype) table
                put newtable
                tell whathappened
    return ()
    where error1       = S.singleton $ Left  $ "Error:" ++show l++":"++show c ++" redeclaraci'o de " ++ lexeme
          whathappened = S.singleton $ Right $ "Agregado " ++ lexeme ++ " en "++show l++":"++show c
          


parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}