{
module Grammar(parser) where
import Tokens
import TableTree
import Control.Monad.RWS.Strict
import qualified Data.Sequence as S
}


%name parser
%tokentype { Token      }

%monad { RWS String (S.Seq(String)) (Zipper Pos) }

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
    | Ins RETURN            ";"         {% return ()}
    | Ins EXIT              ";"         {% return ()}
    | Ins FREE "("ID")"     ";"         {% return ()}
    | Ins FREE "("DATAID")" ";"         {% return ()}
    | Ins READ "("DATAID")" ";"         {% return ()}
    | Ins IF Exp ":" SmplDcls Ins NextIf Else END    {% return ()}
    | Ins WHILE Exp ":" SmplDcls Ins END         {% return ()}
    | Ins FOR ID "=" Exp  "|" Exp "|" Exp ":" SmplDcls Ins  END {% return ()}
    | Ins FOR ID "=" Exp  "|" Exp         ":" SmplDcls Ins  END {% return ()}
    | Ins FOR ID "=" ENUM "|" ENUM        ":" SmplDcls Ins  END {% return ()}
    | Ins BEGIN SmplDcls Ins END    {% return ()} -- No debe aceptar funciones

PrntArgs: {- λ -}             {% return ()}
        | PrntArgs "," Exp    {% return ()} -- Siempre es necesaria una coma a la izq

NextIf: {- λ -}             {% return ()}
      | NextIf ELIF  Exp ":" Ins {% return ()}

Else: {- λ -}      {% return ()}
    | ELSE ":" Ins {% return ()}

SmplDcls: {- λ -}                                  {% return ()}        
    | SmplDcls IsGlob PrimType Ptrs       ID  ";"  {% return ()}
    | SmplDcls IsGlob PrimType EmptyArrs  ID  ";"  {% return ()}
    | SmplDcls IsGlob PrimType StaticArrs ID  ";"  {% return ()}
    | SmplDcls IsGlob PrimType            ID  ";"  {% do 
                                                         tabla <- get
                                                         put $ apply (insert (lexeme $4) (position $4)) tabla
                                                   }

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

DataType : ENUMDEC        {% return ()}
         | STRUCTDEC      {% return ()}
         | UNIONDEC       {% return ()}

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

Ptrs: "*"        {% return ()}
    | Ptrs "*"   {% return ()}

EmptyArrs: "[" "]"             {% return ()}
         |  EmptyArrs "[" "]"  {% return ()}

StaticArrs: "[" INT "]"             {% return ()}
          | StaticArrs "[" INT "]"  {% return ()}

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

Term: TRUE         {% return ()}
    | FALSE        {% return ()}
    | ID           {% return ()}
    | DATAID       {% return ()}
    | FLOAT        {% return ()}
    | INT          {% return ()}
    | CHAR         {% return ()}

{

parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}