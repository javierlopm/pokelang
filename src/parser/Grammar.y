{
module Grammar(
    parser,
    makeTable,
    initialState
) where

import Tokens
import TableTree
import Types
import GrammarMonad


}


%name parser
%tokentype { Token }

%monad { OurMonad }

%error     { parseError }
%token
    ID        { TkId       _ _ }     
    DATAID    { TkDId      _ _ }
    -- Declarations       
    FWD       { TkFwd      _ } 
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
    "-="      { TkMEQ      _ }    
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

Ins : {- λ -}                                 {% return () }
    | Ins PRINT "(" STRING PrntArgs ")"   ";" {% onStrScope $ insert (content $4) (Cons (position $4)) }
    | Ins READ  "("       ID        ")"   ";" {% checkReadable $4 True}
    | Ins WRITE "("       ID        ")"   ";" {% checkReadable $4 False}
    | Ins Exp "=" Exp         ";"         {% checkLIter $2}
    | Ins Exp "*=" Exp        ";"         {% checkLIter $2 }
    | Ins Exp "+=" Exp        ";"         {% checkLIter $2}
    | Ins Exp "-=" Exp        ";"         {% checkLIter $2}
    | Ins BREAK             ";"         {% return ()} --√
    | Ins CONTINUE          ";"         {% return ()} --√
    | Ins RETURN   Exp      ";"         {% return ()} --√
    | Ins RETURN            ";"         {% return ()} --En los void?  --√
    | Ins EXIT              ";"         {% return ()} --√ 
    | Ins FREE "("ID")"     ";"         {% return ()} --√
    --| Ins FREE "("DATAID")" ";"         {% return ()}
    --| Ins READ "("DATAID")" ";"         {% return ()}
    | Ins IF Exp    ":" Ent0 SmplDcls Ins NextIf Else END            {% exitScope  }
    | Ins WHILE Exp ":" Ent0 SmplDcls Ins END                        {% exitScope  }
    | Ins FOR Ent0 Ent3 "=" Exp  "|" Exp "|" Exp ":"  SmplDcls Ins  END {% exitScope  }
    | Ins FOR Ent0 Ent3 "=" Exp  "|" Exp         ":"  SmplDcls Ins  END {% exitScope  }
    | Ins FOR Ent0 Ent3 "=" ENUM "|" ENUM        ":"  SmplDcls Ins  END {% exitScope  }
    | Ins BEGIN Ent0 SmplDcls Ins END                                {% exitScope  } -- No debe aceptar funciones

PrntArgs: {- λ -}             {% return ()}
        | PrntArgs "," Exp    {% return ()} -- Siempre es necesaria una coma a la izq

NextIf: {- λ -}             {% return ()}
      | NextIf ELIF  Exp ":" Ent0 SmplDcls Ins {% exitScope  }

Else: {- λ -}                   {% return ()}
    | ELSE ":" Ent0 SmplDcls Ins {% exitScope  }


SmplDcls: {- λ -}                             {% return ()}        
        | SmplDcls GlobDeclare ";"              {% return ()}

Dcls:  {- λ -}                                       {% return ()}
    | Dcls FUNC PrimType Ent2 "(" Parameters ")" ":" SmplDcls Ins END {% insertFunction $3 $4 }
    | Dcls Reference  ";"            {% return ()} -- Always global
    | Dcls FWD DataType  DATAID     ";"           {% return ()}    -- Forward declarations solo, agregar con Dec Empty
    | Dcls ENUMDEC    Ent1 "{" EnumConsList "}"   {% insertEnum $3 }
    | Dcls STRUCTDEC  Ent1 "{" FieldsList   "}"   {% insertData $3  True }
    | Dcls UNIONDEC   Ent1 "{" FieldsList   "}"   {% insertData $3  False }

GlobDeclare : Reference          { False }
            | GLOBAL Reference   { True  }

Reference: PrimType              {return ()}
         | Reference PrimType    {return ()}
         | Reference "*"         {return ()}
         | Reference "[" "]"     {return ()}
         | Reference "[" INT "]" {return ()}

PrimType : INTDEC    ID        { $1 }
         | BOOLDEC   ID        { $1 }
         | CHARDEC   ID        { $1 }
         | VOIDDEC   ID        { $1 }
         | FLOATDEC  ID        { $1 }
         | ENUMDEC   DATAID ID { $1 }
         | STRUCTDEC DATAID ID { $1 }
         | UNIONDEC  DATAID ID { $1 }

Parameter: ListParam Reference   {% insertDeclareInScope   (makeDec  $2 (position $0) Nothing) $0 False }
        

ListParam: {- λ -}                  {% return () }
         | ListParam Reference ","  {% insertDeclareInScope   (makeDec  $2 (position $0) Nothing) $3 False }
         
Parameters: {- λ -}       {% onZip enterScope } -- Tiene sentido?
          | Parameter     {% onZip enterScope } 

EnumConsList: ENUM                      {% insertEnumCons 1  $1 }
            | EnumConsList "," ENUM     {% insertEnumCons $1 $3 }

CleanRef : CleanType             {return ()}
         | Reference CleanType   {return ()}
         | Reference "*"         {return ()}
         | Reference "[" "]"     {return ()}
         | Reference "[" INT "]" {return ()}

CleanType : INTDEC           { $1 }
          | BOOLDEC          { $1 }
          | CHARDEC          { $1 }
          | VOIDDEC          { $1 }
          | FLOATDEC         { $1 }
          | ENUMDEC   DATAID { $1 }
          | STRUCTDEC DATAID { $1 }
          | UNIONDEC  DATAID { $1 }

FieldsList  : ID "::" CleanRef                  {% return ()}
            | FieldsList  "," ID "::" CleanRef  {% return () } --verificar que realmente existe

-- FieldsList  : ID "::" PrimType            {% insertDeclareInScope   (makeDec  $3 (position $1)   Nothing) $1 False}
--             | ID "::" Ptrs PrimType       {% insertDeclareInScope   (makePtrs $4 (position $1) $3 Nothing) $1  False }
--             | ID "::" DataType DATAID     {% insertDeclareInScope   (makeDec  $3 (position $1) (Just (lexeme $4))) $1 False} --verificar que realmente existe
--             | ID "::" Ptrs DataType DATAID             {% insertDeclareInScope   (makePtrs $4 (position $1) $3 (Just (lexeme $5))) $1  False } --verificar que realmente existe
--             | FieldsList  "," ID "::" PrimType         {% insertDeclareInScope   (makeDec  $5 (position $3) Nothing) $3 False    }
--             | FieldsList  "," ID "::" Ptrs PrimType    {% insertDeclareInScope   (makePtrs $6 (position $2) $5 Nothing) $3  False}
--             | FieldsList  "," ID "::" DataType DATAID  {% insertDeclareInScope   (makeDec  $5 (position $3) (Just (lexeme $6))) $3 False} --verificar que realmente existe
--             | FieldsList  "," ID "::" Ptrs DataType DATAID  {% insertDeclareInScope (makePtrs $6 (position $3) $5 (Just (lexeme $7))) $3  False } --verificar que realmente existe



Exp : 
    -- Expresiones Aritméticas.
      Exp "+" Exp       { $1 }
    | Exp "-" Exp       { $1 }
    | Exp "^" Exp       { $1 }
    | Exp "*" Exp       { $1 }
    | Exp "/" Exp       { $1 }
    | Exp "//" Exp      { $1 }
    | Exp "%" Exp       { $1 }
    | "-" Exp %prec NEG { $2 }
    -- Expresiones Booleanas.
    | Exp OR Exp        { $1 }
    | Exp "||" Exp      { $1 }
    | Exp AND Exp       { $1 }
    | Exp "&&" Exp      { $1 }
    | "!" Exp           { $2 }
    -- Expresiones relacionales.
    | Exp "<"  Exp      { $1 }
    | Exp "<=" Exp      { $1 }
    | Exp ">"  Exp      { $1 }
    | Exp ">=" Exp      { $1 }
    | Exp "==" Exp      { $1 }
    | Exp "!=" Exp      { $1 }
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp           { $1 }
    | Exp "."  Exp           { $1 }
    | ID "[" Exp "]" %prec ARR { $1 }
    --Llamadas a funciones
    | ID "(" Exp ")"         {% do checkIsFunc $1 >> return $3 }  --Arreglar llamadas gramatica todo
    --Acceso a apuntadores
    | "*" Exp %prec POINT  { $2 }
    -- Asociatividad.
    | "(" Exp ")"    { $2 }
    -- Constantes.
    | Term           { $1  }
    -- Llamadas
    | MALLOC "(" Exp ")"       { $3 }
    | SIZEOF "(" Exp ")"       { $3 }
    | SIZEOF "(" PrimType ")"  { $3 }
    | GET    "(" ENUM ")"      { $3 }

Term: TRUE         {% return($1) }
    | FALSE        {% return($1) }
    | ID           {% checkItsDeclared $1 }
    | DATAID       {  $1  }
    | FLOAT        {% return($1) }
    | INT          {% return($1) }
    | CHAR         {% return($1) }

Ent0  : {- λ -}    {% onZip enterScope  }
Ent1 : DATAID      {% insertCheckFunc $1 >> return $1 } 
Ent2 : ID          {% insertCheckFunc $1 >> return $1 } 
Ent3 : ID          {% insertDeclareInScope (makeIter $1 (position $1)) $1 False } 

{
  
parseError [] = error $ "EOF unexpected"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}