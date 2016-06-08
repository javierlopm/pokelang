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
import Data.Sequence

}


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
    "&"       { TkAmp    _ }
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
--    PRINT     { TkPrint  _ }
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

-- Direccion de variable
%right AMP

%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="



%name parser


%%

Prog : Dcls  {% return ()}

Ins : {- λ -}                   {% return () }
    | Ins Exp "="  Exp   ";"    {% return () } -- {% checkLIter $2}
    | Ins Exp "*=" Exp   ";"    {% return () } -- {% checkLIter $2}
    | Ins Exp "+=" Exp   ";"    {% return () } -- {% checkLIter $2}
    | Ins Exp "-=" Exp   ";"    {% return () } -- {% checkLIter $2}
    | Ins BREAK          ";"    {% return () }
    | Ins CONTINUE       ";"    {% return () }
    | Ins RETURN   Exp   ";"    {% return () }
    | Ins RETURN         ";"    {% return () }
    | Ins EXIT           ";"    {% return () } 
--    | Ins PRINT "(" STRING PrntArgs ")" ";" {% onStrScope $ insert (content $4) Types.Empty  }
    | Ins READ  "("       ID        ")" ";" {% checkReadable $4 True}
    | Ins WRITE "("       ID        ")" ";" {% checkReadable $4 False}
    | Ins FREE  "("       ID        ")" ";" {% return ()}
    | Ins FREE  "("     DATAID      ")" ";" {% return ()}
    | Ins READ  "("     DATAID      ")" ";" {% return ()}
    | Ins BEGIN Ent0 SmplDcls Ins END       {% exitScope  } -- No debe aceptar funciones
    | Ins IF Exp    ":" Ent0 SmplDcls Ins Ent1 NextIf Else END    {% return ()  }
    | Ins WHILE Exp ":" Ent0 SmplDcls Ins Ent1 END                {% return ()  }
    | Ins FOR Ent3 "=" Exp  "|" Exp "|" Exp ":"  SmplDcls Ins  END {% exitScope  }
    | Ins FOR Ent3 "=" Exp  "|" Exp         ":"  SmplDcls Ins  END {% exitScope  }
    | Ins FOR Ent4 "=" ENUM "|" ENUM        ":"  SmplDcls Ins  END {% exitScope  }

-- Print arguments
PrntArgs: {- λ -}             {% return ()}
        | PrntArgs "," Exp    {% return ()} 

-- List of elseif
NextIf: {- λ -}                                {% return ()}
      | NextIf ELIF  Exp ":" Ent0 SmplDcls Ins Ent1 {% return ()  }

-- Else list
Else: {- λ -}                    {% return ()  }
    | ELSE ":" Ent0 SmplDcls Ins Ent1 {% return ()  }

-- Declarations that could be global.
SmplDcls: {- λ -}                       {% return ()}        
        | SmplDcls GlobDeclare ID ";"   {% insertDeclareInScope (fst $2) $3 (snd $2) False }

-- Global declarations of references
GlobDeclare : Reference          { ( $1 ,False) }
            | GLOBAL Reference   { ( $2 ,True ) }

-- Randomly nested Pointer-Array-Empty_Arrray references (or not)
Reference: PrimType              {            $1           }
         | Reference "*"         { TypePointer    $1       }
         | Reference "[" "]"     { TypeEmptyArray $1       }
         | Reference "[" INT "]" { TypeArray $1 (value $3) }

PrimType : INTDEC           {     makeType $1    }
         | BOOLDEC          {     makeType $1    }
         | CHARDEC          {     makeType $1    }
         | VOIDDEC          {     makeType $1    }
         | FLOATDEC         {     makeType $1    }
         | ENUMDEC   DATAID {% checkItsDeclared $2 >> return(makeDataType $1 $2) }
         | STRUCTDEC DATAID {% checkItsDeclared $2 >> return(makeDataType $1 $2) }
         | UNIONDEC  DATAID {% checkItsDeclared $2 >> return(makeDataType $1 $2) }


-- Global declarations on scope level 0
Dcls:  {- λ -}                          {% return () }
    | Dcls Reference   ID         ";"   {% insertDeclareInScope $2 $3 True False } -- Always global, GlobDeclare not needed
    | Dcls FWD FUNC Reference ID Ent0 "(" Parameters ")" ";"   {% insertForwardFunc (addType $8 $4) $5 }
    | Dcls ENUMDEC DATAID "{" EnumConsList "}"  {% insertEnum $3 }
    | Dcls Ent5 "{" FieldsList "}" {% checkRecursiveDec (snd $2) $4 >> insertData $2 $4  }
    | Dcls Ent6 "{" FieldsList "}" {% checkRecursiveDec (snd $2) $4 >> insertData $2 $4  }
    | Dcls FUNC  Ent2  ":" Ent0 SmplDcls Ins END -- Ent0 Ent5
    {% insertFunction (snd $3) (fst $3) True }


-- Parameter: ListParam Reference ID  {% insertDeclareInScope $2 $3 False False }    -- Falta Hacer la lista de tipos
        
Parameters: {- λ -}                 {% return emptytuple } 
          | ListParam Reference ID  {% insertDeclareInScope $2 $3 False False >> 
                                         return (addType $1 $2 ) } 

ListParam: {- λ -}                    {% return emptytuple }
         | ListParam Reference ID "," {% insertDeclareInScope $2 $3 False False >> return(addType $1 $2 ) } -- Falta Hacer la lista de tipos
         

EnumConsList: ENUM                      {% insertEnumCons 1  $1 }
            | EnumConsList "," ENUM     {% insertEnumCons $1 $3 }

FieldsList  : ID "::" Reference                  {% (insertDeclareInScope $3 $1 False False) >> 
                                                        return(addType emptytuple (TypeField (lexeme $1) $3))  }
            | FieldsList  "," ID "::" Reference  {% (insertDeclareInScope $5 $3 False False) >> 
                                                        return(addType $1 (TypeField (lexeme $3) $5)) } 

ExpList: {- λ -}        { emptytuple }
       | ExpFirsts Exp  { $1 `addType` (fst $2) }  --Revisar

ExpFirsts : {- λ -}         { emptytuple }
          | ExpFirsts Exp "," { $1 `addType` (fst $2) }  --Revisar

Exp : 
    -- Expresiones Aritméticas.
      Exp "+"  Exp      {% checkBinary nums (fst $1) (fst $3) $2 (snd $1) }
    | Exp "-"  Exp      {% checkBinary nums (fst $1) (fst $3) $2 (snd $1) }
    | Exp "^"  Exp      {% return (TypeBool,(snd $1))                 }
    | Exp "*"  Exp      {% checkBinary nums (fst $1) (fst $3) $2 (snd $1) }   -- Float/Int and Int
    | Exp "/"  Exp      {% checkBinary nums (fst $1) (fst $3) $2 (snd $1) }   -- Both Float
    | Exp "//" Exp      {% checkBinary nums (fst $1) (fst $3) $2 (snd $1) }   -- last one integer
    | Exp "%"  Exp      {% checkBinary nums (fst $1) (fst $3) $2 (snd $1) }   -- Both Integer
    | "-" Exp %prec NEG {% return (TypeBool,(snd $2)) }
    -- Expresiones Booleanas.
    | Exp OR Exp        {% checkBinary [TypeBool] (fst $1) (fst $3) $2 (snd $1)  }
    | Exp "||" Exp      {% checkBinary [TypeBool] (fst $1) (fst $3) $2 (snd $1)  }
    | Exp AND Exp       {% checkBinary [TypeBool] (fst $1) (fst $3) $2 (snd $1)  }
    | Exp "&&" Exp      {% checkBinary [TypeBool] (fst $1) (fst $3) $2 (snd $1)  }
    | "!" Exp           {% return (TypeBool,(snd $2)) }
    -- Expresiones relacionales.
    | Exp "<"  Exp      {% return (TypeBool,(snd $1)) }
    | Exp "<=" Exp      {% return (TypeBool,(snd $1)) }
    | Exp ">"  Exp      {% return (TypeBool,(snd $1)) }
    | Exp ">=" Exp      {% return (TypeBool,(snd $1)) }
    | Exp "==" Exp      {% return (TypeBool,(snd $1)) }
    | Exp "!=" Exp      {% return (TypeBool,(snd $1)) }
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp             {% return (TypeBool,(snd $1)) }
    | Exp "."  Exp             {% return (TypeBool,(snd $1)) }
    | ID "[" Exp "]" %prec ARR {% return (TypeBool,$1) }
    --Llamadas a funciones
    | ID "(" ExpList ")"   {% checkIsFunc $1 >> return (TypeBool,$1) } --Arreglar llamadas gramatica todo {% checkIsFunc $1 >> return (TypeBool,$1) } FALLA
    --Acceso a apuntadores
    | "*" Exp %prec POINT  {% return (TypeBool,(snd $2)) }
    --Direccion de variable
    | "&" Exp %prec AMP    {% return (TypeBool,(snd $2)) }
    -- Asociatividad.
    | "(" Exp ")"    {% return (TypeBool,(snd $2)) }
    -- Constantes.
    --| Term           { TypeBool }
    -- Llamadas
    | MALLOC "(" Exp ")"       {% return (TypeBool,(snd $3)) }
    | SIZEOF "(" Exp ")"       {% return (TypeBool,(snd $3)) }
    | SIZEOF "(" Reference ")" {% return (TypeBool,$1) }
    | GET    "(" ENUM ")"      {% return (TypeBool,$3) }
    | TRUE      {% return (TypeBool,$1) }   
    | FALSE     {% return (TypeBool,$1) }   
    | ID        {% checkItsDeclared $1  }   -- {% checkItsDeclared $1 >> return $1 }
    | DATAID    {% return (TypeError,$1)}   -- {  $1  } ???? check its declared
    | FLOAT     {% return (TypeFloat,$1)}   
    | INT       {% return (TypeInt,$1)  }   
    | CHAR      {% return (TypeChar,$1) }   

Ent0 : {- λ -}     {% onZip enterScope }
Ent1 : {- λ -}     {% exitScope  }
-- Ent1 : DATAID      {% insertEmpty $1  >> return $1 } 
Ent2 : Reference ID "(" Parameters  ")" {% insertFunction ($4 `addType` $1) $2 False
                                               >> return($2,($4 `addType` $1)) } 
Ent3 : ID          {%  onZip enterScope >>
                         insertDeclareInScope TypeInt $1 False True >>
                            return $1                             } 
Ent4 : DATAID ID   {% onZip enterScope >> checkEnumAndInsert $1 $2 >> return $1 } 
Ent5 : STRUCTDEC  DATAID {% insertForwardData $1 $2 >> return ($1,$2)}
Ent6 : UNIONDEC   DATAID {% insertForwardData $1 $2 >> return ($1,$2)}

{
  
parseError [] = error $ "EOF unexpected"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}