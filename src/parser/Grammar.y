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
import Instructions

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
    -- WRITE     { TkWrite  _ }
    -- PRINT     { TkPrint  _ }
    -- MALLOC    { TkAlloc  _ }
    -- FREE      { TkFree   _ }
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

Prog : Dcls  {% checkMain }

Ins : {- λ -}                 {% return TypeVoid }
    | Ins Exp "="  Exp   ";"  {% checkLValue (sel1 $2,sel2 $2) >>= checkOkIns (addToBlock (Assign    ExpTrue ExpTrue) ) True } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins Exp "*=" Exp   ";"  {% checkLValue (sel1 $2,sel2 $2) >>= checkOkIns (addToBlock (AssignMul ExpTrue ExpTrue) ) True } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins Exp "+=" Exp   ";"  {% checkLValue (sel1 $2,sel2 $2) >>= checkOkIns (addToBlock (AssignSum ExpTrue ExpTrue) ) True } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins Exp "-=" Exp   ";"  {% checkLValue (sel1 $2,sel2 $2) >>= checkOkIns (addToBlock (AssignMin ExpTrue ExpTrue) ) True } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins BREAK          ";"  {% checkOkIns (addToBlock Break   )   True $1 }
    | Ins CONTINUE       ";"  {% checkOkIns (addToBlock Continue)   True $1 }
    | Ins EXIT           ";"  {% checkOkIns (addToBlock Exit    )   True $1 }
    | Ins RETURN   Exp   ";"  {% checkOkIns (addToBlock (Return Nothing )) True $1 } -- Cambiar para exp
    | Ins RETURN         ";"  {% checkOkIns (addToBlock (Return Nothing )) True $1 }
    | Ins READ  "("  ID   ")" ";" {% checkReadable $4 True  >>= checkOkIns (addToBlock (Return Nothing )) True } --Camiar Nothing por (Read $4)
    | Ins BEGIN Ent0 SmplDcls Ins END       {% exitScope    >> return TypeVoid} -- No debe aceptar funciones
    | Ins IF Exp    ":" Ent0 SmplDcls Ins Ent1 NextIf Else END    {% return TypeVoid  }
    | Ins WHILE Exp ":" Ent0 SmplDcls Ins Ent1 END                {% return TypeVoid  }
    | Ins FOR Ent3 "=" Exp  "|" Exp "|" Exp ":"  SmplDcls Ins  END {% exitScope   >> return TypeVoid}
    | Ins FOR Ent3 "=" Exp  "|" Exp         ":"  SmplDcls Ins  END {% exitScope   >> return TypeVoid}
    | Ins FOR Ent4 "=" ENUM "|" ENUM        ":"  SmplDcls Ins  END {% exitScope   >> return TypeVoid}



-- List of elseif
NextIf: {- λ -}                                {% return ()}
      | NextIf ELIF  Exp ":" Ent0 SmplDcls Ins Ent1 {% return ()  }

-- Else list
Else: {- λ -}                         {% return ()  }
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
         | Reference "[" INT "]" { TypeArray $1 (value $3) }
         -- | Reference "[" "]"     { TypeEmptyArray $1       }

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
    | Dcls ENUMDEC DATAID "{" EnumConsList "}"  {% insertEnum $3 >> insertLEnumCons $5 (lexeme $3) }
    | Dcls Ent5 "{" FieldsList "}" {% checkRecursiveDec (snd $2) $4 >> insertData $2 $4  }
    | Dcls Ent6 "{" FieldsList Ent7 "}" {% checkRecursiveDec (snd $2) $4 >> insertData $2 $4  }
    | Dcls FUNC  Ent2  ":" Ent0 SmplDcls Ins END -- Ent0 Ent5
    {% insertFunction (snd $3) (fst $3) True }


-- Parameter: ListParam Reference ID  {% insertDeclareInScope $2 $3 False False }    -- Falta Hacer la lista de tipos
        
Parameters: {- λ -}                 {% return emptytuple } 
          | ListParam Reference ID  {% insertParamInScope $2 $3 False False >> 
                                         return (addType $1 $2 ) } 

ListParam: {- λ -}                    {% return emptytuple }
         | ListParam Reference ID "," {% insertParamInScope $2 $3 False False >> return(addType $1 $2 ) } -- Falta Hacer la lista de tipos
         

EnumConsList: ENUM                      {% return([(1,$1)]) }
            | EnumConsList "," ENUM     {% return((1,$3):$1) }

FieldsList  : ID "::" Reference                  {% (insertDeclareInScope $3 $1 False False) >> 
                                                        return(addType emptytuple (TypeField (lexeme $1) $3))  }
            | FieldsList  "," ID "::" Reference  {% (insertDeclareInScope $5 $3 False False) >> 
                                                        return(addType $1 (TypeField (lexeme $3) $5)) } 

ExpList: {- λ -}        { emptytuple }
       | ExpFirsts Exp  { $1 `addType` (sel1 $2) } 

ExpFirsts : {- λ -}         { emptytuple }
          | ExpFirsts Exp "," { $1 `addType` (sel1 $2) } 



Exp :  -- Cambiar los NoExp por las Exp
    -- Expresiones Aritméticas.
      Exp "+"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Plus (sel3 $1) (sel3 $3)) } --Crear funcion reciba operador y args
    | Exp "-"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    | Exp "^"  Exp      {% return (TypeBool,(sel2 $1),NoExp)                  }
    | Exp "*"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }   -- Float/Int and Int
    | Exp "/"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }   -- Both Float
    | Exp "//" Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }   -- last one integer
    | Exp "%"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }   -- Both Integer
    | "-" Exp %prec NEG {% return (TypeBool,(sel2 $2),NoExp)  }
    -- Expresiones Booleanas.
    | Exp OR Exp        {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns NoExp }
    | Exp "||" Exp      {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns NoExp }
    | Exp AND Exp       {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns NoExp }
    | Exp "&&" Exp      {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns NoExp }
    | "!" Exp           {% return (TypeBool,(sel2 $2),NoExp)  }
    -- Expresiones relacionales.
    | Exp "<"  Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    | Exp "<=" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    | Exp ">"  Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    | Exp ">=" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    | Exp "==" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    | Exp "!=" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns NoExp }
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp            {% return (TypeBool,(sel2 $1),NoExp) }
    | Exp "."  ID             {% checkFieldAccess $1 $3  >>= expIns NoExp } 
    | ID SquareList %prec ARR {% return (TypeBool,$1,NoExp)  }
    --Llamadas a funciones
    | ID "(" ExpList ")"   {% checkFunctionCall $1 $3  >>= expIns NoExp } 
    --Acceso a apuntadores
    | "*" Exp %prec POINT  {% return (TypeBool,(sel2 $2),NoExp) }
    --Direccion de variable
    | "&" Exp %prec AMP    {% return (TypeBool,(sel2 $2),NoExp)  }
    -- Asociatividad.
    | "(" Exp ")"    {% return (TypeBool,(sel2 $2),NoExp) }
    -- Constantes.
    -- Llamadas
    | SIZEOF "(" Reference ")" {% return (TypeInt,$1,NoExp) } -- Can be known at compile time
    -- | GET    "(" ENUM ")"      {% return (TypeBool,(sel2 $2),NoExp) } -- Si no lo hacemos por gramatica, mejor error pero no se puede conocer a tiempo de compilacion
    | GET    "(" ENUM ")"      {% return (TypeBool,$3,NoExp) }
    | TRUE      {% return (TypeBool,$1,ExpTrue) }   
    | FALSE     {% return (TypeBool,$1,ExpFalse) }   
    | ID        {% checkItsDeclared $1  >>= expIns (ExpVar (lexeme $1))  } 
    | DATAID    {% return (TypeError,$1,ExpVar (lexeme $1)) } -- check its declared
    | FLOAT     {% return (TypeFloat,$1,ExpFloat (rep $1)) }   
    | INT       {% return (TypeInt,$1,ExpInt (value $1)) }   
    | CHAR      {% return (TypeChar,$1,ExpChar (char $1)) }  

 

SquareList: "[" Exp "]"            { (sel1 $2,sel2 $2) } -- Check it's int and acc number of nesting
          | SquareList "[" Exp "]" { (sel1 $3,sel2 $3) } -- Check it's int and acc number of nesting

Ent0 : {- λ -}     {% onZip enterScope }
Ent1 : {- λ -}     {% exitScope  }
Ent2 : Reference ID "(" Parameters  ")" {% insertFunction ($4 `addType` $1) $2 False
                                            >> cleanParams
                                               >> return($2,($4 `addType` $1)) } 
Ent3 : ID          {%  onZip enterScope >>
                         insertDeclareInScope TypeInt $1 False True >>
                            return $1                             } 
Ent4 : DATAID ID   {% onZip enterScope >> 
                            checkEnumAndInsert $1 $2 >> 
                                return $1 } 
Ent5 : STRUCTDEC  DATAID {% insertForwardData $1 $2 >> 
                                return ($1,$2)}
Ent6 : UNIONDEC   DATAID {% toggleUnion >> 
                                insertForwardData $1 $2 >> 
                                    return ($1,$2)}
Ent7 : {- λ -}    {% toggleUnion }

{


checkComp a b c d = do 
    res <- checkBinary nums a b c d
    return (transformType (fst res) TypeBool , snd res)

parseError [] = error $ "EOF unexpected"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}