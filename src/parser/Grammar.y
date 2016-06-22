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
    | Ins Exp "="  Exp   ";"  {% checkLValue (sel1 $2, sel2 $2) >> addToBlock (Assign    ExpTrue ExpTrue) >> return TypeVoid } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins Exp "*=" Exp   ";"  {% checkLValue (sel1 $2, sel2 $2) >> addToBlock (AssignMul ExpTrue ExpTrue) >> return TypeVoid } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins Exp "+=" Exp   ";"  {% checkLValue (sel1 $2, sel2 $2) >> addToBlock (AssignSum ExpTrue ExpTrue) >> return TypeVoid } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins Exp "-=" Exp   ";"  {% checkLValue (sel1 $2, sel2 $2) >> addToBlock (AssignMin ExpTrue ExpTrue) >> return TypeVoid } --(trd $2) (trd $4)) ) } --Falta caso particular para checkAssing
    | Ins BREAK          ";"  {% checkOkIns (addToBlock Break   )    $1  }
    | Ins CONTINUE       ";"  {% checkOkIns (addToBlock Continue)    $1  }
    | Ins EXIT           ";"  {% checkOkIns (addToBlock Exit    )    $1  }
    | Ins RETURN   Exp   ";"  {% checkOkIns (addToBlock (Return Nothing )) $1 } -- Cambiar para exp
    | Ins RETURN         ";"  {% checkOkIns (addToBlock (Return Nothing )) $1 }
    | Ins READ  "("  ID   ")" ";" {% checkReadable $4 True  >> return TypeVoid} --revisar
    | Ins ID    "("ExpList")" ";" {% return TypeVoid } 
    | Ins BEGIN Ent0 SmplDcls Ins END                              {% exitScope >> checkOkIns (addToBlock EnterBlock ) $1 } -- Verificar que el tipo de ins es Void y $1 
    | Ins IF Exp    ":" Ent0 SmplDcls Ins Ent1 NextIf Else END     {% checkOkIns (addToBlock (mergeIf (Guard ExpTrue) $9 $10 )) $1 } -- verificar que $3 es bool, $9 y $10 son void
    | Ins WHILE Exp ":" Ent0 SmplDcls Ins Ent1 END                 {% checkOkIns (addToBlock (While ExpTrue) ) $1  }
    | Ins FOR Ent3 "=" Exp  "|" Exp "|" Exp ":"  SmplDcls Ins  END {% exitScope >> checkOkIns (addToBlock (ForStep (ExpInt 1) (ExpInt 2) (ExpInt 1))) $1 }
    | Ins FOR Ent3 "=" Exp  "|" Exp         ":"  SmplDcls Ins  END {% exitScope >> checkOkIns (addToBlock (For (ExpInt 1) (ExpInt 2))) $1 }
    | Ins FOR Ent4 "=" ENUM "|" ENUM        ":"  SmplDcls Ins  END {% exitScope >> checkOkIns (addToBlock (For (ExpInt 1) (ExpInt 2))) $1 }




-- List of elseif
NextIf: {- λ -}                                     { newIf  }
      | NextIf ELIF  Exp ":" Ent0 SmplDcls Ins Ent1 { insertIf $1 (Guard ExpTrue)  } -- Propagar Error si los tipos no cuadran

-- Else list
Else: {- λ -}                         {  Nothing     }
    | ELSE ":" Ent0 SmplDcls Ins Ent1 { (Just Else)  }

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
    | Exp "-"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Neg (sel3 $1) (sel3 $3)) }
    | Exp "^"  Exp      {% return (TypeBool,(sel2 $1),(Binary Power (sel3 $1) (sel3 $3)))                  }
    | Exp "*"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Multiply (sel3 $1) (sel3 $3)) }   -- Float/Int and Int
    | Exp "/"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Div (sel3 $1) (sel3 $3)) }   -- Both Float
    | Exp "//" Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary FloatDiv (sel3 $1) (sel3 $3)) }   -- last one integer
    | Exp "%"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Mod (sel3 $1) (sel3 $3)) }   -- Both Integer
    | "-" Exp %prec NEG {% return (TypeBool,(sel2 $2),(Unary Neg (sel3 $2)))  }
    -- Expresiones Booleanas.
    | Exp OR Exp        {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns (Binary SOr (sel3 $1) (sel3 $3)) }
    | Exp "||" Exp      {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns (Binary Or (sel3 $1) (sel3 $3)) }
    | Exp AND Exp       {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns (Binary SAnd (sel3 $1) (sel3 $3)) }
    | Exp "&&" Exp      {% checkBinary [TypeBool] (sel1 $1) (sel1 $3) $2 (sel2 $1)  >>= expIns (Binary And (sel3 $1) (sel3 $3)) }
    | "!" Exp           {% return (TypeBool,(sel2 $2),(Unary Not (sel3 $2)))  }
    -- Expresiones relacionales.
    | Exp "<"  Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Less       (sel3 $1) (sel3 $3)) }
    | Exp "<=" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary LessEql    (sel3 $1) (sel3 $3)) }
    | Exp ">"  Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Greater    (sel3 $1) (sel3 $3)) }
    | Exp ">=" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary GreaterEql (sel3 $1) (sel3 $3)) }
    | Exp "==" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Eql        (sel3 $1) (sel3 $3)) }
    | Exp "!=" Exp      {% checkComp (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary NotEql     (sel3 $1) (sel3 $3)) }
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp            {% return (TypeBool,(sel2 $1),(Binary Access     (sel3 $1) (sel3 $3))) }
    | Exp "."  ID             {% checkFieldAccess $1 $3  >>= expIns (NoExp) } -- Binary Access (sel3 $1) (sel3 $3)
    | ID SquareList %prec ARR {% return (TypeBool,$1,NoExp)  }
    --Llamadas a funciones
    | ID "(" ExpList ")"   {% checkFunctionCall $1 $3  >>= expIns NoExp } 
    --Acceso a apuntadores
    | "*" Exp %prec POINT  {% return (TypeBool,(sel2 $2),Unary Access (sel3 $2)) }
    --Direccion de variable
    | "&" Exp %prec AMP    {% return (TypeBool,(sel2 $2),Unary Address (sel3 $2))  }
    -- Asociatividad.
    | "(" Exp ")"    {% return $2 }
    -- Constantes.
    -- Llamadas
    | SIZEOF "(" Reference ")" {% return (TypeInt,$1,NoExp) } -- Can be known at compile time
    -- | GET    "(" ENUM ")"      {% return (TypeBool,(sel2 $2),NoExp) } -- Si no lo hacemos por gramatica, mejor error pero no se puede conocer a tiempo de compilacion
    | GET    "(" ENUM ")"      {% return (TypeBool,$3,ExpEnum  (lexeme $3)) }
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