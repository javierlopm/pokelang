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

%left "!!"

-- Para los enteros.
%left "+" "-"
%left "*" "/" "//" "%"
%left "^"
%right NEG      -- Para el - unario.

--Strucs Union y Arreglos
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

Prog : Dcls  {% checkMain' $1 }

Ins : {- λ -}                 {% return (TypeVoid, newBlock ) }
    | Ins Exp "="  Exp   ";"  {% checkAllOk [checkLValue (sel1 $2, sel2 $2), (return (fst $1))] (sel1 $2) (sel1 $4) (lexeme (sel2 $4)) 0 >>= checkOkIns (Assign    (sel3 $2) (sel3 $4)) (snd $1)  } 
    | Ins Exp "*=" Exp   ";"  {% checkAllOk [checkLValue (sel1 $2, sel2 $2), (return (fst $1))] TypeVoid TypeVoid "" 1 >>= checkOkIns (AssignMul (sel3 $2) (sel3 $4)) (snd $1)  } 
    | Ins Exp "+=" Exp   ";"  {% checkAllOk [checkLValue (sel1 $2, sel2 $2), (return (fst $1))] TypeVoid TypeVoid "" 2 >>= checkOkIns (AssignSum (sel3 $2) (sel3 $4)) (snd $1)  } 
    | Ins Exp "-=" Exp   ";"  {% checkAllOk [checkLValue (sel1 $2, sel2 $2), (return (fst $1))] TypeVoid TypeVoid "" 3 >>= checkOkIns (AssignMin (sel3 $2) (sel3 $4)) (snd $1)  } 
    | Ins BREAK          ";"  {% checkOkIns Break    (snd $1) (fst $1)  }
    | Ins CONTINUE       ";"  {% checkOkIns Continue (snd $1) (fst $1)  }
    | Ins EXIT           ";"  {% checkOkIns Exit     (snd $1) (fst $1)  }
    | Ins RETURN   Exp   ";"  {% checkOkIns ((Return (Just (sel3 $3)) )) (snd $1) (fst $1) } -- Cambiar para exp
    | Ins RETURN         ";"  {% checkOkIns ((Return Nothing )) (snd $1) (fst $1) }
    | Ins READ  "("  ID   ")" ";" {% checkReadable $4 True  >>= checkOkIns ((Read . ExpVar .lexeme) $4) (snd $1) } --revisar
    | Ins ID    "("ExpList")" ";" {% (checkFunctionCall $2 (fst $4)) >>=  (checkOkIns (Call (lexeme $2) (snd $4) ) (snd $1)) . (notErrors (fst $1)) . fst   } 
    | Ins BEGIN Ent0 SmplDcls Ins END   {% exitScope >>   checkOkIns (snd $5) (snd $1) (notErrors (fst $1) (fst $5)) } 
    | Ins IF    Exp ":" Ent0 SmplDcls Ins Ent1 NextIf Else END     {% checkAllOk [(checkGuarded $2 $3 $7), (return (fst $10)), (return (fst $1))] TypeVoid TypeVoid "" 0 >>= checkOkIns (mergeIf (Guard (trd $3) (snd $7)) (snd $9) (snd $10) ) (snd $1) } --{% checkOkIns (addToBlock (mergeIf (Guard ExpTrue) $9 $10 )) $1 } -- verificar que $3 es bool, $9 y $10 son void
    | Ins WHILE Exp ":" Ent0 SmplDcls Ins Ent1 END                 {% checkAllOk [(checkGuarded $2 $3 $7), (return (fst $1))] TypeVoid TypeVoid "" 0                     >>= checkOkIns (While (trd $3) (snd $7) ) (snd $1) }               --{% checkOkIns (addToBlock (While ExpTrue) ) $1  }
    -- left it here
    | Ins FOR Ent3 "=" Exp  "|" Exp "|" Exp ":"  SmplDcls Ins  END {% exitScope >> checkAllOk [ checkFor     $2   [$5,$7,$9] , return (fst $12), return (fst $1) ] TypeVoid TypeVoid "" 0 >>= checkOkIns (ForStep (trd $5) (trd $7) (trd $9) (snd $12)) (snd $1) } -- MISSING INSTRUCTIONS
    | Ins FOR Ent3 "=" Exp  "|" Exp         ":"  SmplDcls Ins  END {% exitScope >> checkAllOk [ checkFor     $2   [$5,$7]    , return (fst $10), return (fst $1) ] TypeVoid TypeVoid "" 0 >>= checkOkIns (For (trd $5) (trd $7) (snd $10)) (snd $1) } 
    | Ins FOR Ent4 "=" ENUM "|" ENUM        ":"  SmplDcls Ins  END {% exitScope >> checkAllOk [ checkEnumFor $2 $3 $5 $7     , return (fst $10), return (fst $1) ] TypeVoid TypeVoid "" 0 >>= checkOkIns (For ((ExpEnum .lexeme) $5) ((ExpEnum .lexeme) $7) (snd $10)) (snd $1) }


-- List of elseif
NextIf: {- λ -}                                     {% return (TypeVoid,newIf)  } 
      | NextIf ELIF  Exp ":" Ent0 SmplDcls Ins Ent1 {% checkAllOk [(checkGuarded $2 $3 $7),(return (fst $1))] TypeVoid TypeVoid "" 0 >>= checkAndBuild (insertIf (snd $1) (Guard (trd $3) (snd $7))) }

-- Else list
Else: {- λ -}                         {% return (TypeVoid, Nothing) }
    | ELSE ":" Ent0 SmplDcls Ins Ent1 {% checkAndBuild (Just (snd $5) ) (fst $5)  } 

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
Dcls:  {- λ -}                                                 {% return [] }
    | Dcls Reference   ID         ";"                          {% insertDeclareInScope $2 $3 True False >> return $1 } -- Always global, GlobDeclare not needed
    | Dcls FWD FUNC Reference ID Ent0 "(" Parameters ")" ";"   {% insertForwardFunc (addType $8 $4) $5  >> return $1 }
    | Dcls ENUMDEC DATAID "{" EnumConsList "}"     {% insertEnum $3 >> insertLEnumCons $5 (lexeme $3)   >> return $1 }
    | Dcls Ent5 "{" FieldsList "}"                 {% checkRecursiveDec (snd $2) $4 >> insertData $2 $4 >> return $1 }
    | Dcls Ent6 "{" FieldsList Ent7 "}"            {% checkRecursiveDec (snd $2) $4 >> insertData $2 $4 >> return $1 }
    | Dcls FUNC  Ent2  ":" Ent0 SmplDcls Ins END   {% insertFunction (snd $3) (fst $3) True >> return ( ( lexeme(fst $3),(snd $7)) : $1 ) }


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

ExpList: {- λ -}        { (emptytuple            , emptyExpList ) }
       | ExpFirsts Exp  { ((fst $1) `addType` (sel1 $2), (snd $1) `addExpList` (sel3 $2) ) } 

ExpFirsts : {- λ -}           { (emptytuple            , emptyExpList ) }
          | ExpFirsts Exp "," { ((fst $1) `addType` (sel1 $2), (snd $1) `addExpList` (sel3 $2) ) } 



Exp :  -- Cambiar los NoExp por las Exp
    -- Expresiones Aritméticas.
      Exp "+"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Plus  (sel3 $1) (sel3 $3)) } --Crear funcion reciba operador y args
    | Exp "-"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Minus (sel3 $1) (sel3 $3)) }
    | Exp "^"  Exp      {% return ((sel1 $1),(sel2 $1),(Binary Power (sel3 $1) (sel3 $3)))                  }
    | Exp "*"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Multiply (sel3 $1) (sel3 $3)) }   -- Float/Int and Int
    | Exp "/"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Div (sel3 $1) (sel3 $3)) }   -- Both Float
    | Exp "//" Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary FloatDiv (sel3 $1) (sel3 $3)) }   -- last one integer
    | Exp "%"  Exp      {% checkBinary nums (sel1 $1) (sel1 $3) $2 (sel2 $1) >>= expIns (Binary Mod (sel3 $1) (sel3 $3)) }   -- Both Integer
    | "-" Exp %prec NEG {% return (TypeInt, (sel2 $2),(Unary Neg (sel3 $2)))  }
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
    | Exp "!!" Exp            {% return (TypeBool,(sel2 $1),(Binary Access     (sel3 $3) (sel3 $1))) }
    | Exp "."  ID             {% checkFieldAccess $1 $3  >>= expIns (Binary Access (sel3 $1) (ExpVar (lexeme $3))) } -- Binary Access (sel3 $1) (sel3 $3)
    | ID SquareList %prec ARR {% return (TypeBool,$1,(arrayParser (ExpVar (lexeme $1)) (snd $2))) }
    --Llamadas a funciones
    | ID "(" ExpList ")"   {% checkFunctionCall $1 (fst $3)  >>= expIns (CallVal (lexeme $1) (snd $3)) } 
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
    | ENUM      {% return (TypeEnumCons,$1,ExpEnum  (lexeme $1)) }

 

SquareList: "[" Exp "]"            { ([sel1 $2],[sel3 $2]) } -- Check it's int and acc number of nesting
          | SquareList "[" Exp "]" { (sel1 $3 : (fst $1),sel3 $3 : (snd $1)) } -- Check it's int and acc number of nesting


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
                                return $2 } 
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

trd  (_,_,a) = a
}