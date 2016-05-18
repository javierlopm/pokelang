{
module Grammar(parser,ScopeNZip(..),makeTable,initialState) where
import Tokens
import TableTree
import Types
import Data.Maybe(fromJust,isNothing)
import Control.Monad.RWS.Strict
import qualified Data.Sequence as S

}


%name parser
%tokentype { Token }

%monad { RWS String (S.Seq(Message)) ScopeNZip }

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
    | Ins PRINT "(" STRING PrntArgs ")"   ";" {% onScope $ insert ('_':(content $4)) (Cons (position $4)) }
    | Ins READ  "("       ID        ")"   ";" {% checkReadable $4 True}
    | Ins WRITE "("       ID        ")"   ";" {% checkReadable $4 False}
    | Ins Exp "=" Exp         ";"         {% checkLIter $2}
    | Ins Exp "*=" Exp        ";"         {% checkLIter $2 }
    | Ins Exp "+=" Exp        ";"         {% checkLIter $2}
    | Ins BREAK             ";"         {% return ()} --√
    | Ins CONTINUE          ";"         {% return ()} --√
    | Ins RETURN   Exp      ";"         {% return ()} --√
    | Ins RETURN            ";"         {% return ()} --En los void?  --√
    | Ins EXIT              ";"         {% return ()} --√ 
    | Ins FREE "("ID")"     ";"         {% return ()} --√
    --| Ins FREE "("DATAID")" ";"         {% return ()}
    --| Ins READ "("DATAID")" ";"         {% return ()}
    | Ins IF Exp    ":" Ent SmplDcls Ins NextIf Else END            {% exitScope  }
    | Ins WHILE Exp ":" Ent SmplDcls Ins END                        {% exitScope  }
    | Ins FOR Ent Ent3 "=" Exp  "|" Exp "|" Exp ":"  SmplDcls Ins  END {% exitScope  }
    | Ins FOR Ent Ent3 "=" Exp  "|" Exp         ":"  SmplDcls Ins  END {% exitScope  }
    | Ins FOR Ent Ent3 "=" ENUM "|" ENUM        ":"  SmplDcls Ins  END {% exitScope  }
    | Ins BEGIN Ent SmplDcls Ins END                                {% exitScope  } -- No debe aceptar funciones

PrntArgs: {- λ -}             {% return ()}
        | PrntArgs "," Exp    {% return ()} -- Siempre es necesaria una coma a la izq

NextIf: {- λ -}             {% return ()}
      | NextIf ELIF  Exp ":" Ent SmplDcls Ins {% exitScope  }

Else: {- λ -}                   {% return ()}
    | ELSE ":" Ent SmplDcls Ins {% exitScope  }


SmplDcls: {- λ -}                                     {% return () }        
    | SmplDcls IsGlob PrimType Ptrs          ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $5) $4 Nothing) $5  $2}
    | SmplDcls IsGlob DataType DATAID  Ptrs  ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $6) $5 (Just (lexeme $4))) $6  $2}
    | SmplDcls IsGlob PrimType EmptyArrs     ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $5) $4 Nothing) $5  $2} -- Azucar sintactico? jeje
    | SmplDcls IsGlob PrimType StaticArrs    ID  ";"  {% insertDeclareInScope (makeArr  $3 (position $5) $4) $5  $2}
    | SmplDcls IsGlob PrimType               ID  ";"  {% insertDeclareInScope (makeDec  $3 (position $4) Nothing) $4  $2}
    | SmplDcls IsGlob DataType DATAID        ID  ";"  {% insertDeclareInScope (makeDec  $3 (position $5) (Just (lexeme $4))) $5  $2}

Dcls:  {- λ -}                                       {% return ()}
    | Dcls FUNC PrimType Ent2 "(" Parameters ")" ":" SmplDcls Ins END {% insertFunction $3 $4 }
    | Dcls PrimType Ptrs       ID  ";"            {% return ()}
    | Dcls PrimType EmptyArrs  ID  ";"            {% return ()}
    | Dcls PrimType StaticArrs ID  ";"            {% return ()}
    | Dcls PrimType            ID  ";"            {% return ()}
    | Dcls FWD DataType    DATAID     ";"         {% return ()}    -- Forward declarations solo, agregar con Dec Empty
    | Dcls ENUMDEC    Ent1 "{" EnumConsList "}"   {% insertEnum $3 }
    | Dcls STRUCTDEC  Ent1 "{" FieldsList   "}"   {% insertData $3  True }
    | Dcls UNIONDEC   Ent1 "{" FieldsList   "}"   {% insertData $3  False }

IsGlob : {- λ -}     { False }
         | GLOBAL    { True  }

PrimType : INTDEC         { $1 }
         | BOOLDEC        { $1 }
         | CHARDEC        { $1 }
         | VOIDDEC        { $1 }
         | FLOATDEC       { $1 }

DataType : ENUMDEC        { $1 }
         | STRUCTDEC      { $1 }
         | UNIONDEC       { $1 }

Parameter: ListParam PrimType             ID   {% insertDeclareInScope   (makeDec  $2 (position $3) Nothing) $3 False }
         | ListParam DataType DATAID      ID   {% insertDeclareInScope   (makeDec  $2 (position $3) (Just (lexeme $3))) $4 False }
         | ListParam DataType DATAID Ptrs ID   {% insertDeclareInScope (makePtrs $2 (position $5) $4 (Just (lexeme $3))) $5 False}
         | ListParam PrimType Ptrs        ID   {% insertDeclareInScope   (makePtrs $2 (position $4) $3 Nothing) $4  False }
         | ListParam PrimType EmptyArrs   ID   {% insertDeclareInScope   (makePtrs $2 (position $4) $3 Nothing) $4  False }


ListParam: {- λ -}                                {% return () }
         | ListParam PrimType               ID ","  {% insertDeclareInScope   (makeDec  $2 (position $3) Nothing) $3 False }
         | ListParam DataType DATAID  Ptrs  ID ","  {% insertDeclareInScope (makePtrs $2 (position $5) $4 (Just (lexeme $3))) $5 False}
         | ListParam DataType DATAID        ID ","  {% insertDeclareInScope   (makeDec  $2 (position $3) (Just (lexeme $3))) $4 False }
         | ListParam PrimType Ptrs          ID ","  {% insertDeclareInScope   (makePtrs $2 (position $4) $3 Nothing) $4  False }
         | ListParam PrimType EmptyArrs     ID ","  {% insertDeclareInScope   (makePtrs $2 (position $4) $3 Nothing) $4  False }

Parameters: {- λ -}       {% onZip enterScope } -- Tiene sentido?
          | Parameter     {% onZip enterScope } 

EnumConsList: ENUM                      {% insertEnumCons 1  $1 }
            | EnumConsList "," ENUM     {% insertEnumCons $1 $3 }

FieldsList  : ID "::" PrimType            {% insertDeclareInScope   (makeDec  $3 (position $1)   Nothing) $1 False}
            | ID "::" Ptrs PrimType       {% insertDeclareInScope   (makePtrs $4 (position $1) $3 Nothing) $1  False }
            | ID "::" DataType DATAID     {% insertDeclareInScope   (makeDec  $3 (position $1) (Just (lexeme $4))) $1 False} --verificar que realmente existe
            | ID "::" Ptrs DataType DATAID             {% insertDeclareInScope   (makePtrs $4 (position $1) $3 (Just (lexeme $5))) $1  False } --verificar que realmente existe
            | FieldsList  "," ID "::" PrimType         {% insertDeclareInScope   (makeDec  $5 (position $3) Nothing) $3 False    }
            | FieldsList  "," ID "::" Ptrs PrimType    {% insertDeclareInScope   (makePtrs $6 (position $2) $5 Nothing) $3  False}
            | FieldsList  "," ID "::" DataType DATAID  {% insertDeclareInScope   (makeDec  $5 (position $3) (Just (lexeme $6))) $3 False} --verificar que realmente existe
            | FieldsList  "," ID "::" Ptrs DataType DATAID  {% insertDeclareInScope (makePtrs $6 (position $3) $5 (Just (lexeme $7))) $3  False } --verificar que realmente existe

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

Ent  : {- λ -}     {% onZip enterScope  }
Ent1 : DATAID      {% insertCheckFunc $1 >> return $1 } 
Ent2 : ID          {% insertCheckFunc $1 >> return $1 } 
Ent3 : ID          {% insertDeclareInScope (makeIter $1 (position $1)) $1 False } 

{

type OurMonad    = RWS String (S.Seq(Message)) ScopeNZip
type SymTable    = Scope Declare  
type TableZipper = Zipper Declare 

-- state from monad State
data ScopeNZip = ScopeNZip { scp      :: SymTable
                           , zipp     :: TableZipper} 
                           deriving (Show)
checkIsFunc :: Token -> OurMonad()
checkIsFunc (TkId (r,c) lex) = do
                    state <- get
                    let treeSearch = (lookUp (zipp state) lex)
                    let scopeSearch = (getValS lex (scp state))
                    if isFunc treeSearch || isFunc scopeSearch
                        then tell $ S.singleton $ Left  $ lex ++ "\' at " ++ show r++":"++show c ++ " it's a callable function or procedure."
                        else tell $ S.singleton $ Right $ "Error: "++lex ++ "\' at " ++ show r++":"++show c ++ " it's not a function or procedure."

checkLIter :: Token -> OurMonad()
checkLIter (TkId (r,c) lex) = do
                    state <- get
                    let treeSearch = (lookUp (zipp state) lex)
                    let scopeSearch = (getValS lex (scp state))
                    if  (\x -> isLIter x && (not.isNothing) x)  treeSearch || (\x -> isLIter x && (not.isNothing) x) scopeSearch
                                then tell $ S.singleton $ Left  $ "Cannot assing to \'" ++ lex ++ "\' at " ++ show r++":"++show c ++ " because it's an iteration Variable."
                                else if isNothing treeSearch && isNothing scopeSearch  
                                    then tell $ S.singleton $ Left  $ "Cannot assing to \'" ++ lex ++ "\' at " ++ show r++":"++show c ++ " because it's not declared."
                                    else tell $ S.singleton $ Right  $ "Variable " ++ lex ++ " at " ++ show r++":"++show c ++ " can be assing."

checkReadable :: Token -> Bool -> OurMonad ()
checkReadable (TkId (l,c) lex) bit = do
    state <- get
    let word = if bit then "readable."
                      else "writeable."
    if isReadable (lookUp (zipp state) lex) || isReadable (getValS lex (scp state) )
        then tell $ S.singleton $ Right  $ "Variable " ++ lex ++ " at " ++ show l++":"++show c ++ " is "++word
        else tell $ S.singleton $ Left   $ concat $ ["Error:",show l,":",show c," variable ",lex," is not "++word++"ESTOY EN:\n","================",(show . fst) (zipp state),"==================","\n"]
  
initialState :: ScopeNZip
initialState = ScopeNZip emptyScope (fromScope emptyScope)

makeTable :: ScopeNZip -> Scope Declare
makeTable (ScopeNZip s z) = fuse s z

onZip :: (TableZipper -> TableZipper ) -> OurMonad()
onZip fun = do zipper <- gets zipp
               state  <- get
               put state { zipp = fun zipper }


onScope :: (SymTable -> SymTable) -> OurMonad ()
onScope fun = do scope <- gets scp
                 state <- get
                 put state { scp = fun scope }

{-succCons ::  OurMonad ()
succCons = do conG  <- gets constGen
              state <- get
              put state { constGen = succ conG }-}

exitScope :: OurMonad ()
exitScope = onZip (fromJust.goUp)

-- Agregar por adelantado para recursion
insertCheckFunc :: Token -> OurMonad ()
insertCheckFunc tk = do
    state <- get
    if isMember ((fromScope.scp) state) (lexeme tk)
        then tell error1
        else do tell whathappened
                onScope $ insert (lexeme tk) Empty
  where error1       = S.singleton $ Left  $ "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
        whathappened = S.singleton $ Right $ "Agregando " ++ lexeme tk ++ " por adelantado en "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk 

insertFunction :: Token -> Token -> OurMonad ()
insertFunction typ ident  = do 
    state <- get
    if isMember ((fromScope.scp) state) (lexeme ident) 
        then do if ( isEmpty . fromJust ) (getValS (lexeme ident) (scp state)) 
                    then do tell whathappened
                            onScope $ insert (lexeme ident) 
                                             (Function (position ident) 
                                                       (makeType typ) 
                                                       (fromZipper (zipp state)))
                            onZip (const (fromScope emptyScope)) -- Clean zipper
                    else tell error1
        else do tell whathappened
                onScope $ insert (lexeme ident) 
                                 (Function (position ident) 
                                           (makeType typ) 
                                           (fromZipper (zipp state)))
                onZip (const (fromScope emptyScope)) -- Clean zipper
  where error1       = S.singleton $ Left  $ "Error:" ++ linecol ++" redefinition of " ++ lexeme ident
        whathappened = S.singleton $ Right $ "Function " ++ lexeme ident ++ " added at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident 


insertData :: Token -> Bool -> OurMonad ()
insertData typ isStruct = do 
    state <- get
    if isMember ((fromScope.scp) state) (lexeme typ) -- Chequear que es vaina forward
        then do if ( isEmpty . fromJust ) (getValS (lexeme typ) (scp state))  
                    then do tell whathappened
                            onScope $ insert (lexeme typ)
                                              (if isStruct 
                                                 then Struct p name (fromZipper (zipp state))
                                                 else Union  p name (fromZipper (zipp state)))
                            onZip (const (fromScope emptyScope)) -- Clean zipper
                    else tell error1
        else do tell whathappened
                onScope $ insert (lexeme typ)
                                  (if isStruct 
                                     then Struct p name (fromZipper (zipp state))
                                     else Union  p name (fromZipper (zipp state)))
                onZip (const (fromScope emptyScope)) -- Clean zipper
  where error1       = S.singleton $ Left  $ "Error:" ++ linecol ++" redeclaraci'o del tipo " ++ lexeme typ
        whathappened = S.singleton $ Right $ "Agregada la estructura/union "  ++ lexeme typ ++ " en "++ linecol
        linecol      = (show.fst.position) typ ++":"++(show.snd.position) typ
        p     = position typ
        name  = lexeme typ

insertEnum :: Token -> OurMonad ()
insertEnum id = do
    state <- get 
    if isMember ((fromScope.scp) state) (lexeme id) 
        then do if ( isEmpty . fromJust ) (getValS (lexeme id) (scp state))  
                    then do tell whathappened
                            onScope $ insert (lexeme id)
                                    (Enum (position id) (fromZipper (zipp state)))
                    else tell error1
        else do tell whathappened
                onScope $ insert (lexeme id)
                                 (Enum (position id) (fromZipper (zipp state)))
    onZip (const (fromScope emptyScope)) -- Clean zipper
  where error1       = S.singleton $ Left $ "Error:" ++ linecol ++" redeclaraci'o del enum" ++ lexeme id
        whathappened = S.singleton $ Right $ "Agregado el enum "  ++ lexeme id ++ " en "    ++ linecol
        linecol      = (show.fst.position) id ++":"++(show.snd.position) id

insertEnumCons :: Int -> Token -> OurMonad(Int)
insertEnumCons ord (TkEnumCons (l,c) str) = do
    state <- get
    if isMember (zipp state) str
        then tell error1
        else do tell whathappened
                onZip $ apply $ insert str (EnumCons (l,c) str ord)
    return (succ (ord))
  where error1       = S.singleton $ Left $ "Error:" ++ linecol ++" constante de enum " ++ str ++ " ya declarada."
        whathappened = S.singleton $ Right $ "Agregada constante de enum "  ++ str ++ " en "    ++ linecol
        linecol      = show l ++":"++ show c


--se deberia verificar que el valor no esta ya en alguna constante
{-addStr declare = do id  <- gets constGen
                    succCons
                    onScope $ insert (show id) declare -}

--insertIter :: Maybe Declare -> Token -> OurMonad(Token)
--insertIter Nothing (TkId (l,c) lexeme ) = do tell $ S.singleton $ Left  $ "Token " ++ lex ++ " at " ++ show l++":"++show c ++ " is not var." 
--                                             return $ TkId (l,c) lex
--insertIter Nothing (TkId (l,c) lexeme ) = do tell $ S.singleton $ Left  $ "Token " ++ lex ++ " at " ++ show l++":"++show c ++ " is not var." 
--                                             return $ TkId (l,c) lex

insertDeclareInScope :: Maybe Declare -> Token -> Bool -> OurMonad ()
insertDeclareInScope Nothing (TkId (l,c) lexeme ) _ =
    tell $ S.singleton $ Left  $ 
    "Error:" ++show l++":"++show c ++" "  ++ lexeme ++ 
    " is VOIDtorb, but it may only be instanced as reference."
insertDeclareInScope (Just dcltype) (TkId (l,c) lexeme ) isGlob = do 
    table <- get 
    if isMember (zipp table) lexeme
        then tell error1
        else if isGlob 
                then onScope $ insert lexeme dcltype 
                else onZip $ apply  $ insert lexeme dcltype       
    tell whathappened
    where error1       = S.singleton $ Left  $ "Error:" ++show l++":"++show c ++" redefinition of " ++ lexeme
          whathappened = S.singleton $ Right $ "Added " ++ lexeme ++ " at "++show l++":"++show c


checkItsDeclared :: Token -> OurMonad (Token)
checkItsDeclared (TkId (l,c) lex) = do
    state <- get
    if (not . isNothing) (lookUp (zipp state) lex) || isInScope (scp state) lex
        then tell $ S.singleton $ Right  $ "Variable " ++ lex ++ " at " ++ show l++":"++show c ++ " well used."
        else tell $ S.singleton $ Left   $ concat $ ["Error:",show l,":",show c," variable ",lex," used but not declared.ESTOY EN:\n","================",(show . fst) (zipp state),"==================","\n"]
    return $ TkId (l,c) lex
  

parseError [] = error $ "EOF unexpected"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}