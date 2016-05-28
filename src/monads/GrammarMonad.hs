module GrammarMonad(
    OurMonad,
    SymTable,
    TableZipper,
    ScopeNZip,
    exec,
    checkIsFunc,
    checkLIter,
    checkReadable,
    initialState,
    makeTable,
    onZip,
    onScope,
    onStrScope,
    exitScope,
    insertCheckFunc,
    insertFunction,
    insertData,
    insertEnum,
    insertEnumCons,
    insertDeclareInScope,
    checkItsDeclared,
) where

import Control.Monad.RWS.Strict
import Data.Maybe(fromJust,isNothing)
import Tokens
import TableTree
import Types
import qualified Data.Sequence as S

type OurMonad    = RWS String (S.Seq(Message)) ScopeNZip
type SymTable    = Scope Declare  
type TableZipper = Zipper Declare 

-- state from monad State
data ScopeNZip = ScopeNZip { strTbl   :: SymTable
                           , scp      :: SymTable
                           , zipp     :: TableZipper} 
                           deriving (Show)

-- Alias for executing RWS monad
exec = execRWS

-- Check if given token is a valid function/procedure that can be called
checkIsFunc :: Token -> OurMonad()
checkIsFunc (TkId (r,c) lex) = do
                    state <- get
                    let treeSearch  = (lookUp (zipp state) lex)
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

-- Monad initial state: two empty scopes and one zipper for an empty scope
initialState :: ScopeNZip
initialState = ScopeNZip emptyScope emptyScope (fromScope emptyScope)

-- Make a tuple with the String Scope and a Scope tree with globals and local
-- scopes fused. Scopeception
makeTable :: ScopeNZip -> ( Scope Declare ,Scope Declare)
makeTable (ScopeNZip str gscp z) = ( str ,fuse gscp z)

-- Modify function for the zipper in the state
onZip :: (TableZipper -> TableZipper ) -> OurMonad()
onZip fun = do zipper <- gets zipp
               state  <- get
               put state { zipp = fun zipper }


-- Modify function for the string symbol table in the state
onStrScope :: (SymTable -> SymTable ) -> OurMonad()
onStrScope fun = do stringTable <- gets strTbl
                    state       <- get
                    put state { strTbl = fun stringTable }


-- Modify function for the global variables table in the state monad
onScope :: (SymTable -> SymTable) -> OurMonad ()
onScope fun = do scope <- gets scp
                 state <- get
                 put state { scp = fun scope }


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