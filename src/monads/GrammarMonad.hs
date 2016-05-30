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
    insertEmpty,
    insertEmptyData,
    insertForwardFunc,
    insertFunction,
    insertData,
    insertEnum,
    insertEnumCons,
    insertDeclareInScope,
    checkItsDeclared,
    checkEnumAndInsert
) where

import Control.Monad.RWS.Strict
import Data.Maybe(fromJust,isNothing)
import Tokens
import TableTree
import Types
import ErrorHandle(strError)
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

-- Monad initial state: two empty scopes and one zipper for an empty scope
initialState :: ScopeNZip
initialState = ScopeNZip emptyScope emptyScope (fromScope emptyScope)

-- Make a tuple with the String Scope and a Scope tree with globals and local
-- scopes fused. Scopeception
makeTable :: ScopeNZip -> ( Scope Declare ,Scope Declare)
makeTable (ScopeNZip str gscp z) = ( str ,fuse gscp z)

-- Aliases for writing to the log
mkErr = S.singleton . Left
mkLog = S.singleton . Right
tellError = tell . mkErr
tellLog   = tell . mkLog


{- 
    Modifier functions for the state monad
-}

-- Modify zipper
onZip :: (TableZipper -> TableZipper ) -> OurMonad()
onZip fun = do zipper <- gets zipp
               state  <- get
               put state { zipp = fun zipper }


-- Modify string symbol table
onStrScope :: (SymTable -> SymTable ) -> OurMonad()
onStrScope fun = do stringTable <- gets strTbl
                    state       <- get
                    put state { strTbl = fun stringTable }


-- Modify global variables table 
onScope :: (SymTable -> SymTable) -> OurMonad ()
onScope fun = do scope <- gets scp
                 state <- get
                 put state { scp = fun scope }

-- Exit the actual scope in the Zipper
exitScope :: OurMonad ()
exitScope = onZip (fromJust.goUp)

{-
    Check, add to scope, log functions
-}

-- Check if given token is a valid function/procedure that can be called
checkIsFunc :: Token -> OurMonad()
checkIsFunc (TkId (r,c) lex) = do
    state <- get
    let treeSearch  = (lookUp (zipp state) lex)
    let scopeSearch = (getValS lex (scp state))
    if isFunc treeSearch || isFunc scopeSearch
        then tellError $ "Error:"++show r++":"++show c++" \'"++lex ++ "\' at " ++ " it's not a callable function or procedure."
        else tellLog   $ lex ++ "\' at " ++ show r++":"++show c ++ " it's a callable function or procedure."

-- checkLIter :: Token -> OurMonad()
-- checkLIter (TkId (r,c) lex) = do
--     state <- get
--     let treeSearch  = (lookUp (zipp state) lex)
--     let scopeSearch = (getValS lex (scp state))
--     if  (\x -> isLIter x && (not.isNothing) x)  treeSearch || (\x -> isLIter x && (not.isNothing) x) scopeSearch
--                 then tellError error1
--                 else if isNothing treeSearch && isNothing scopeSearch  
--                     then tellError error2
--                     else tellLog  whathappened
--   where error1 = "Error:"++ show r++":"++show c ++" Cannot assing to \'"++ lex ++ "\' because it's an iteration variable."
--         error2 = "Error:"++ show r++":"++show c ++" Cannot assing to \'"++ lex ++ "\' because it's not declared."
--         whathappened = "Variable " ++ lex ++ " at " ++ show r++":"++show c ++ " can be assing."

checkLIter :: Token -> OurMonad()
checkLIter (TkId (l,c) lexeme) = do
    state <- get 
    if isMember (zipp state) lexeme
        then if isLIter $ fromJust $ getVal (zipp state) lexeme
                then tellError error2 
                else tellLog whathappened
        else tellLog error1 -- Verificacion ya existente en niveles mas bajos. No se requiere indicar error
  where error1 = strError (l,c) "Cannot assign to" lexeme "because it's an iteration variable."
        error2 = strError (l,c) "Cannot assign to" lexeme "because it's not declared."
        whathappened = "Variable " ++ lexeme ++ " at " ++ show l++":"++show c ++ " can be assing."


checkReadable :: Token -> Bool -> OurMonad ()
checkReadable (TkId (l,c) lexeme) bit = do
     state <- get
     let word = if bit then "readable."
                       else "writeable."
     if (isReadable (lookUp (zipp state) lexeme)) || (isReadable $ getValS lexeme (scp state))
         then tellLog   $ "Variable " ++ lexeme ++ " at " ++ show l++":"++show c ++ " is "++ word
         else tellError $ strError (l,c) " variable " lexeme (" is not " ++ word )


-- Agregar por adelantado para recursion
insertEmpty :: Token -> OurMonad ()
insertEmpty tk = do
    state <- get
    if isMember ((fromScope .scp) state) (lexeme tk)
        then tell error1
        else do tell whathappened
                onScope $ insert (lexeme tk) Empty
  where error1       = mkErr  $ "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
        whathappened = mkLog  $ "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk

insertEmptyData :: Token -> Token -> OurMonad ()
insertEmptyData datatk tk = do
    state <- get
    if isMember ((fromScope .scp) state) (lexeme tk)
        then tell error1
        else do tell whathappened
                onScope $ insert (lexeme tk) (EmptyWithType typ)
  where error1       = mkErr  $ "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
        whathappened = mkLog  $ "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk
        typ  = if isStruct datatk then (TypeStruct (lexeme tk) )
                                  else (TypeUnion  (lexeme tk) )

insertForwardFunc :: TypeTuple -> Token -> OurMonad ()
insertForwardFunc typ tk = do
    state <- get
    if isMember ((fromScope .scp) state) (lexeme tk)
        then tell error1
        else do tell whathappened
                onScope $ insert (lexeme tk) (EmptyWithType (TypeFunction typ))
  where error1       = mkErr  $ "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
        whathappened = mkLog  $ "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk 

insertFunction :: TypeTuple -> Token -> OurMonad ()
insertFunction tuple ident  = do 
    state <- get
    if isMember ((fromScope .scp) state) (lexeme ident) 
        then do if ( isEmpty . fromJust ) (getValS (lexeme ident) (scp state)) 
                    then do tell whathappened
                            onScope $ insert (lexeme ident) 
                                             (Function (position ident) 
                                                       (TypeFunction tuple) 
                                                       (fromZipper (zipp state)))
                            onZip (const (fromScope emptyScope)) -- Clean zipper
                    else tell error1
        else do tell whathappened
                onScope $ insert (lexeme ident) 
                                 (Function (position ident) 
                                           (TypeFunction tuple) 
                                           (fromZipper (zipp state)))
                onZip (const (fromScope emptyScope)) -- Clean zipper
  where error1       = mkErr $ "Error:" ++ linecol ++" redefinition of " ++ lexeme ident
        whathappened = mkLog $ "Function " ++ lexeme ident ++ " added at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident 


-- Check,add, log for structs and union
insertData :: (Token,Token) -> Bool -> OurMonad ()
insertData (typ,ident) isStruct = do 
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
  where error1       = mkErr $ "Error:" ++ linecol ++" type \'" ++ lexeme typ ++ "\' already declared."
        whathappened = mkLog $ "Adding struct/union "  ++ lexeme typ ++ " at "++ linecol
        linecol      = (show.fst.position) typ ++":"++(show.snd.position) typ
        p     = position typ
        name  = lexeme typ

-- Check,add, log for enums
insertEnum :: Token -> OurMonad ()
insertEnum id_ = do
    state <- get 
    if isMember ((fromScope.scp) state) (lexeme id_) 
        then do if ( isEmpty . fromJust ) (getValS (lexeme id_) (scp state))  
                    then insertNTell state
                    else tell error1
        else insertNTell state
    onZip (const (fromScope emptyScope)) -- Clean zipper
  where error1       = mkErr $ "Error:" ++ linecol ++" lexeme \'" ++ lexeme id_ ++ "\' used in enum already declared."
        whathappened = mkLog $ "Adding enum "  ++ lexeme id_ ++ " at "    ++ linecol
        linecol      = (show.fst.position) id_ ++":"++(show.snd.position) id_
        insertNTell state = do tell whathappened
                               onScope $ insert (lexeme id_) 
                                                (Enum (position id_) 
                                                      (lexeme id_) 
                                                      (fromZipper (zipp state)))

insertEnumCons :: Int -> Token -> OurMonad(Int)
insertEnumCons ord (TkEnumCons (l,c) str) = do
    state <- get
    if isMember (zipp state) str
        then tell error1
        else do tell whathappened
                onZip $ apply $ insert str (EnumCons (l,c) str ord)
    return (succ (ord))
  where error1       = mkErr $ "Error:" ++ linecol ++ " enum constant " ++ str ++ " already declared in this scope."
        whathappened = mkLog $ "Enum "  ++ str ++ " add at "    ++ linecol
        linecol      = show l ++":"++ show c


insertDeclareInScope :: Type -> Token -> Bool -> Bool -> OurMonad ()
insertDeclareInScope  TypeVoid (TkId (l,c) lexeme ) _      _ = (tellError .concat) $ ["Error:",show l,":",show c," ",lexeme ," is VOIDtorb, but it may only be instanced as reference."]
insertDeclareInScope dcltype  (TkId (l,c) lexeme ) isGlob readonly = do 
    state <- get
    if isMember (zipp state) lexeme -- Most recent scope
      then tellError error1
      else if isGlob 
            then if isInScope (scp state) lexeme -- global scope enum
                    then tellError error2
                    else do tellLog whathappened 
                            onScope $ insert lexeme scopevar    
            else do tellLog whathappened
                    (onZip . apply) $ insert lexeme scopevar
    where error1       = generror ++ " in actual scope."
          error2       = generror ++ " in global scope."
          scopevar     = (Variable (l,c) dcltype readonly)
          generror     = "Error:" ++ show l ++":"++show c ++" redefinition of " ++ lexeme
          whathappened = "Added " ++ lexeme ++" at "++show l++":"++show c

-- Check if datatype is enum and insert as readonly
checkEnumAndInsert :: Token -> Token -> OurMonad ()
checkEnumAndInsert (TkDId (lD,cD) lexemeD) (TkId (l,c) lexeme) = do
    state <- get
    if isInScope (scp state) lexemeD  -- Check in globals for enum
        then if enumMatches (fromJust (getValS lexeme (scp state))) lexemeD -- if its enum and has same name --enumMatches (fromJust (getValS lexeme (scp state))) lexemeD
                then do tellLog whathappened
                        onScope $ insert lexeme (Variable (l,c) (TypeEnum lexemeD) True)
                else tellError  $ error2 
        else tellError error1
  where whathappened  = "Iter enum at "++show lD ++":"++show cD++" inserted in scope"
        error1    = "Error:"++ show lD ++":"++show cD ++" datatype "++lexemeD++" used but not found."
        error2    = "Error:"++ show lD ++":"++show cD ++" trying to iterate over non ENUM type"


checkItsDeclared :: Token -> OurMonad ()
checkItsDeclared (TkId (l,c) lex) = do
    state <- get
    if (not . isNothing) (lookUp (zipp state) lex) || isInScope (scp state) lex
        then (tellLog . concat) whathappened
        else (tell.mkErr .concat) error1
  where whathappened = ["Variable ",lex," at ",show l,":",show c," well used."]
        error1       = ["Error:",show l,":",show c," variable ",lex," used but not declared."]

-- checkRedeclare :: Token -> OurMonad (Token)
-- checkRedeclare (TkId (l,c) lex) = do
--     state <- get
--     if isMember (zipp state) lex then (tellError . concat) 
--                                  else (tellLog   . concat)  
--     return $ TkId (l,c) lex
--   where whathappened = ["V"]
--         error1       = ["Error:",show l,":",show c," variable ",lex," already declared in this scope."]
