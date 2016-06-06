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
   -- insertEmptyData,
    insertForwardFunc,
    insertForwardData,
    insertFunction,
    insertData,
    insertEnum,
    insertEnumCons,
    insertDeclareInScope,
    checkItsDeclared,
    checkEnumAndInsert,
    checkBinary
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
isInGlobals state tk = isMember ((fromScope .scp) state) (lexeme tk)

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
checkIsFunc (TkId (r,c) lex1) = do
    state <- get
    if isFunc (getValS lex1 (scp state)) -- Just checking in global scope,
        then tellLog   whathpnd          -- cause functions are global
        else tellError error1
  where error1   = strError (r,c) "" lex1 " it's not a callable function or procedure."
        whathpnd = lex1 ++ "\' at " ++ show r++":"++show c ++ " it's a callable function or procedure."

-- Check if variable it's an iteration varible (could it be used in assignment?)
checkLIter :: Token -> OurMonad()
checkLIter (TkId (l,c) lexeme) = do
    state <- get 
    if isMember (zipp state) lexeme
        then if isLIter $ fromJust $ getVal (zipp state) lexeme
                then tellError error2 
                else tellLog whathappened
        else tellLog error1 -- This check exists already in lower levels
  where error1 = strError (l,c) "Cannot assign to" lexeme "because it's an iteration variable."
        error2 = strError (l,c) "Cannot assign to" lexeme "because it's not declared."
        whathappened = "Variable " ++ lexeme ++ " at " ++ show l++":"++show c ++ " can be assing."

-- Dunno lol
checkReadable :: Token -> Bool -> OurMonad ()
checkReadable (TkId (l,c) lexeme) bit = do
     state <- get
     let word = if bit then "readable."
                       else "writeable."
     if (isReadable (lookUp (zipp state) lexeme)) || (isReadable $ getValS lexeme (scp state))
         then tellLog   $ "Variable " ++ lexeme ++ " at " ++ show l++":"++show c ++ " is "++ word
         else tellError $ strError (l,c) " variable " lexeme (" is not " ++ word )


-- Adding identifier as soon as possible for recursion in functions
insertEmpty :: Token -> OurMonad ()
insertEmpty tk = do
    state <- get
    if isInGlobals state tk
        then do if isEmpty $ fromJust $ getValS (lexeme tk) (scp state)
                  then tellLog "Nothing happened. Tryng to insert empty when forward declaration found"
                  else tellLog error1
        else do tellLog whathappened
                onScope $ insert (lexeme tk) Empty
  where error1       = "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
        whathappened = "Adding " ++ lexeme tk ++ " as soon as possible at " ++ linecol ++ "with type Empty" 
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk


-- Adding identifiers as soon as possible for recursion in datatypes
-- insertEmptyData :: Token -> Token -> OurMonad ()
-- insertEmptyData datatk tk = do
--     state <- get
--     if isInGlobals state tk
--         then tell error1
--         else do tell whathappened
--                 tellError $ "wtf im adding "++ show typ
--                 onScope $ insert (lexeme tk) typ
--   where error1       = mkErr  $ "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
--         whathappened = mkLog  $ "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol 
--         linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk
--         typ  = if isStruct datatk then TypeStruct (lexeme tk)
--                                   else TypeUnion  (lexeme tk)

-- Adding forward declartion to functions
insertForwardFunc :: TypeTuple -> Token -> OurMonad ()
insertForwardFunc typ tk = do
    state <- get
    if isInGlobals state tk
        then tellError error1
        else do tellLog whathappened
                onScope $ insert (lexeme tk) (EmptyWithType (TypeFunction typ))
    onZip (const (fromScope emptyScope)) -- Cleaning scope bc of parameters
  where error1       = "Error:" ++ linecol ++" redefinition of " ++ lexeme tk
        whathappened = "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk 

insertForwardData :: Token-> Token -> OurMonad ()
insertForwardData typ tk = do
    state <- get
    if isInGlobals state tk
        then if storedType (typeFound state) == storedType declare
                then do tellLog whathappened
                        onScope $ insert (lexeme tk)  declare
                else tellError error1
        else do tellLog whathappened
                onScope $ insert (lexeme tk)  declare
  where error1       = "Error:" ++ linecol ++" type of " ++ lexeme tk ++ " doesn't match."
        whathappened = "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk
        build cons cons2  = cons (position tk) (cons2 (lexeme tk)) emptytuple emptyScope
        declare = if isStruct typ then build Struct TypeStruct 
                                  else build Union  TypeUnion 
        typeFound state =  fromJust $ getValS (lexeme tk) (scp state)

-- Adding function to global scope and cleaning actual zipper
insertFunction :: TypeTuple -> Token -> OurMonad ()
insertFunction tuple ident  = do 
    state <- get
    if isInGlobals state ident
        then do if emptyTypeMatches (fromJust (getValS (lexeme ident) (scp state)))
                                    tuple 
                    then insertNclean state
                    else tellError error1
                            -- tellError $ "En " ++ lexeme ident ++ show (fromJust (getValS (lexeme ident) (scp state))) ++ " vs " ++ show tuple
        else insertNclean state
  where error1       = strError (position ident) "type of function" (lexeme ident) " doesn't match with forward declaration."
        whathappened = "Function " ++ lexeme ident ++ " added at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident 
        insertNclean state = do tellLog whathappened
                                onScope $ insert (lexeme ident) 
                                             (Function (position ident) 
                                                       (TypeFunction tuple) 
                                                       (fromZipper (zipp state)))
                                onZip (const (fromScope emptyScope)) -- Clean zipper


-- Check,add, log for structs and union
insertData :: (Token,Token) -> TypeTuple -> OurMonad ()
insertData (typ,ident) tt = do 
    state <- get
    tell whathappened
    onScope $ insert (lexeme ident) $ if isStruct typ
                                          then build Struct TypeStruct state
                                          else build Union  TypeUnion  state
    onZip (const (fromScope emptyScope))
  where whathappened = mkLog $ "Adding struct/union "  ++ lexeme ident ++ " at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident
        build cons cons2 state  = cons (position ident) 
                                       (cons2 (lexeme ident)) 
                                       tt 
                                       (fromZipper (zipp state))

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
          whathappened = "Added " ++ lexeme ++" at "++show l++":"++show c ++ " with type " ++ show dcltype

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
        error1    = strError (lD,cD) "datatype" lexemeD "used but not found."
        error2    = strError (lD,cD) "trying to iterate" lexemeD "over non ENUM type."

checkItsDeclared :: Token -> OurMonad ()
checkItsDeclared tk = do
    state <- get
    if (not . isNothing) (lookUp (zipp state) (lexeme tk)) || isInScope (scp state) (lexeme tk)
        then (tellLog . concat) whathappened
        else tellError error1
  where whathappened = ["Variable ",lexeme tk," at ",(show . position) tk," well used."]
        error1       = strError (position tk) " variable or datatype" (lexeme tk) "used but not declared."


checkBinary :: [Type] -> Type -> Type -> Token -> OurMonad (Type)
checkBinary expected TypeError _ _ = return TypeError
checkBinary expected _ TypeError _ = return TypeError
checkBinary expected l r tok = do
    if l == r 
      then do if (any (==l) expected) 
                  then return l
                  else tellError error2 >> return TypeError
      else do tellError error1 >> return TypeError
  where error1 = strError (position tok) "Types in the operator" (toStr tok) "are not equal."
        error2 = strError (position tok) "Types in" (toStr tok) "did't match any of the expected types."
              