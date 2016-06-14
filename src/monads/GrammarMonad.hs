module GrammarMonad(
    OurMonad,
    SymTable,
    TableZipper,
    ScopeNZip,
    exec,
    checkIsFunc,
    checkLValue,
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
    checkBinary,
    checkFunctionCall,
    checkRecursiveDec,
    checkFieldAccess,
    checkMain,
    tellError,
    toggleUnion,
    getDataSize
) where

import Control.Monad.RWS.Strict
import Control.Monad(foldM)
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
                           , enuTbl   :: SymTable
                           , scp      :: SymTable
                           , zipp     :: TableZipper
                           , onUnion  :: Bool } 
                           deriving (Show)

-- Alias for executing RWS monad
exec = execRWS

-- Monad initial state: two empty scopes and one zipper for an empty scope
initialState :: ScopeNZip
initialState = ScopeNZip emptyScope 
                         emptyScope 
                         builtinFunctions 
                         (fromScope emptyScope) 
                         False

-- Make a tuple with the String Scope and a Scope tree with globals and local
-- scopes fused. Scopeception
makeTable :: ScopeNZip -> ( Scope Declare , Scope Declare , Scope Declare)
makeTable (ScopeNZip str enu gscp z) = ( str , enu , fuse gscp z)

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

-- Modify enum symbol table
onEnuScope :: (SymTable -> SymTable ) -> OurMonad()
onEnuScope fun = do enumTable   <- gets enuTbl
                    state       <- get
                    put state { enuTbl = fun enumTable }

-- Modify global variables table 
onScope :: (SymTable -> SymTable) -> OurMonad ()
onScope fun = do scope <- gets scp
                 state <- get
                 put state { scp = fun scope }

-- Exit the actual scope in the Zipper
exitScope :: OurMonad ()
exitScope = onZip (fromJust.goUp)

-- Toggle union
toggleUnion :: OurMonad () -> OurMonad ()
toggleUnion = do isInUnion <- gets onUnion
                 state     <- get
                 put state { onUnion = not isInUnion }

{-
    Check, add to scope, log functions
-}


-- Check if given token is a valid function/procedure that can be called
checkIsFunc :: Token -> OurMonad(Maybe(Declare))
checkIsFunc (TkId (r,c) lex1) = do
    state <- get
    if isFunc (getValS lex1 (scp state)) --Type- Functions are in global scope
        then tellLog whathpnd
        else tellError error1
    return(getValS lex1 (scp state))
  where error1   = strError (r,c) "" lex1 " it's not a callable function or procedure."
        whathpnd = lex1 ++ "\' at " ++ show r++":"++show c ++ " it's a callable function or procedure."

{-checkRValue :: Token -> Type -> (Type,Token) -> OurMonad(Type)
checkRValue _ TypeError _                =  return TypeError
checkRValue _ _ (TypeError,_)            =  return TypeError 
checkRValue (TkAssign _) myT1 (myT2,tok) =  if myT1 == myT2 
                                              then return myT1
                                              else return TypeError
checkRValue (TkTEQ _) myT1 (myT2,tok) =  
  if (myT1 == TypeFloat || myT1 == TypeInt) && 
     (myT2 == TypeFloat || myT2 == TypeInt)
       then return myT1
checkRValue (TkPEQ _) myT1 (myT2,tok) =  if myT1 == myT2 then return myT1
checkRValue (TkMEQ _) myT1 (myT2,tok) =  if myT1 == myT2 then return myT1-}

-- Check if variable it's an iteration varible (could it be used in assignment?)
checkLValue :: (Type,Token) -> OurMonad(Type)
checkLValue (TypeError,_)               = return TypeError
checkLValue (myType ,myToken) = do
    state <- get 
    if haveLexeme myToken
     then
      if isMember (zipp state) myLex
          then if (isLIter $ fromJust $ getVal (zipp state) myLex)
                  then tellError error1       >> return TypeError
                  else 
                    if (isLValue myType $ fromJust $ getVal (zipp state) myLex)
                    then tellLog whathappened >> return myType  --REVISAR
                    else tellError error2     >> return TypeError
      else if isInScope (scp state) myLex 
          then if (isLIter $ fromJust $ getValS myLex (scp state))  
                  then tellError error1       >> return TypeError
                  else 
                    if (isLValue myType $ fromJust $ getValS myLex (scp state) )
                    then tellLog whathappened >> return myType  --REVISAR
                    else tellError error2     >> return TypeError
          else return TypeError -- This check exists already in lower levels
    else tellError error4 >> return TypeError
   where 
        (l,c)  = position myToken
        myLex  = lexeme myToken
        error1 = strError (l,c) "Cannot assign to" myLex "because it's an iteration variable."
        error2 = strError (l,c) "Cannot assign to" myLex "because it's not a valid L-Value."
        error3 = strError (l,c) "Cannot assign to" myLex "because it's not declared."
        error4 = strError (l,c) "Cannot assign to" (show (toConstr myToken)) "because it's not a valid L-Value."
        whathappened = "Variable " ++ myLex ++ " at " ++ show l++":"++show c ++ " can be assing."

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
insertFunction :: TypeTuple -> Token -> Bool -> OurMonad ()
insertFunction tuple ident clean = do 
    state <- get
    if isInGlobals state ident
        then do if emptyTypeMatches (fromJust (getValS (lexeme ident) (scp state)))
                                    tuple 
                    then insertNclean state
                    else tellError error1 -- tellError $ "En " ++ lexeme ident ++ show (fromJust (getValS (lexeme ident) (scp state))) ++ " vs " ++ (show . TypeFunction) tuple
        else insertNclean state
  where error1       = strError (position ident) "type of function" (lexeme ident) " doesn't match with forward declaration."
        whathappened = "Function " ++ lexeme ident ++ " added at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident 
        insertNclean state = do tellLog whathappened
                                onScope $ insert (lexeme ident) 
                                             (Function (position ident) 
                                                       (TypeFunction tuple) 
                                                       (fromZipper (zipp state)))
                                if clean
                                    then onZip (const (fromScope emptyScope)) -- Clean zipper
                                    else onZip enterScope


-- Check,add, log for structs and union
insertData :: (Token,Token) -> TypeTuple -> OurMonad ()
insertData (typ,ident) tt = do 
    state <- get
    tell whathappened
    onScope $ insert (lexeme ident) $ if isStruct typ
                                      then build Struct 
                                                 TypeStruct 
                                                 state 
                                                 False 
                                                 ((ofs . fromZipper . zipp) state)
                                      else build Union 
                                                 TypeUnion 
                                                 state 
                                                 True
                                                 (maxMapped 
                                                    ((fromZipper . zipp) state)
                                                    varSize)
    onZip (const (fromScope emptyScope))
  where whathappened = mkLog $ "Adding struct/union "  ++ lexeme ident ++ " at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident
        build cons cons2 state isUnion size = cons (position ident) 
                                              (cons2 (lexeme ident)) 
                                              tt 
                                              (fromZipper (zipp state))
                                              size
Monad m => (a -> b -> m a) -> a -> [b] -> m a
maxMapped  :: Scope a  -> OurMonad(Int)
maxMapped s  = foldM searchAndMax 0 (decList s)
    where searchAndMax lstMax dec = do newSize <- varSize dec
                                               if newSize >= lastMax
                                               then return newSize
                                               else return lastMax

--revisar y obtener tam
varSize :: Declare -> OurMonad(Int)
varSize = undefined

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
    if isInScope (enuTbl state) str
        then tell error1
        else do tell whathappened
                onEnuScope $ insert str (EnumCons (l,c) str ord)
    return (succ (ord))
  where error1       = mkErr $ "Error:" ++ linecol ++ " enum constant " ++ str ++ " already declared in this scope."
        whathappened = mkLog $ "Enum "  ++ str ++ " add at "    ++ linecol
        linecol      = show l ++":"++ show c


insertDeclareInScope :: Type -> Token -> Bool -> Bool -> OurMonad ()
insertDeclareInScope  TypeVoid (TkId (l,c) lexeme ) _  _ = (tellError .concat) $ ["Error:",show l,":",show c," ",lexeme ," is VOIDtorb, but it may only be instanced as reference."]
insertDeclareInScope dcltype  (TkId (l,c) lexeme ) isGlob readonly = do 
    state <- get
    if isMember (zipp state) lexeme -- Most recent scope
      then tellError error1
      else if isGlob 
            then if isInScope (scp state) lexeme -- global scope enum
                    then tellError error2
                    else do tellLog whathappened
                            inUnion <- gets onUnion
                            if inUnion
                                then onScope $ insert lexeme scopevar  
                                else onScope $ insert0 lexeme scopevar
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

checkItsDeclared :: Token -> OurMonad ((Type,Token))
checkItsDeclared tk = do
    state <- get
    if (not . isNothing) (lookUp (zipp state) (lexeme tk)) 
        then (tellLog . concat) whathappened >>
                return ((typeFound (lookUp (zipp state) (lexeme tk))),tk)
        else if isInScope (scp state) (lexeme tk)
                then return (( typeFound (getValS (lexeme tk) (scp state))),tk)
                else tellError error1 >> return (TypeError,tk)
      
  where whathappened = ["Variable ",lexeme tk," at ",(show . position) tk," well used."]
        error1       = strError (position tk) " variable or datatype" (lexeme tk) "used but not declared."
        typeFound    = storedType . fromJust



checkBinary :: [Type] -> Type -> Type -> Token -> Token -> OurMonad ((Type,Token))
checkBinary expected TypeError _ _ tok = return (TypeError,tok)
checkBinary expected _ TypeError _ tok = return (TypeError,tok)
checkBinary expected l r tok tok2      = do
    if l == r 
      then do if (any (==l) expected) 
                  then return (l,tok2)
                  else tellError error2 >> return (TypeError,tok2)
      else do tellError error1 >> return (TypeError,tok2)
  where error1 = strError (position tok) "Types in the operator" (toStr tok) ("are not equal (" ++ show l ++ " and " ++ show r ++ ")")
        error2 = strError (position tok) "Operands in" (toStr tok) ("have type" ++ show l ++ " but did't match any of the expected types." ++ show expected)

checkFunctionCall :: Token -> TypeTuple -> OurMonad((Type,Token))
checkFunctionCall ident calltup = do
    res <- checkIsFunc ident
    if isNothing res 
        then return (TypeError,ident)-- Nothing to do, error
        else do let funcSig = (getTuple . storedType . fromJust) res
                if lengthMatches calltup funcSig 
                then if trd $ tuplesMatch calltup funcSig
                     then do tellLog "Function call types work"
                             return (funcReturnType funcSig,ident)
                     else do tellError . error1 $ tuplesMatch calltup funcSig
                             return (TypeError,ident)
                else tellError error2 >> return (TypeError,ident)
  where trd (_,_,a) = a
        error1 (expected,p,_) = strError (position ident) "Error in the call of" (lexeme ident) ("argument number "++show p++" didn't match with expected " ++ show expected)
        error2  = strError (position ident) "number of arguments don't match with" (lexeme ident) "declaration."

checkFieldAccess :: (Type,Token) -> Token -> OurMonad((Type,Token))
checkFieldAccess (TypeError,tk1) _ = return (TypeError,tk1)
checkFieldAccess (ty1,tk1) tk2 = do
    state <- get
    if structured ty1 
    then do let strScope = (fields . fromJust) $ getValS (getDataName ty1) 
                                                         (scp state) 
            if isInScope strScope (lexeme tk2)
            then return(((storedType . fromJust) (getValS (lexeme tk2) strScope)),tk2)
            else tellError (error2 (getDataName ty1)) >> return (TypeError,tk1)
    else tellError error1  >> return (TypeError,tk1)
  where error1 = strError (position tk1) "Variable" (lexeme tk1) "it's not a valid struct/union, field cannot be accessed"
        error2 dn = strError (position tk1) "Variable" (lexeme tk2) ("not found in struct/union " ++ show dn )
        l      = lexeme tk1

checkMain :: OurMonad()
checkMain = do
    globals <- gets scp
    if isInScope globals "hitMAINlee"
        then return ()
        else tellError $ strError (0,0) "" "hitMAINlee" "function not found"

checkRecursiveDec :: Token -> TypeTuple -> OurMonad()
checkRecursiveDec dataTok typeSec = do 
    if isNotRecursiveData (lexeme dataTok) typeSec
        then return ()
        else tellError error1
  where error1 =  strError (position dataTok) "Data type" (lexeme dataTok) "cannot be recursive. (Pssss try to use a pointer)"

builtinFunctions :: SymTable
builtinFunctions = foldl insertFunc emptyScope declarations
  where insertFunc scp (str,dec) = insert str dec scp
        printable t    = or $ map ($t) [(==TypeString),isPointer,isBasic,(==TypeEnumCons)]          
        makeFunc types = (Function (0,0) (makeTypeTuple types) emptyScope)
        declarations = [
          ("liberar"       , makeFunc [TypeSatisfies isPointer, TypeVoid] ),
          ("vamo_a_imprimi", makeFunc [TypeSatisfies printable, TypeVoid] ),
          ("atrapar"       , makeFunc [TypeInt      , TypeVoid  ] ),
          ("intToFLoat"    , makeFunc [TypeInt      , TypeFloat ] ),
          ("floor"         , makeFunc [TypeFloat    , TypeInt   ] ),
          ("celing"        , makeFunc [TypeFloat    , TypeInt   ] ),
          ("succ"          , makeFunc [TypeEnumCons , TypeInt   ] ),
          ("pred"          , makeFunc [TypeEnumCons , TypeInt   ] ),
          ("pidGET"        , makeFunc [TypeEnumCons , TypeInt   ] )
          ]
-- ("SIZEther",       (Function (0,0) (makeTypeTuple [TypeSatisfies isBasic, TypeInt]) emptyScope)),
