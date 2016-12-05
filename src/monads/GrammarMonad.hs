module GrammarMonad(module GrammarMonad) where

import Debug.Trace(trace)

import Control.Monad.RWS.Strict
import Control.Monad(foldM,sequence)
import Data.Maybe(fromJust,isNothing,isJust)
import Tokens
import TableTree
import Types
import ErrorHandle(strError)
import qualified Data.Sequence as S
import Instructions


type OurMonad    = RWS String (S.Seq(Message)) ScopeNZip
type SymTable    = Scope Declare  
type TableZipper = Zipper Declare 

-- state from monad State
data ScopeNZip = ScopeNZip { strTbl    :: [Declare]
                           , enuTbl    :: SymTable
                           , scp       :: SymTable
                           , zipp      :: TableZipper
                           , onUnion   :: Bool
                           , str_count :: Int
                           , isRef     :: Bool } 
                           deriving (Show)

-- Alias for executing RWS monad
exec = execRWS

-- Alias for execute and save result
run = runRWS

-- Monad initial state: two empty scopes and one zipper for an empty scope
initialState :: ScopeNZip
initialState = ScopeNZip [] 
                         emptyScope 
                         builtinFunctions 
                         (fromScope emptyScope) 
                         False
                         0
                         False

-- Make a tuple with the String Scope and a Scope tree with globals and local
-- scopes fused. Scopeception
makeTable :: ScopeNZip -> ( [Declare] , Scope Declare , Scope Declare)
makeTable (ScopeNZip str enu gscp z _ _ _ ) = ( str , enu , fuse gscp z )


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
onStrScope :: ([Declare] -> [Declare] ) -> OurMonad()
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

setRef :: OurMonad()
setRef = do state <- get
            put state { isRef = True }
unSetRef :: OurMonad()
unSetRef = do state <- get
              put state { isRef = False }

-- Exit the actual scope in the Zipper
exitScope :: OurMonad ()
exitScope = onZip (fromJust.goUp)

-- Toggle union
toggleUnion :: OurMonad () 
toggleUnion = do isInUnion <- gets onUnion
                 state     <- get
                 put state { onUnion = (not isInUnion) }

new_str_lex :: String -> OurMonad((Direction,Bool))
new_str_lex input = do
    all_strings <- gets strTbl
    let search_res = foldl find Nothing all_strings
    maybe (new_int)
          (\ dir_found -> return (dir_found,False))
          search_res
    
  where find Nothing (StrCons _ dir val) = if val == input then (Just dir) else Nothing
        find (Just x) _ =  (Just x)
        new_int = do new_int <- gets str_count
                     state   <- get
                     put state { str_count =  new_int + 1 }
                     return ((ThisLab ("str_" ++ show new_int)),True)

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
checkLValue (_,(TkEnumCons p s))     = do tellError error1 >> return TypeError
                              where
                                  error1 = strError p "Cannot assign to" s "because ENUMantye constants are not a valid L-Value."
checkLValue (TypeError,_)     = return TypeError
checkLValue (myType ,myToken) = do
    state <- get 
    if haveLexeme myToken
     then
      if isMember (zipp state) myLex
          then if (isLIter $ fromJust $ getVal (zipp state) myLex)
                  then tellError error1       >> return TypeError
                  else 
                    if (isLValue myType $ fromJust $ getVal (zipp state) myLex)
                    then tellLog whathappened >> return TypeVoid  --REVISAR
                    else tellError error2     >> return TypeError
      else if isInScope (scp state) myLex 
          then if (isLIter $ fromJust $ getValS myLex (scp state))  
                  then tellError error1       >> return TypeError
                  else 
                    if (isLValue myType $ fromJust $ getValS myLex (scp state) )
                    then tellLog whathappened >> return TypeVoid  --REVISAR
                    else tellError error2     >> return TypeError
          else return TypeVoid 
    else tellError error4 >> return TypeError
   where 
        (l,c)  = position myToken
        myLex  = lexeme myToken
        error1 = strError (l,c) "Cannot assign to" myLex "because it's an iteration variable."
        error2 = strError (l,c) "Cannot assign to" myLex "because it's not a valid L-Value."
        error3 = strError (l,c) "Cannot assign to" myLex "because it's not declared."
        error4 = strError (l,c) "Cannot assign to" (show (toConstr myToken)) "because it's not a valid L-Value."
        whathappened = "Variable " ++ myLex ++ " at " ++ show l++":"++show c ++ " can be assing."

recentVar :: String -> OurMonad(Exp)
recentVar myLex = do state <- get
                     let new_dec = fromJust $ lookUp (zipp state) myLex
                     return (ExpVar new_dec myLex)
            

-- Dunno lol
checkReadable :: Token -> Bool -> OurMonad (Type)
checkReadable (TkId (l,c) lexeme) bit = do
     state <- get
     let word = if bit then "readable."
                       else "writeable."
     if (isReadable (lookUp (zipp state) lexeme)) || (isReadable $ getValS lexeme (scp state))
         then tellLog   (logm word)    >> return TypeVoid
         else tellError (error1 word)  >> return TypeError
  where logm   word = "Variable " ++ lexeme ++ " at " ++ show l++":"++show c ++ " is "++ word
        error1 word = strError (l,c) " variable " lexeme (" is not " ++ word )


-- Adding identifier as soon as possible for recursion in functions
insertEmpty :: Token -> OurMonad ()
insertEmpty tk = do
    state <- get
    if isInGlobals state tk
        then do if isEmpty $ fromJust $ getValS (lexeme tk) (scp state)
                  then tellLog "Nothing happened. Tryng to insert empty when forward declaration found"
                  else tellLog error1
        else do tellLog whathappened
                onScope $ insert (lexeme tk) Empty 0 --Se debe CAMBIAR
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
                onScope $ insert (lexeme tk) (EmptyWithType (TypeFunction typ)) 0 --Se debe CAMBIAR
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
                        onScope $ insert (lexeme tk)  declare 0 --Se debe CAMBIAR
                else tellError error1
        else do tellLog whathappened
                onScope $ insert (lexeme tk)  declare 0 --Se debe CAMBIAR
  where error1       = "Error:" ++ linecol ++" type of " ++ lexeme tk ++ " doesn't match."
        whathappened = "Adding " ++ lexeme tk ++ " as soon as possible "++ linecol
        linecol      = (show.fst.position) tk ++":"++(show.snd.position) tk
        build cons cons2  = cons (position tk) (cons2 (lexeme tk)) emptytuple emptyScope
        declare = if isStruct typ 
                     then build Struct TypeStruct 
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
        isProc       = if (funcReturnType tuple) == TypeVoid then True
                       else False
        insertNclean state = do tellLog whathappened
                                onScope $ insert (lexeme ident) 
                                             (Function (position ident) 
                                                       (TypeFunction tuple) 
                                                       ((fromZipper) (zipp state)) isProc) 0 --Se debe CAMBIAR
                                if clean
                                    then onZip (const (fromScope emptyScope)) -- Clean zipper
                                    else return ()

cleanParams :: OurMonad()
cleanParams = do
    state <- get
    let newScope = (cleanOffset . fst . zipp) state
    put (state { zipp = fromScope newScope})

-- Check,add, log for structs and union
insertData :: (Token,Token) -> TypeTuple -> OurMonad ()
insertData (typ,ident) tt = do 
    state <- get
    tell whathappened
    let (_,padd) = ((align . getOfs . fromZipper . zipp) state)
    let newscp  = addSOffset ((fromZipper . zipp) state) padd
    let newData = if isStruct typ
                  then build Struct TypeStruct newscp False 
                  else build Union  TypeUnion  newscp True  
    onScope $ insert (lexeme ident) newData 0 --Se debe CAMBIAR
    onZip (const (fromScope emptyScope))
  where whathappened = mkLog $ "Adding struct/union "  ++ lexeme ident ++ " at "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident
        build cons cons2 scp isUnion   = cons (position ident) 
                                              (cons2 (lexeme ident)) 
                                              tt
                                              scp
                                              
                                              


--revisar y obtener tam
varSize :: Type -> OurMonad(Int)
varSize (TypeStruct s) = do 
    global <- gets scp
    return $ (getOfs . fields . fromJust) $ getValS s global
varSize (TypeUnion  s) = do 
    global <- gets scp
    return $ (getOfs . fields . fromJust) $ getValS s global
varSize TypeInt          = return 4  -- Basic types are going to change
varSize TypeBool         = return 1
varSize TypeChar         = return 1
varSize TypeFloat        = return 4
varSize (TypeEnum _ )    = return 4
varSize (TypePointer  _) = return 4
 
varSize (TypeArray  t d) = do s <- varSize t
                              return( d * s)


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
                                                      (fromZipper (zipp state))) 0 --Se debe CAMBIAR

insertLEnumCons :: [(Int,Token)] -> String -> OurMonad()
insertLEnumCons [] _ = return()
insertLEnumCons [(ord,tok)] sdt  = insertEnumCons ord tok sdt >> return ()
insertLEnumCons ((ord,tok):sl) sdt = do
  aux <- insertEnumCons ord tok sdt
  insertLEnumCons ((aux,snd(head sl)):tail sl) sdt

insertEnumCons :: Int -> Token -> String -> OurMonad(Int)
insertEnumCons ord (TkEnumCons (l,c) str) sdt = do
    state <- get
    if isInScope (enuTbl state) str
        then tell error1
        else do tell whathappened
                onEnuScope $ insert str (EnumCons (l,c) sdt str ord) 0
    return (succ (ord))
  where error1       = mkErr $ "Error:" ++ linecol ++ " enum constant " ++ str ++ " of datatype"++sdt++", was already declared in this scope."
        whathappened = mkLog $ "Enum "  ++ str ++ " add at "    ++ linecol
        linecol      = show l ++":"++ show c



insertDeclareInScope :: Type -> Token -> Bool -> Bool -> OurMonad ()
insertDeclareInScope  TypeVoid (TkId (l,c) lexeme ) _  _ = (tellError .concat) $ ["Error:",show l,":",show c," ",lexeme ," is VOIDtorb, but it may only be instanced as reference."]
insertDeclareInScope dcltype   (TkId (l,c) lexeme ) isGlob readonly = do 
    state <- get
    if isMember (zipp state) lexeme -- Most recent scope
    then tellError error1
    else if isGlob 
         then if isInScope (scp state) lexeme -- global scope enum
              then tellError error2
              else do 
                   tellLog whathappened
                   onScope  $ insert lexeme (scopevar Label) 0 --Se debe CAMBIAR
         else do 
              tellLog whathappened
              inUnion <- gets onUnion
              let (newofs,padd) = (align . getOfs . fst) $ zipp state
              sz <- varSize dcltype
              --tellError $ "Puse padding de " ++ show (newofs,padd) ++ " en " ++ lexeme
              if inUnion
              then (onZip . apply) $ insert0 lexeme (scopevar (Offset 0))        sz 
              else (onZip . apply) $ insert  lexeme (scopevar (Offset (-newofs)))  (padd+sz) 

    where error1       = generror ++ " in actual scope."
          error2       = generror ++ " in global scope."
          scopevar  d  = (Variable (l,c) dcltype readonly d)
          generror     = "Error:" ++ show l ++":"++show c ++" redefinition of " ++ lexeme
          whathappened = "Added " ++ lexeme ++" at "++show l++":"++show c ++ " with type " ++ show dcltype


insertParamInScope :: Type -> Token -> Bool -> Bool -> OurMonad ()
insertParamInScope  TypeVoid (TkId (l,c) lexeme ) _  _ = (tellError .concat) $ ["Error:",show l,":",show c," ",lexeme ," is VOIDtorb, but it may only be instanced as reference."]
insertParamInScope dcltype   (TkId (l,c) lexeme ) isGlob readonly = do 
    state <- get
    if isMember (zipp state) lexeme -- Most recent scope
    then tellError error1
    else do 
      tellLog whathappened
      let (newofs,padd) = (align . getOfs . fst) $ zipp state
      sz <- varSize dcltype

      --tellError $ "Puse padding de " ++ show (newofs,padd) ++ " en " ++ lexeme
      isref <- gets isRef

      if (isRef state)
      then (onZip . apply) $ insert  lexeme (scopevar (Reference (newofs)))  (padd+4)
      else (onZip . apply) $ insert  lexeme (scopevar (Offset    (newofs)))  (padd+sz)
  where error1       = generror ++ " in actual scope."
        error2       = generror ++ " in global scope."
        scopevar  d  = (Variable (l,c) dcltype readonly d)
        generror     = "Error:" ++ show l ++":"++show c ++" redefinition of " ++ lexeme
        whathappened = "Added " ++ lexeme ++" at "++show l++":"++show c ++ " with type " ++ show dcltype


-- Check if datatype is enum and insert as readonly
checkEnumAndInsert :: Token -> Token -> OurMonad ()
checkEnumAndInsert (TkDId (lD,cD) lexemeD) (TkId (l,c) lexeme) = do
    state <- get
    if isInScope (scp state) lexemeD  -- Check in globals for enum
        then if enumMatches (fromJust (getValS lexemeD (scp state))) lexemeD -- if its enum and has same name --enumMatches (fromJust (getValS lexeme (scp state))) lexemeD
                then do tellLog whathappened
                        onScope $ insert lexeme (Variable (l,c) (TypeEnum lexemeD) True (Offset 0 )) 0 --Se debe CAMBIAR
                else tellError error2
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

checkItsDeclared' :: Token -> OurMonad ((Type,Token,Exp))
checkItsDeclared' tk = do
    state <- get
    if (not . isNothing)  (lookUp (zipp state) (lexeme tk))
        then do (tellLog . concat) whathappened
                return ((typeFound (lookUp (zipp state) (lexeme tk))),tk, (ExpVar (fromJust (lookUp (zipp state) (lexeme tk))) (lexeme tk) ))
        else if isInScope (scp state) (lexeme tk)
                then return (( typeFound (getValS (lexeme tk) (scp state))),tk,(ExpVar (fromJust (getValS (lexeme tk) (scp state))) (lexeme tk) ))
                else tellError error1 >> return (TypeError,tk,NoExp)
  where whathappened = ["Variable ",lexeme tk," at ",(show . position) tk," well used."]
        error1       = strError (position tk) " variable or datatype" (lexeme tk) "used but not declared."
        typeFound    = storedType . fromJust

buildVar :: Token -> OurMonad(Exp)
buildVar tk = do 
    state <- get
    maybe (maybe (tellError error1 >> return NoExp)
                 (\ globalDec -> return (ExpVar globalDec (lexeme tk)) )
                 ( getValS (lexeme tk) (scp state)))
          (\ localDec -> (tellLog .concat) whathappened >>return (ExpVar localDec (lexeme tk)))
          (lookUp (zipp state) (lexeme tk))
 where error1       = strError (position tk) " variable or datatype" (lexeme tk) "used but not declared."
       whathappened = ["Variable ",lexeme tk," at ",(show . position) tk," well used."]

getDeclare :: Token -> OurMonad(Maybe(Declare))
getDeclare tk = do 
    state <- get
    maybe (maybe (return Nothing)
                 (\ globalDec -> return (Just globalDec))
                 ( getValS (lexeme tk) (scp state)))
          (\ localDec -> (tellLog .concat) whathappened >> return (Just localDec))
          (lookUp (zipp state) (lexeme tk))
 where error1       = strError (position tk) " variable or datatype" (lexeme tk) "used but not declared."
       whathappened = ["Variable ",lexeme tk," at ",(show . position) tk," well used."]



expIns :: Exp -> (Type,Token) -> OurMonad((Type,Token,Exp))
expIns ins (TypeError,to) = return (TypeError,to,NoExp)
expIns ins (ty,to)        = return (ty,to,ins)

expIns' :: Exp -> (Type,Token) -> OurMonad((Type,Token,Exp))
expIns' (Binary op a b) (TypeInt,to)   = return (TypeInt,  to, (Binary op a b))
expIns' (Binary op a b) (TypeFloat,to) = return (TypeFloat,to,(Binary op' a b))
  where op' = case op of
              Negi      -> Negf 
              Plusi     -> Plusf 
              Minusi    -> Minusf
              Multiplyi -> Multiplyf
              _         -> op
expIns' ins (t,to)      = return (TypeError,to,NoExp)

expInsF :: Exp -> ((Type,Token),Bool) -> OurMonad((Type,Token,Exp))
expInsF (CallVal a b s _) (t,boolProc)   = expIns (CallVal a b s boolProc) t
expInsF a (t,_)                      = expIns a t


checkFloatDiv ::  [Type] -> Type -> Type -> Token -> Token -> OurMonad ((Type,Token))
checkFloatDiv expected TypeInt TypeInt tok tok2   = return (TypeInt,tok2)
checkFloatDiv expected TypeFloat TypeInt tok tok2 = return (TypeFloat,tok2)
checkFloatDiv expected t t2 tok tok2           = do tellError error  >> return (TypeError,tok2)
    where error = strError (position tok) "Types in the operator" (toStr tok) ("must be butterFloat or pINTachu / pINTachu.")

checkNeg      :: (Type,Token,Exp) -> OurMonad((Type,Token,Exp))
checkNeg (TypeInt,to,exp1)   = return (TypeInt,to,(Unary Negi exp1))
checkNeg (TypeFloat,to,exp1) = return (TypeFloat,to,(Unary Negf exp1))
checkNeg (_,to,_)            = do tellError error >> return (TypeError,to,NoExp)
    where error = strError (position to) "Types in the operator" (toStr to) ("must be either butterFloat or pINTachu.")

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


checkFunctionCall :: Token -> TypeTuple -> OurMonad(((Type,Token),Bool))
checkFunctionCall ident calltup = do
    res <- checkIsFunc ident
    if isNothing res 
        then return ((TypeError,ident),False)-- Nothing to do, error
        else do let funcSig = (getTuple . storedType . fromJust) res
                let boolProc  = isProc $ fromJust res
                if lengthMatches calltup funcSig 
                then if trd $ tuplesMatch calltup funcSig
                     then do tellLog "Function call types work"
                             return ((funcReturnType funcSig,ident),boolProc)
                     else do tellError . (error1 (calltup)) $ tuplesMatch calltup funcSig
                             return ((TypeError,ident),False)
                else tellError error2 >> return ((TypeError,ident),False)
  where trd  (_,_,a) = a
        fst' (a,_,_) = a
        error1 typegot (expected,p,_) = strError (position ident) "Error in the call of" (lexeme ident) ("argument number "++show p++" didn't match with expected " ++ show expected ++ " but " ++ show typegot ++ " found.")
        error2  = strError (position ident) "number of arguments don't match with" (lexeme ident) "declaration."

checkFunctionType :: Type -> OurMonad( )
checkFunctionType typ = if typ == TypeVoid then setRef else return ()

checkPointer :: Token -> (Type,Token,Exp) -> OurMonad((Type,Token,Exp))
checkPointer tok tup = if (isError (sel1 tup)) 
    then return (TypeError,sel2 tup,NoExp) 
    else  if (not (isPointer (sel1 tup))) 
            then do tellError (strError (position tok) "" (lexeme (sel2 tup)) (" expecting a pointer expression but " ++ show (sel2 tup) ++ "found"))
                    return (TypeError,sel2 tup,NoExp)
            else return (stripPointer(sel1 tup),sel2 tup,Unary Access (sel3 tup))

checkFieldAccess :: (Type,Token,Exp) -> Token -> OurMonad((Type,Token,Exp))
checkFieldAccess (TypeError,tk1,_) _ = return (TypeError,tk1,NoExp)
checkFieldAccess (TypePointer ty1,tk1,exp1) tk2 = checkFieldAccess (ty1,tk1,exp1) tk2
checkFieldAccess (ty1,tk1,exp1) tk2 = do
    state <- get
    if structured ty1 
    then do let strScope = (fields . fromJust) $ getValS (getDataName ty1) 
                                                         (scp state) 
            if isInScope strScope l2
            then do let dec = fromJust $ getValS l2 strScope
                    return (storedType dec, tk2, Binary Access exp1 (ExpVar dec l2) )
            else tellError (error2 (getDataName ty1)) >> return (TypeError,tk1,NoExp)
    else tellError error1  >> return (TypeError,tk1,NoExp)
  where error1 = strError (position tk1) "Variable" l "it's not a valid struct/union, field cannot be accessed" -- AQUI
        error2 dn = strError (position tk1) "Variable" l2 ("not found in struct/union " ++ show dn )
        l      = lexeme tk1
        l2     = lexeme tk2

buildPrint :: Token -> Ins -> Type -> OurMonad((Type,Ins))
buildPrint string i t = do 
    (mem_addr,is_new) <- new_str_lex (content string)
    -- Agregar este strings a tabla
    let dec = StrCons (position string) mem_addr (content string)
    if is_new then onStrScope $ (:) dec
              else return ()
    -- Pasar la direccion en vez de esta basura: (S.singleton (ExpVar dec (content string)))
    let new_ins = Call "vamo_a_imprimi" (S.singleton (ExpVar dec (content string))) 4 True
    bleh <- checkOkIns new_ins i t
    return bleh

buildGenPrint ::  (Type,Token,Exp) -> Ins -> Type -> OurMonad((Type,Ins))
buildGenPrint (ty,tk,e) i  t = checkOkIns new_ins i t
  where func_call TypeInt   = "vamo_a_imprimi_i"
        func_call TypeBool  = "vamo_a_imprimi_i"
        func_call TypeChar  = "vamo_a_imprimi_c"
        func_call TypeFloat = "vamo_a_imprimi_f"
        func_call _         = ""
        call_str = func_call ty
        new_ins = if (call_str /= "") then Call call_str (S.singleton e) 4 False else NoOp

checkMain :: OurMonad()
checkMain = do
    globals <- gets scp
    if isInScope globals "hitMAINlee"
        then return ()
        else tellError $ strError (0,0) "" "hitMAINlee" "function not found"

-- Check if there is a mainFunction and add it at the begining of the list
checkMain' :: [(String,Ins,TypeTuple)] -> OurMonad( [(String,Ins,TypeTuple)] )
checkMain' functions = do
    globals <- gets strTbl
    let ((fs,td),list) = foldl processIns ((Nothing,S.empty),[]) functions
    if isJust fs
    then return (("hitMAINlee" , (fromJust fs), td) : list)
    else do tellError $ strError (0,0)"" "hitMAINlee" "function not found"
            return []
  where processIns ((a,s),l) (string,insTree,t) = if string == "hitMAINlee" 
                                              then ((Just insTree,t), l)
                                              else ((a,s), (string,insTree,t):l)

checkRecursiveDec :: Token -> TypeTuple -> OurMonad()
checkRecursiveDec dataTok typeSec = do 
    if isNotRecursiveData (lexeme dataTok) typeSec
        then return ()
        else tellError error1
  where error1 =  strError (position dataTok) "Data type" (lexeme dataTok) "cannot be recursive. (Pssss try to use a pointer)"

checkOkIns :: Ins -> Ins -> Type -> OurMonad ( (Type,Ins) )
checkOkIns ins block t = if t /= TypeError
                            then return (TypeVoid, ins `insertIns` block )
                            else return (TypeError, Error )

buildRead :: Token -> (Type,Ins) -> OurMonad((Type,Ins))
buildRead four one = do 
    itsReadable <- checkReadable four True 
    newVar      <- buildVar four
    checkOkIns (Read newVar ) (snd one) TypeVoid


-- Change by a instruction type default
adefault = undefined

checkAndBuild :: a -> Type -> OurMonad( (Type,a) )
checkAndBuild instruction t = if t /= TypeError
                              then return (TypeVoid ,instruction)
                              else return (TypeError,instruction)

--checkOK :: OurMonad (Type) -> Type -> OurMonad (Type)
checkOk action t = if t /= TypeError
                      then action
                      else return TypeError

{- Checking all the returns from monad types aren't errors -}
checkAllOk :: [OurMonad(Type)] -> Type -> Type -> String -> Token -> Int -> OurMonad(Type)
checkAllOk l TypeVoid TypeVoid _ tok _ =  
  do typeL <- (sequence l)
     if all (/= TypeError) typeL
     then return TypeVoid
     else return TypeError
checkAllOk l (TypeEnum s1) (TypeEnum s2)  _  tok 0 = if s1==s2 then checkAllOk l TypeVoid TypeVoid "" tok 0
                                                 else return TypeError
checkAllOk l (TypeEnum s1) (TypeEnumCons) s2 tok 0 = do
  enuTbl <- gets enuTbl
  if ((not.isNothing) (getValS s2 enuTbl)) then 
    if s1 == ( storedDType $ fromJust $ getValS s2 enuTbl) 
    then checkAllOk l TypeVoid TypeVoid "" tok 0
    else return TypeError
  else
    return TypeError
  --else do return TypeError
checkAllOk l a b s tok t
    | checkAssign a b t = checkAllOk l TypeVoid TypeVoid s tok 0
    | otherwise       = do
                        tellError error1 >> return TypeError
    where error1= strError (0,0) "Incompatible types on asignation: " (toStr tok)  $ "\""++show a ++ (toStr tok)  ++ show b ++ "\""

getBaseType :: Type -> Type
getBaseType (TypeArray t1 d1) = getBaseType t1
getBaseType a                 = a

checkAssign :: Type -> Type -> Int -> Bool
checkAssign (TypeField _ TypeInt) TypeInt at = True
checkAssign (TypeField _ TypeBool) TypeBool 0 = True
checkAssign (TypeField _ TypeChar) TypeChar 0 = True
checkAssign (TypeField _ TypeFloat) TypeFloat at = True
checkAssign (TypeArray t1 d1) t2 0 = (getBaseType t1) == t2 && isBasic t2
checkAssign (TypeArray t1 d1) t2 t = (getBaseType t1) == t2 && isNumeric t2
checkAssign t1 (TypeArray t2 d2) 0 = (getBaseType t2) == t1 && isBasic t1
checkAssign t1 (TypeArray t2 d2) t = (getBaseType t2) == t1 && isNumeric t1
checkAssign TypeBool TypeBool 0 = True
checkAssign TypeChar TypeChar 0 = True
--checkAssign TypeFloat TypeInt at = True
checkAssign TypeFloat TypeFloat at = True
checkAssign TypeInt   TypeInt at = True
--checkAssign TypeEnum TypeEnumCons 0 = True
--checkAssign TypeEnum TypeEnum 0 = True
checkAssign (TypePointer t1) (TypePointer t2) 0 =  checkAssign t1 t2 0
checkAssign (TypePointer _) TypeInt 0 =  True
checkAssign (TypeEmptyArray t1) t2 0 = t1 == t2
--checkAssign (TypeArray t) t 0 = True
checkAssign _ _ _ = False

checkOkType :: OurMonad ()  -- Action to execute
                 -> Type      -- Type obtained
                   -> Type      -- Expected Type
                     -> Token     -- If error found, report with string
                       -> Type      -- Returning Type
                         -> OurMonad (Type)
checkOkType _ TypeError _ _ _ = return TypeError
checkOkType ac t expectedT tok rt
    | t == expectedT = ac >> return rt
    | otherwise      = tellError err >> return TypeError
    where err = strError (position tok) 
                         ("Expected " ++ (show expectedT) ++ " on") (toStr tok) ("but " ++ (show t) ++ " found")

checkGuarded :: Token                  -- If token
                 -> (Type,Token,Exp)    -- Bool Exp
                   -> (Type,Ins)          -- Instruction inside if
                       -> OurMonad(Type)      -- Returning Type
checkGuarded tok (t,expTk,_) (typeif,_) = do
    t1 <- checkOkType (return ()) t TypeBool tok TypeVoid
    if (t1 == TypeVoid) && (typeif == TypeVoid) 
        then return TypeVoid
        else return TypeError

checkFor :: Token                  -- For Token
              -> [(Type,Token,Exp)]   -- Int Exp
                -> OurMonad(Type)
checkFor tok expList = do
    typeList <- mapM checkIsInt expList
    if (all (== TypeVoid) typeList)
        then return TypeVoid
        else return TypeError
  where checkIsInt (ty,token,_) = checkOkType (return ()) ty TypeInt token TypeVoid

-- Check if upper limit and lower limits match over ENUM for iteration
checkEnumFor :: Token                  -- For Token
              -> Token                 -- Variable to insert as enum
                -> Token -> Token        -- Enum constants
                    -> OurMonad(Type)
checkEnumFor tok newVar enum1 enum2 = do
    state <- get
    type1 <- findEnum enum1 state 
    type2 <- findEnum enum2 state
    if (type1 /= TypeError) && (type2 /= TypeError)
        --then if trace ("Comparando " ++ (show type1) ++ " y " ++ (show type2) ++ " y " ++ (show (type1 == type2)) ) (type1 == type2)
        then if (type1 == type2)
                then return TypeVoid   -- insertar newVar ya que se conoce la variable, o meh
                else do (tellError (err2 type1 type2)) >>return TypeError
        else return TypeError
  where findEnum t state = maybe (tellError err1 >> return TypeError)
                                 (\dec-> if isEnumCons dec 
                                         then return (TypeEnum (storedDType dec))
                                         else do tellError ("something happened over for/enum" ++ show (position t))
                                                 return TypeError)
                                 (getValS (lexeme t) (enuTbl state))
          where err1       =  strError (position t) 
                              "Undeclared enum constant" 
                              (lexeme t) 
                              " on iteration limit."
        err2 t1 t2 = strError (position tok) 
                              "Types do not match over" 
                              (toStr tok) 
                              ("upper limit is " ++ (show t1) ++ " and lower limit is " ++ (show t2) )

checkArray :: Token -> [Exp] -> OurMonad((Type,Token,Exp))
checkArray tok list = do
    varDec <- getDeclare tok 
    maybe (return (TypeError,tok,NoExp))
          (\ dec -> do 
              let (final_t,expBuilt) = arrayParser (stripArray (storedType dec)) (reverse list)
              let finalExp = (Binary Access (ExpVar dec (lexeme tok)) expBuilt)
              return (final_t, tok, finalExp))
          varDec
  where error1 = "HELP array"


--base +  (j * tamDimJ + i) * tamTipo
--base +  ((k * tamDimK + j) * tamDimJ + i) * tamTipo
arrayParser :: Type -> [Exp] -> (Type,Exp)
arrayParser (TypeArray t dim) (exp:[]) = (t,(Binary Multiplyi (ExpInt (getSize t)) (exp)))
arrayParser ts exps = (final_t,expBuilt)
  where 
    (dims,final_t) = dimensionArray ts
    dims' = dims ++ [getSize final_t]
    expBuilt = foldl buildExp (Binary Multiplyi (head exps) (ExpInt (head dims')) ) (tail expList)
    buildExp accExp (dim,i) = (Binary Multiplyi (Binary Plusi accExp i) (ExpInt dim))
    expList  =  zip dims' exps



dimensionArray :: Type -> ([Int],Type)
dimensionArray (TypeArray t1 dim) = ( dim : dimesions  ,final_t)
    where (dimesions,final_t) = dimensionArray t1 
dimensionArray final_t            = ([],final_t)

--arrayParser :: Exp -> [Exp] -> Exp
--arrayParser var = foldr nest var
--                where
--                  nest nLevel var = (Binary Access nLevel var)

--checkAll :: Token -> [Type] -> [Type] -> OurMonad (Type)
--checkAll t obtained expected = if all (not isError) obtained
--                               then process $ foldl findErrors [] (zip obtained expected)
--                               else return TypeError
--  where findErrors errs (o,e) =  if (o /= e)
--                                 then "" : errs
--                                 else errs
--        process []   = return TypeVoid
--        process errs = 





sel1 :: (Type,Token,Exp) -> Type
sel1 (a,_,_) = a

sel2 :: (Type,Token,Exp) -> Token
sel2 (_,b,_) = b

sel3 :: (Type,Token,Exp) -> Exp
sel3 (_,_,c) = c




addToBlock :: Ins -> OurMonad()
addToBlock i =  undefined

builtinFunctions :: SymTable
builtinFunctions = foldl insertFunc emptyScope declarations  --insertFunction :: TypeTuple -> Token -> Bool -> OurMonad ()
 where insertFunc scp (str,dec) = insert str dec 0 scp 
       printable t    = or $ map ($t) [(==TypeString),isPointer,isBasic,(==TypeEnumCons)]          
       makeFunc types = (Function (-1,-1) (makeTypeTuple types) emptyScope True)
       declarations = [
         ("vamo_a_lee"    , makeFunc [TypeSatisfies printable] ),
         ("liberar"       , makeFunc [TypeSatisfies isPointer, TypeVoid] ),
         ("vamo_a_imprimi", makeFunc [TypeSatisfies printable, TypeVoid] ),
         ("atrapar"       , makeFunc [TypeInt      , TypeVoid  ] ),
         ("intToFloat"    , makeFunc [TypeInt      , TypeFloat ] ),
         ("intToChar"     , makeFunc [TypeInt      , TypeChar  ] ),
         ("charToInt"     , makeFunc [TypeChar     , TypeInt   ] ),
         ("floor"         , makeFunc [TypeFloat    , TypeInt   ] ),
         ("celing"        , makeFunc [TypeFloat    , TypeInt   ] ),  
         ("succ"          , makeFunc [TypeEnumCons , TypeEnumCons ] ),
         ("pred"          , makeFunc [TypeEnumCons , TypeEnumCons ] ),
         ("pidGET"        , makeFunc [TypeEnumCons , TypeInt   ] ),
         ("SIZEther",(Function (-1,-1) 
                               (makeTypeTuple 
                                [TypeSatisfies isBasic, TypeInt]) 
                               emptyScope True))]
-- builtinFunctions = emptyScope
