module TableTree(
    Scope  (..),
    Zipper ,
    goDown,
    goUp,
    goLeft,
    goRight,
    goTop,
    apply,
    emptyScope,
    enterScope,
    insert,
    insert0,
    isMember,
    isInScope,
    lookUp,
    fromScope,
    fromZipper,
    getVal,
    getValS,
    getOfs,
    showScope,
    fuse,
    cleanOffset,
   -- maxMapped,
    getAll,
    decList,
    changeSize,
    addSOffset
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as DS
import Data.Foldable(toList)
import Data.Sequence(empty,viewl,viewr,length,Seq,(|>),(<|),ViewL((:<)),ViewR((:>)),(><))
import Data.Maybe(fromJust,isNothing)
import Data.List (intercalate)

firstLocal = 12

-- Tabla de símbolos
type SymbolTable a = Map.Map String a

-- Auxiliar para mostrar tablas
showSTL :: Show a => [(String,a)] -> Int -> String
showSTL myL i =  replicate (i*2) ' ' ++ 
                  intercalate (replicate (i*2) ' ')  
                      (map (\(a,b) -> a ++ " " ++ show b ++ "\n") myL)

-- Acciones de movimiento
data Action = DownA | RightA | RootA | StChild
          deriving(Eq,Show) 

-- Scope
data Scope a = Scope { tb:: (SymbolTable a), ofs:: Int , chs :: (Seq(Scope a))}


instance  Show a => Show (Scope a) where
  show = showScope 0

showScope :: Show a => Int -> Scope a -> String
showScope i (Scope st ofs chld) = "\n" ++ replicate (i*2) ' ' ++ 
                "Level " ++ show i ++ ", Offset: "++show ofs++"\n" ++ 
                replicate (i*2) ' ' ++  "—————————\n" ++
                showSTL (Map.toList st) i ++ concatMap (showScope (i+1)) ((toList) chld) -- yarrrrr

data Breadcrumb a = Breadcrumb { left  :: [Scope a]
                               , right :: Seq(Scope a)
                               , action:: [Action]
                               }
          deriving(Show)  

type Zipper a = (Scope a, Breadcrumb a)

-- Symbol table
newtable :: SymbolTable a
newtable = Map.empty

addEntry :: String -> a -> SymbolTable a -> SymbolTable a
addEntry = Map.insert

-- Scope
emptyScope :: Scope a
emptyScope = Scope newtable 0 empty 

addSOffset :: Scope a -> Int -> Scope a
addSOffset (Scope st ofs l) of2 = Scope st (ofs+of2) l

cleanOffset :: Scope a -> Scope a
cleanOffset (Scope st _ l) = Scope st firstLocal l

enterScope' :: Scope a -> Scope a
enterScope' (Scope symtable ofs l)  = Scope symtable ofs ((addSOffset emptyScope ofs) <| l)
--BEezelbu es mi favorito. Despues de chiabe, ofc
enterScope'' :: Scope a -> Scope a
enterScope'' (Scope symtable ofs l)  = Scope symtable ofs ( l |> (addSOffset emptyScope ofs) ) 
-- Revisar

insert :: String -> a -> Int -> Scope a  -> Scope a
insert key val size (Scope symtable ofs chl)  = Scope (addEntry key val symtable) (ofs+size)  chl

insert0 :: String -> a -> Int -> Scope a -> Scope a
insert0 key val size (Scope symtable ofs chl)  = Scope (addEntry key val symtable) newsize  chl
    where newsize = max size ofs

-- Zipper
fromScope :: Scope a -> Zipper a
fromScope orig = (orig,Breadcrumb [] empty [RootA] )

enterScope :: Zipper a -> Zipper a
enterScope = fromJust . goDown . apply enterScope'

-- enterScope :: Zipper a -> Zipper a
-- enterScope = allwayRight . fromJust . goDown . apply enterScope' 


fromZipper :: Zipper a -> Scope a
fromZipper = fst . goTop

-- No funciona -- Revisar si conviene trabajar con ST o con Scopes
goDown :: Zipper a -> Maybe (Zipper a)
goDown (Scope symt ofc chls , breadcrumbs) 
    | DS.null chls  = Nothing
    | otherwise  = Just (ch,newBread)
    where (chdrn :> ch) = viewr chls
          newBread = Breadcrumb ( Scope symt ofc empty : left breadcrumbs )
                                (chdrn >< right breadcrumbs) 
                                ( replicate (Data.Sequence.length chdrn) (StChild)  ++ (DownA:(action breadcrumbs)))  --Guarda ST sin hijos para luego ponerselos al subir
        --newBread = Breadcrumb (symt:(left breadcrumbs)) (chdrn++(right breadcrumbs)) (DownA:(action breadcrumbs)) 

apply :: (Scope a -> Scope a) -> Zipper a -> Zipper a
apply f (scope,breadcrumbs) = (f scope,breadcrumbs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (scp, breadcrumbs) = case snd brk2 of
                             StChild:_ -> wentR
                             otherwise -> Nothing
    where acts  = (action breadcrumbs)
          wentR = Just $ (hrBr, newBread)
          (hrBr :< lrBr) = viewl (right breadcrumbs)
          brk1  = break (\x -> (x== DownA) ||(x==RootA)) acts                      
          brk2  = break (/= RightA) $ fst brk1
          newBread = Breadcrumb 
                      (scp:(left breadcrumbs)) 
                      lrBr $ 
                            (RightA : fst brk2) ++ (tail (snd brk2)) ++ snd brk1

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (scp, (Breadcrumb lft rgt [])) = Nothing
goLeft (scp, (Breadcrumb lft rgt (RightA:lact))) = Just ((head lft),(Breadcrumb (tail lft) (scp<|rgt) nAct))
    where acts  = (RightA:lact)
          brk1  = break (\x -> (x== DownA) ||(x==RootA)) acts

          brk2  = break (/= RightA) $ fst brk1
          nAct  = (tail (fst brk2) ++ ((snd brk2)) ++ StChild:snd brk1)            --RightA,RightA,StChild,goDown,...
goLeft (scp, (Breadcrumb lft rgt brc)) = Nothing

-- Move righ hasta que choque el hueso
allwayRight :: Zipper a -> Zipper a
allwayRight zi = if isNothing newright 
                    then zi
                    else allwayRight . fromJust $ newright
    where newright = goRight zi

getST :: Scope a -> SymbolTable a
getST (Scope st ofs chld) = st

getChld :: Scope a -> Seq (Scope a)
getChld (Scope st ofs chld) = chld

getOfs :: Scope a -> Int
getOfs (Scope st ofs chld) = ofs


wentUp :: Zipper a -> Seq(Scope a) -> Zipper a
wentUp (scp, (Breadcrumb lft rgt (DownA:lact)))   acc = ((Scope (getST (head lft)) (getOfs (head lft)) (acc|>scp)),(Breadcrumb (tail lft) rgt lact ))
wentUp (scp, (Breadcrumb lft rgt (StChild:lact))) acc = wentUp (scp, (Breadcrumb lft lrBr (lact))) (acc|>hrBr)
              where 
                (hrBr :< lrBr) = viewl rgt

wentLeft :: Zipper a -> Zipper a
wentLeft inp@(scp, (Breadcrumb lft rgt (RightA:lact))) = wentLeft $ fromJust $ goLeft inp
wentLeft inp@(scp, (Breadcrumb lft rgt (oact)))        = inp

goUp :: Zipper a -> Maybe (Zipper a)
goUp (scp, (Breadcrumb lft rgt []))      = Nothing
goUp (scp, (Breadcrumb lft rgt [RootA])) = Nothing
goUp inp@(scp, (Breadcrumb lft rgt (DownA:lact)))   = Just (wentUp inp empty)
goUp inp@(scp, (Breadcrumb lft rgt (StChild:lact))) = Just (wentUp inp empty)
goUp inp@(scp, (Breadcrumb lft rgt (RightA:lact)))  = Just (wentUp (wentLeft inp) empty)
goUp (scp, (Breadcrumb lft rgt brc))     = Nothing

goTop :: Zipper a ->  Zipper a
goTop inp@(scp, (Breadcrumb lft rgt act)) = if (act==[RootA] || act==[]) then inp
                         else  goTop $ fromJust $ goUp inp

isMember :: Zipper a -> String -> Bool
isMember ((Scope st _ _),brc) key = Map.member key st

isInScope :: Scope a -> String -> Bool
isInScope (Scope st _ _ ) key = Map.member key st

getValS :: String -> Scope a -> Maybe a
getValS key (Scope st _ _)= Map.lookup key st

getVal :: Zipper a -> String -> Maybe a
getVal ((Scope st _ _),brc) key = Map.lookup key st

lookUp  :: Zipper a -> String -> Maybe a
lookUp zip key = if isNothing mySearch
                    then if isNothing mayUp
                            then Nothing
                            else lookUp (fromJust mayUp) key
                    else mySearch
    where mySearch = getVal zip key
          mayUp    = (goUp zip)

fuse :: Scope a -> Zipper a -> Scope a
fuse (Scope smtbl ofs _ ) z =  Scope smtbl ofs (( chs . fromZipper) z)

decList :: Scope a  -> [a]
decList (Scope st ofs _ ) = map snd $ Map.toList st
-- maxMapppend (Scope tb _ _ ) f = (maximum . (map  f)  . toList) tb

getAll :: (a -> Bool) -> Scope a -> [(String,a)]
getAll f scp = Map.toList $ Map.filter f (tb scp)

-- Swap size for the new one
changeSize :: Scope a -> Int -> Scope a 
changeSize _ _ = undefined