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
    lookUp,
    fromScope,
    fromZipper
    ) where

-- import import qualified Data.Sequence as S
import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust,isNothing)
import Data.List (intercalate)

type SymbolTable a = Map.Map String a

data Action = DownA | RightA | RootA | StChild
          deriving(Eq,Show) 

showSTL :: Show a => [(String,a)] -> Int -> String
showSTL myL i =  replicate (i*2) ' ' ++ 
                  intercalate (replicate (i*2) ' ')  
                      (map (\(a,b) -> a ++ " " ++ show b ++ "\n") myL)


data Scope a = Scope (SymbolTable a) [Scope a] -- Y si... usamos sequence aqui para tenerlos ordenados?

instance  Show a => Show (Scope a) where
  show = showScope 0

showScope :: Show a => Int -> Scope a -> String
showScope i (Scope st chld) = "\n" ++ replicate (i*2) ' ' ++ 
                "Level " ++ show i ++ ":\n" ++ 
                replicate (i*2) ' ' ++  "—————————" ++ "\n" ++
                showSTL (Map.toList st) i ++ concatMap (showScope (i+1)) chld 

data Breadcrumb a = Breadcrumb { left  :: [Scope a]
                   , right :: [Scope a]
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
emptyScope = Scope newtable []

enterScope' :: Scope a -> Scope a
enterScope' (Scope symtable l)  = Scope symtable (emptyScope:l)

insert :: String -> a -> Scope a -> Scope a
insert key val (Scope symtable chl) = Scope (addEntry key val symtable) chl


-- Zipper
fromScope :: Scope a -> Zipper a
fromScope orig = (orig,Breadcrumb [] [] [RootA] )

enterScope :: Zipper a -> Zipper a
enterScope = fromJust . goDown . apply enterScope'

fromZipper :: Zipper a -> Scope a
fromZipper = fst . goTop

-- No funciona -- Revisar si conviene trabajar con ST o con Scopes
goDown :: Zipper a -> Maybe (Zipper a)
goDown (Scope symt [] , breadcrumbs) = Nothing
goDown (Scope symt (ch:chdrn) , breadcrumbs) = Just (ch,newBread)
    where 
          newBread = Breadcrumb ((Scope symt []):(left breadcrumbs)) (chdrn++(right breadcrumbs)) (((map (\x -> StChild) chdrn)++(DownA:(action breadcrumbs))) )  --Guarda ST sin hijos para luego ponerselos al subir
          --newBread = Breadcrumb (symt:(left breadcrumbs)) (chdrn++(right breadcrumbs)) (StChild:((map (\x -> StChild) chdrn)++(DownA:(action breadcrumbs))) )
        --newBread = Breadcrumb (symt:(left breadcrumbs)) (chdrn++(right breadcrumbs)) (DownA:(action breadcrumbs)) 

apply :: (Scope a -> Scope a) -> Zipper a -> Zipper a
apply f (scope,breadcrumbs) = (f scope,breadcrumbs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (scp, breadcrumbs) = case snd brk2 of
                             StChild:_ -> wentR
                             otherwise -> Nothing
    where acts  = (action breadcrumbs)
          wentR = Just $ ((head (right breadcrumbs)), newBread)
          brk1  = break (\x -> (x== DownA) ||(x==RootA)) acts                      
          brk2  = break (/= RightA) $ fst brk1
          newBread = Breadcrumb 
                      (scp:(left breadcrumbs)) 
                      (tail (right breadcrumbs)) $ 
                            (RightA : fst brk2) ++ (tail (snd brk2)) ++ snd brk1

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (scp, (Breadcrumb lft rgt [])) = Nothing
goLeft (scp, (Breadcrumb lft rgt (RightA:lact))) = Just ((head lft),(Breadcrumb (tail lft) (scp:rgt) nAct))
    where acts  = (RightA:lact)
          brk1  = break (\x -> (x== DownA) ||(x==RootA)) acts
          brk2  = break (/= RightA) $ fst brk1
          nAct  = (tail (fst brk2) ++ ((snd brk2)) ++ StChild:snd brk1)            --RightA,RightA,StChild,goDown,...
goLeft (scp, (Breadcrumb lft rgt brc)) = Nothing

getST :: Scope a -> SymbolTable a
getST (Scope st chld) = st

getChld :: Scope a -> [Scope a]
getChld (Scope st chld) = chld

wentUp :: Zipper a -> [Scope a] -> Zipper a
wentUp (scp, (Breadcrumb lft rgt (DownA:lact)))   acc = ((Scope (getST (head lft)) (scp:reverse acc)),(Breadcrumb (tail lft) rgt lact ))
wentUp (scp, (Breadcrumb lft rgt (StChild:lact))) acc = wentUp (scp, (Breadcrumb lft (tail rgt) (lact))) ((head rgt):acc)

wentLeft :: Zipper a -> Zipper a
wentLeft inp@(scp, (Breadcrumb lft rgt (RightA:lact))) = wentLeft $ fromJust $ goLeft inp
wentLeft inp@(scp, (Breadcrumb lft rgt (oact)))        = inp

goUp :: Zipper a -> Maybe (Zipper a)
goUp (scp, (Breadcrumb lft rgt []))      = Nothing
goUp (scp, (Breadcrumb lft rgt [RootA])) = Nothing
goUp inp@(scp, (Breadcrumb lft rgt (DownA:lact)))   = Just (wentUp inp [])
goUp inp@(scp, (Breadcrumb lft rgt (StChild:lact))) = Just (wentUp inp [])
goUp inp@(scp, (Breadcrumb lft rgt (RightA:lact)))  = Just (wentUp (wentLeft inp) [])
goUp (scp, (Breadcrumb lft rgt brc))     = Nothing

goTop :: Zipper a ->  Zipper a
goTop inp@(scp, (Breadcrumb lft rgt act)) = if (act==[RootA] || act==[]) then inp
                         else  goTop $ fromJust $ goUp inp

isMember :: Zipper a -> String -> Bool
isMember ((Scope st chld),brc) key = Map.member key st

getVal :: Zipper a -> String -> Maybe a
getVal ((Scope st chld),brc) key = Map.lookup key st

lookUp  :: Zipper a -> String -> Maybe a
lookUp zip key = if isNothing mySearch
         then (if isNothing mayUp
             then Nothing
             else lookUp (fromJust mayUp) key)
         else mySearch
        where mySearch = getVal zip key
              mayUp    = (goUp zip)
