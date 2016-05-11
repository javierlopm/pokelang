	module TableTree(
    Scope  (..),
    Zipper (..),
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
    --fromSymTable,
    fromScope
    ) where

import qualified Data.Map.Strict as Map
-- import import qualified Data.Sequence as S
import Data.Maybe
import Data.List(intersperse)

type SymbolTable a = Map.Map String a

showSTL :: Show a => [(String,a)] -> Int -> String
showSTL myL i =  (replicate (i*2) ' ') ++ (concat $ intersperse (replicate (i*2) ' ') $ map (\(a,b) -> a ++ " " ++ show b ++ "\n") myL)


data Action = UpA | DownA | LeftA | RightA | RootA | StChild  -- Creo que Left Up no es usada.
				  deriving(Eq,Show)	

data Scope a = Scope (SymbolTable a) [Scope a] -- Y si... usamos sequence aqui para tenerlos ordenados?

instance  Show a => Show (Scope a) where
	show = showScope 0

showScope :: Show a => Int -> Scope a -> String
showScope i (Scope st chld) = "\n" ++ (replicate (i*2) ' ') ++ 
								"Level " ++ show i ++ ":\n" ++ 
							  (replicate (i*2) ' ') ++	"—————————" ++ "\n" ++
								showSTL (Map.toList st) i ++ concatMap (showScope (i+1)) chld 

--instance Show (Scope a) where
--	show (Scope  st chld) = intersparse "    " map (\(a,b) -> show a ++ " " ++ show b + "\n") myL


data Breadcrumb a = Breadcrumb { left  :: [Scope a]
					    	   , right :: [Scope a]
					    	   , action:: [Action]
						       }
				  deriving(Show)	

--data Breadcrumbs a = Breadcrumbs [Breadcrumb a]
--                   deriving(Show)

-- Zipper como nodo actual y tupla (Tabla de padre, hermanos a la izq y a la der
type Zipper a = (Scope a, Breadcrumb a)

-- Symbol table
newtable :: SymbolTable a
newtable = Map.empty

addEntry :: String -> a -> SymbolTable a -> SymbolTable a
addEntry = Map.insert

-- Scope
emptyScope :: Scope a
emptyScope = Scope newtable []

enterScope :: Scope a -> Scope a
enterScope (Scope symtable l)  = Scope symtable (emptyScope:l) -- Isn't it shorter?
-- enterScope (Scope symtable []) = Scope symtable (emptyScope:[])
-- enterScope (Scope symtable (lst@(Scope stc chld):childL)) = Scope symtable (emptyScope:lst:childL)

insert :: String -> a -> Scope a -> Scope a
insert key val (Scope symtable chl) = Scope (addEntry key val symtable) chl

--fromScope :: Scope a -> Zipper a
--fromScope orig@(Scope symt chdrn)= (orig,Breadcrumb [] chdrn [RootA] )

-- Zipper
fromScope :: Scope a -> Zipper a
fromScope orig@(Scope symt chdrn)= (orig,Breadcrumb [] [] [RootA] )
--fromSymTable orig@(Scope symt chdrn)= (orig,Breadcrumb [] chdrn ((map (\x -> StChild) chdrn)++[RootA])) 

fromZipper :: Zipper a -> Scope a --Only Root
fromZipper = fst

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

--getC :: [breadcrumb] -> 

goRight :: Zipper a -> Maybe (Zipper a)
goRight (scp, breadcrumbs) = case (snd brk2) of
                             []      -> Nothing       --Basicamente, si no quedan hermanos que revisar.
                             StChild:_ -> wentR
                             otherwise -> Nothing
                where acts  = (action breadcrumbs)
                      wentR = Just $ ((head (right breadcrumbs)), newBread)
                      brk1  = break (\x -> (x== DownA) ||(x==RootA)) acts                      
                      brk2  = break (/= RightA) $ fst brk1
                      newBread = Breadcrumb (scp:(left breadcrumbs)) (tail (right breadcrumbs)) $ (RightA : fst brk2) ++ (tail (snd brk2)) ++ snd brk1

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
wentLeft inp@(scp, (Breadcrumb lft rgt (oact))) = inp

goUp :: Zipper a -> Maybe (Zipper a)
goUp (scp, (Breadcrumb lft rgt []))      = Nothing
goUp (scp, (Breadcrumb lft rgt [RootA])) = Nothing
goUp inp@(scp, (Breadcrumb lft rgt (DownA:lact))) = Just (wentUp inp [])
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
{-Funciones restantes:

--ToRoot   --Regresar a la raiz Zipper(up hasta que action == [RootA])
--isMember (member a st) --Esta en la tabla
--LookUp   --Busca en el zipper hacia arriba y devuelve true o false
--LookUpZ  --Análogo pero devuelve el zipper donde lo consiguió
--InsertSZ  --Inserta Socpe al Zipper
--Print con el formato adecuado de niveles (print raiz -> print hijo -> print nieto <- hijo -> print 2dohijo -> print 2do nieto -> ... ) (creo que es asi, dfs, ¿no?)
--adaptar el Repl a esto


-}




-- Corrida so far
-- Crear un nodo vacio, convertirlo en zipper, insertar hola con 42, crearle un hijo, ir al hijo
-- apply enterScope  $ apply (insert "hola" 42) $ fromScope  emptyScope

test = newtable
test2 = addEntry "elem1" 1 test
test3 = addEntry "elem2" 2 test2
test4 = addEntry "elem3" 3 test3
s1 = emptyScope
s2 = insert "elem1" 1 s1
s3 = insert "elem2" 2 s2
s4 = insert "elem3" 3 s3
s5 = enterScope s4
z1 = fromScope s5
aux1 = newtable
aux12 = addEntry "ImHijo" 1 aux1
aux2 = addEntry "Im2 Hijo" 2 aux1
aux3 = addEntry "Im3 Hijo" 3 aux1
aux4 = addEntry "Im4 Hijo" 4 aux1
aux5 = addEntry "Im5 Hijo" 5 aux1
aux6 = addEntry "Im6 Hijo" 6 aux1
z1_2 = ((Scope test4 [(Scope aux12 []),(Scope aux2 []),(Scope aux3 []),(Scope aux4 []),(Scope aux5 []),(Scope aux6 [])])     ,    (Breadcrumb {left = [], right = [], action = [RootA]})     )   --Nodo con dos hijos
z1d = goDown z1_2
z1c = fromJust z1d


v1 = (Scope test4 [(Scope aux12 []),(Scope aux2 []),(Scope aux3 []),(Scope aux4 []),(Scope aux5 []),(Scope aux6 [])])
x1 = fromScope v1 
x1c = fromJust $ goDown x1 
xlrl = fromJust $ goLeft $ fromJust $ goLeft $ fromJust $ goLeft $ fromJust $ goLeft $ fromJust $ goRight $ fromJust $ goRight $ fromJust $ goRight $ fromJust $ goRight x1c

