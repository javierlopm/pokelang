module TableTree(
    Scope  (..),
    Zipper (..)
    ) where

import qualified Data.Map.Strict as Map
-- import import qualified Data.Sequence as S
import Data.Maybe

type SymbolTable a = Map.Map String a

data Action = UpA | DownA | LeftA | RightA | RootA | StChild
				  deriving(Show)	

data Scope a = Scope (SymbolTable a) [Scope a] -- Y si... usamos sequence aqui para tenerlos ordenados?
             deriving (Show) -- Sustituir por el show mostrado en clases

data Breadcrumb a = Breadcrumb { left  :: [SymbolTable a]
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
enterScope (Scope symtable []) = Scope symtable (emptyScope:[])
enterScope (Scope symtable (lst@(Scope stc chld):childL)) = Scope symtable (emptyScope:lst:childL)

insert :: String -> a -> Scope a -> Scope a
insert key val (Scope symtable chl) = Scope (addEntry key val symtable) chl

-- Zipper
fromSymTable :: Scope a -> Zipper a
fromSymTable orig@(Scope symt chdrn)= (orig,Breadcrumb [] chdrn [RootA] )
--fromSymTable orig@(Scope symt chdrn)= (orig,Breadcrumb [] chdrn ((map (\x -> StChild) chdrn)++[RootA])) 

-- No funciona -- Revisar si conviene trabajar con ST o con Scopes
down :: Zipper a -> Maybe (Zipper a)
down (Scope symt [] , breadcrumbs) = Nothing
down (Scope symt (ch:chdrn) , breadcrumbs) = Just (ch,newBread)
    where 
          newBread = Breadcrumb (symt:(left breadcrumbs)) (chdrn++(right breadcrumbs)) (DownA:((map (\x -> StChild) chdrn)++(action breadcrumbs)) )
    	  --newBread = Breadcrumb (symt:(left breadcrumbs)) (chdrn++(right breadcrumbs)) (DownA:(action breadcrumbs)) 

--apply :: (Scope a -> Scope a) -> Zipper a -> Zipper a
--apply f (scope,breadcrumbs) = (f scope,breadcrumbs)

-- Corrida so far
-- Crear un nodo vacio, convertirlo en zipper, insertar hola con 42, crearle un hijo, ir al hijo
-- apply enterScope  $ apply (insert "hola" 42) $ fromSymTable  emptyScope

test = newtable
test2 = addEntry "elem1" 1 test
test3 = addEntry "elem2" 2 test2
test4 = addEntry "elem3" 3 test3
s1 = emptyScope
s2 = insert "elem1" 1 s1
s3 = insert "elem2" 2 s2
s4 = insert "elem3" 3 s3
s5 = enterScope s4
z1 = fromSymTable s5
aux1 = newtable
aux12 = addEntry "I'm Hijo" 1 aux1
aux2 = addEntry "I'm 2 Hijo" 2 aux1
z1_2 = ((Scope test4 [(Scope aux1 []),(Scope aux2 [])])     ,    (Breadcrumb {left = [], right = [(Scope aux1 [])], action = [RootA]})     )   --Nodo con dos hijos
z1d = down z1_2
z1c = fromJust z1d
