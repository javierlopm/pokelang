module TableTree(
    Scope  (..),
    Zipper (..),
    newtable,
    addEntry,
    emptyScope,
    enterScope,
    insert,
    fromScope,
    down,
    apply
    ) where

import qualified Data.Map.Strict as Map
-- import import qualified Data.Sequence as S
import Data.Maybe

type SymbolTable a = Map.Map String a

data Action = Up | Down | Left | Right | None
				  deriving(Show)	

data Scope a = Scope (SymbolTable a) [Scope a] -- Y si... usamos sequence aqui para tenerlos ordenados?
             deriving (Show) -- Sustituir por el show mostrado en clases

data Breadcrumb a = Breadcrumb { left  :: [Scope a]
					    	   , right :: [Scope a] -- El primero en right ser'a el ultimo visitado
					    	   --, action:: Action
						       }
				  deriving(Show)	

--data Breadcrumbs a = Breadcrumbs [Breadcrumb a]
--                   deriving(Show)

-- Zipper como nodo actual y tupla (Tabla de padre, hermanos a la izq y a la der
type Zipper a = (Scope a, [Breadcrumb a])

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

-- Zipper
fromScope :: Scope a -> Zipper a
fromScope = flip (,) $ [] 

-- No funciona -- Revisar si conviene trabajar con ST o con Scopes
down :: Zipper a -> Maybe (Zipper a)
down (Scope symt [] , breadcrumbs)         = Nothing
down (Scope symt (ch:chdrn) , breadcrumbs) = Just (ch,((Breadcrumb [] [oldstep]):breadcrumbs))
    where oldstep = (Scope symt chdrn) --Al subir volvemos a armar el hijo 

apply :: (Scope a -> Scope a) -> Zipper a -> Zipper a
apply f (scope,breadcrumbs) = (f scope,breadcrumbs)

-- Corrida so far
-- Crear un nodo vacio, convertirlo en zipper, insertar hola con 42, crearle un hijo, ir al hijo
-- apply enterScope  $ apply (insert "hola" 42) $ fromScope  emptyScope

test = newtable
test2 = addEntry "elem1" 1 test
test3 = addEntry "elem2" 2 test2
s1 = emptyScope
s2 = insert "elem1" 1 s1
s3 = insert "elem2" 2 s2
s4 = enterScope s3
z1 = fromScope s3