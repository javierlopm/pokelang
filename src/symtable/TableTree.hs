module TableTree(
    Scope  (..),
    Zipper (..)
    ) where

import qualified Data.Map.Strict as Map
-- import import qualified Data.Sequence as S
import Data.Maybe

type SymbolTable a = Map.Map String a

data Scope a = Scope (SymbolTable a) [Scope a] -- Y si... usamos sequence aqui para tenerlos ordenados?
    deriving (Show) -- Sustituir por el show mostrado en clases


data Breadcrumbs a = Breadcrumbs [(SymbolTable a,[Scope a],[Scope a])]
    deriving(Show)

-- Zipper como nodo actual y tupla (Tabla de padre, hermanos a la izq y a la der
type Zipper a = (Scope a, Breadcrumbs a )

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
fromSymTable = flip (,) $ Breadcrumbs []

-- No funciona -- Revisar si conviene trabajar con ST o con Scopes
down :: Zipper a -> Maybe (Zipper a)
down (Scope symt [] , breadcrumbs) = Nothing
down (Scope symt (ch:chdrn) , breadcrumbs) = Just (ch,(oldstep:breadcrumbs))
    where oldstep = (symt,[],chdrn)

apply :: (Scope a -> Scope a) -> Zipper a -> Zipper a
apply f (scope,breadcrumbs) = (f scope,breadcrumbs)

-- Corrida so far
-- Crear un nodo vacio, convertirlo en zipper, insertar hola con 42, crearle un hijo, ir al hijo
-- apply enterScope  $ apply (insert "hola" 42) $ fromSymTable  emptyScope



test = newtable
test2 = addEntry "elem1" 1 test
test3 = addEntry "elem2" 2 test2
s1 = emptyScope
s2 = insert "elem1" 1 s1
s3 = insert "elem2" 2 s2
s4 = enterScope s3
z1 = fromSymTable s3
