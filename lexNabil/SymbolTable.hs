{-|

    UNIVERSIDAD SIMON BOLIVAR
    Departamento de Computacion y Tecnologia de la Informacion.
    CI3725 - Traductores e Interpretadores
    Abril - Julio 2015

    AUTORES:
        Edward Fernandez.   Carnet: 10-11121
        Nabil Marquez.      Carnet: 11-10683

    DESCRIPCION: Estructura de la tabla de simbolos asi como sus metodos y constructores.

  Versión 0.2 16-06-2015
-}

module SymbolTable 
  ( SymbolTable
  , create
  , insert
  , varOut
  , varName
  , get_table
  , get_dad
  , get_sons
  , get_maxStr
  , delete
  , update
  , isElem
  , isEmpty
  , lookupE
  , addSon
  , cDad
  , upperIsE
  , upperLookupE
  )
where
import AST
import Data.Map (Map)            
import qualified Data.Map as Map  
import Data.Maybe
                                

data SymbolTable
  = SymbolTable {table :: (Map String (String,String)), dad :: (Maybe SymbolTable), sons :: [SymbolTable], maxStr::Int}
  deriving(Eq,Show)

--------------------------------------------------------------------------------------
--Funciones getters de la tabla de simbolos.
--------------------------------------------------------------------------------------

get_table :: SymbolTable -> (Map String (String,String))
get_table = table

get_dad :: SymbolTable -> Maybe SymbolTable
get_dad = dad

get_sons :: SymbolTable -> [SymbolTable]
get_sons = sons

get_maxStr :: SymbolTable -> Int
get_maxStr = maxStr

--------------------------------------------------------------------------------------
--Constructores de la Tabla de Simbolos.
--------------------------------------------------------------------------------------

--Crear tabla vacia.
create :: SymbolTable
create =  SymbolTable {table=(Map.empty), dad=Nothing ,sons=[], maxStr=15}

--Insertar en tabla.
insert :: String -> (String,String) -> SymbolTable  -> SymbolTable
insert var val sT = sT {table=mST,maxStr=nmStr}
                    where 
                      mST=(Map.insert var val (get_table sT))
                      nmStr=max (length var) (get_maxStr sT)

--Unir dos tablas.
union :: SymbolTable -> SymbolTable  -> SymbolTable
union sT1 sT2 = sT1 {table=mST,maxStr=(max (get_maxStr sT1) (get_maxStr sT2))}
                where mST=(Map.union (get_table sT1) (get_table sT2))

--Borrar elemento de tabla.
delete :: Variable -> SymbolTable  -> SymbolTable
delete var sT = sT {table=mST}
                    where mST=(Map.delete (varName var) (get_table sT))

--Decide si var es elemento de una tabla de simbolos.
isElem :: String -> SymbolTable  -> Bool
isElem var sT = Map.member var (get_table sT)

--Decide si la tabla esta vacia.
isEmpty :: SymbolTable  -> Bool
isEmpty sT = (Map.keys (get_table sT))==[]

--Cambiar padre de tabla de simbolos.
cDad :: SymbolTable  ->  SymbolTable  ->  SymbolTable
cDad sT sTdad = sT {dad=Just sTdad} 

--Agregar hijos a la tabla de simbolos.
addSon :: SymbolTable  ->  SymbolTable  ->  SymbolTable
addSon sT sTson = sT {sons=(get_sons sT)++[sTson]} 

--Actualizar valor elemento en una tabla.
update :: String -> (String,String) -> SymbolTable  -> SymbolTable
update var val sT =
    if (isElem var sT) then sT {table=mST,maxStr=nmStr}
    else sT
    where 
      mST=(Map.insert var val (get_table sT))
      nmStr=max (length var) (get_maxStr sT)

--Buscar informacion respecto a un elemento en una tabla.
lookupE :: String -> SymbolTable  -> (String,String)
lookupE var sT = 
            if (isJust aux) then fromJust aux
            else ("NonType","NonValue")
            where
              aux=Map.lookup var (get_table sT)

--Buscar si un elemento pertenece al arbol de tablas recursivamente ascendente.
upperIsE :: String -> SymbolTable  -> Bool
upperIsE var sT = if isElem var sT then True
                  else if sT'==Nothing then False
                       else upperIsE var (fromJust sT')
                  where
                    sT'=get_dad sT

--Buscar informacion respecto a un elemento recursivamente ascendente.
upperLookupE :: String -> SymbolTable  -> (String,String)
upperLookupE var sT = if isElem var sT then lookupE var sT
                  else if sT'==Nothing then ("NonType","NonValue")
                       else upperLookupE var (fromJust sT')
                  where
                    sT'=get_dad sT

--Formato de Typo,Valor
varOut :: Variable -> (String,String) 
varOut (Var'  a) = ("Variable","0Null")

----Formato de Nombre
varName :: Variable -> String
varName (Var'  a) = a