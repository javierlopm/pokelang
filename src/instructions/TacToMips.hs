module TacToMips(module TacToMips) where

import Prelude hiding(null,replicate)
import Data.Word
import Data.Sequence hiding(empty,replicate)
import qualified Data.Sequence as S(empty)
import qualified Data.Vector   as V(empty)
import qualified Data.Map      as M(empty)
import Data.Foldable as F(foldl)
import Data.Foldable (toList)
import Data.Functor  (fmap)
import Data.Vector     hiding(toList,(++),concat,null,empty)
import Data.Map.Strict hiding(toList,null,empty)
import Tac
import Control.Monad.State


type Mips       = String
type Register   = Word
type Registers  = [Word]


fp :: Word
fp = 31
sp :: Word
sp = 30


compile :: Program -> Program -> Mips -> Mips
compile globs program crt = ".data\n"   ++ translate globs      ++ 
                        "\n.text\n" {-++ translate programcrt ++
                        "\n"        ++ crt-}
                        ++ crt

translate :: Program -> Mips
translate  = undefined

-- template :: Program -> [Register] -> Mips
-- template (Addi     Dest Src1 Src2)  = "addi " 
-- template (Subi     Dest Src1 Src2)  = ""
-- template (Divi     Dest Src1 Src2)  = ""
-- template (Mod      Dest Src1 Src2)  = ""
-- template (Multi    Dest Src1 Src2)  = ""
-- template (Pot      Dest Src1 Src2)  = ""
-- template (Negai    Dest Src1     )  = ""

partition :: Program -> Seq(Program)
partition program = build $ F.foldl includeInLast (S.empty,S.empty) program
    where includeInLast (prg,actualBlock) ins 
            | isTag     ins = (prg |> (actualBlock |> ins), S.empty) 
            | isJump    ins = (prg |> actualBlock,   S.empty |> ins)
            | otherwise     = (prg, actualBlock |> ins)
          build (blocks,ablock)
            | null ablock  = blocks |> ablock
            | otherwise    = blocks 
-- despues de un jump, nuevo elemento
-- en un tag         , nuevo elemento

-- Agregamos en bloque actual:
--      Si vemos un tag:
--          agregar, cerrar bloque actual y crear otro
--      Si vemos un jump:
--          cerrar bloque actual y agregar en el siguiente

showPartitions :: Seq(Program) -> String
showPartitions prg = concat $ toList $ fmap (\ p -> (showP p)++ "\n----------\n") prg


{- Inicio de getReg -}

data GetReg = GetReg { regDescriptor :: Vector [Var]
                     , varDescriptor :: Map Var ([Var],[Register]) } 
                     deriving (Show)

newRegisters :: Vector [Var]
newRegisters = replicate 32 []

initDescriptor :: GetReg
initDescriptor = GetReg newRegisters M.empty

type MipsGenerator = StateT GetReg IO

cleanDescriptor :: MipsGenerator ()
cleanDescriptor = put $ GetReg newRegisters M.empty

