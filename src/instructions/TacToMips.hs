module TacToMips(module TacToMips) where

import Prelude hiding(null)
import Data.Word
import Data.Sequence
import Data.Foldable as F(foldl)
import Data.Foldable(toList)
import Data.Functor(fmap)
import Tac

type Mips     = String
type Register = Word

fp :: Word
fp = 31
sp :: Word
sp = 30

-- data GetReg = GetReg { regDescriptor    :: [Declare]
--                      , enuTbl    :: SymTable
--                      , scp       :: SymTable
--                      , zipp      :: TableZipper
--                      , onUnion   :: Bool
--                      , str_count :: Int
--                      , isRef     :: Bool } 
--                            deriving (Show)

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
partition program = build $ F.foldl includeInLast (empty,empty) program
    where includeInLast (prg,actualBlock) ins 
            | isTag     ins = (prg |> (actualBlock |> ins), empty) 
            | isJump    ins = (prg |> actualBlock,  empty |> ins)
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
