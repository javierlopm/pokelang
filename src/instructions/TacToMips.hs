{-# LANGUAGE OverloadedStrings #-}
module TacToMips(module TacToMips) where

import Prelude hiding(replicate)
import Control.Monad.State
import Data.Word
import Tac
import qualified Data.Text     as T
import qualified Data.Sequence as S(empty,null)
import qualified Data.Vector   as V(empty,null)
import qualified Data.Map      as M(empty)
import Data.Foldable as F(foldl)
import Data.Foldable (toList)
import Data.Functor  (fmap)
import Data.Sequence   hiding(replicate,null,empty)
import Data.Vector     hiding(toList,(++),concat,null,empty,any)
import Data.Map.Strict hiding(toList,null,empty,findIndex,(!))


type Mips       = T.Text
type Register   = Int
type Registers  = [Int]

lowreg  = 8
highreg = 23

fp :: Register
fp = 31
sp :: Register
sp = 30


compile :: Program -> Program -> Mips -> Mips
compile globs program crt = T.concat [".data\n"
                                     ,translate globs
                                     ,"\n.text\n"
                                     ,crt]
                    {-++ translate programcrt ++
                        "\n"        ++ crt-}
            -- crt

translate :: Program -> Mips
translate  = undefined

{- auxiliaries for templates -}
(~~) :: Mips -> Mips -> Mips
a ~~ b = a `T.append` b

stt :: Show a => a -> Mips
stt = T.pack . show

build3Mips :: Mips -> Register -> Register -> Register -> Mips
build3Mips m r1 r2 r3 = m~~"$"~~(stt r1)~~", $"~~(stt r2)~~", $"~~(stt r3)~~"\n"

buildiMips :: Mips -> Register -> Register -> Int -> Mips
buildiMips m r1 r2 c = m~~"$"~~(stt r1)~~", $"~~(stt r2)~~", "~~(stt c)~~"\n"

template :: IntIns -> [Register] -> Mips
template (Addi _ _ (Int_Cons c)) [r1,r2] = buildiMips "addi" r1 r2 c
template (Subi _ _ (Int_Cons c)) [r1,r2] = buildiMips "addi" r1 r2 (-c)
template (Addi _ (Int_Cons c) _) [r1,r2] = buildiMips "addi" r1 r2 c
template (Subi _ (Int_Cons c) _) [r1,r2] = buildiMips "addi" r1 r2 (-c)
template (Subi _ _ _) [r1,r2,r3]  = build3Mips "sub" r1 r2 r3
template (Addi _ _ _) [r1,r2,r3]  = build3Mips "add" r1 r2 r3
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
            | S.null ablock = blocks |> ablock
            | otherwise     = blocks 
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

data Descriptor = Descriptor { regDescriptor  :: Vector [Var]
                             , varDescriptor  :: Map Var ([Var],[Register])
                             , assembly       :: Mips } 
                     deriving (Show)


addOffset :: Int -> Maybe Int
addOffset a = Just (a+lowreg)

findVar :: Var -> Vector [Var] -> Maybe(Register)
findVar var v = (findIndex (any (var==)) v) >>= addOffset 

findEmpty :: Vector [Var] -> Maybe(Register)
findEmpty v = (findIndex null v) >>= addOffset

findRegister :: Var -> Vector [Var] -> Register
findRegister var v = maybe (maybe (error "no hay spills") id (findEmpty v))
                           id
                           (findVar var v)


newRegisters :: Vector [Var]
newRegisters = replicate 16 [] -- 23-8+1

initDescriptor :: Descriptor
initDescriptor = Descriptor newRegisters M.empty T.empty

type MipsGenerator = StateT Descriptor IO

killVar :: Var -> MipsGenerator()
killVar _ = undefined

copyDescriptor :: Var -> Var -> MipsGenerator()
copyDescriptor = undefined

updateRegDescriptors :: Int -> ([Var] -> [Var]) -> MipsGenerator()
updateRegDescriptors reg func = do 
    registers <- gets regDescriptor
    state     <- get 
    put state { regDescriptor = registers // [(reg,(func (registers ! reg)))]}

emit :: Mips -> MipsGenerator()
emit code = do
    acc    <- gets assembly
    state  <- get
    put state { assembly = T.append acc code }

-- clearDescriptor :: MipsGenerator ()
-- clearDescriptor = put $ Descriptor newRegisters M.empty 

getReg :: [Var] -> MipsGenerator([Registers])
getReg (lval:rval:[])      = undefined
getReg (lval:exp1:exp2:[]) = undefined

-- translate :: Seq(Program) -> MipsGenerator(Mips)