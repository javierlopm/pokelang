{-# LANGUAGE OverloadedStrings #-}
module TacToMips(module TacToMips) where

import Prelude hiding(replicate,mapM_,lookup)
import Data.Word
import Tac
import qualified Data.Text     as T
import qualified Data.Sequence as S(empty,null)
import qualified Data.Vector   as V(empty,null)
import qualified Data.Map      as M(empty)
import Data.Foldable as F(foldl)
import Data.Foldable (toList,mapM_)
import Data.Functor  (fmap)
import Control.Monad.State hiding(mapM_)
import Data.Sequence       hiding(replicate,null,empty)
import Data.Vector         hiding(toList,(++),concat,null,empty,any,mapM_,sequence,mapM,and,map)
import Data.Map.Strict     hiding(toList,null,empty,findIndex,(!),map)
import System.Random

type Mips       = T.Text
type Register   = Int
type Registers  = [Int]

lowreg  = 8
highreg = 23

-- numRegs = highreg - lowreg + 1
numRegs = 16

fp :: Register
fp = 31
sp :: Register
sp = 30


-- compile :: Program -> Program -> Mips -> Mips
-- compile globs program crt = T.concat [".data\n"
--                                      ,translate globs
--                                      ,"\n.text\n"
--                                      ,crt]
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
build3Mips m r1 r2 r3 = "    "~~m~~" $"~~(stt r1)~~",$"~~(stt r2)~~",$"~~(stt r3)~~"\n"

buildiMips :: Mips -> Register -> Register -> Int -> Mips
buildiMips m r1 r2 c = "    "~~m~~" $"~~(stt r1)~~",$"~~(stt r2)~~", "~~(stt c)~~"\n"

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

load :: Var -> Register -> Mips
load _ _ = ""

addOffset :: Int -> Maybe Int
addOffset a = Just (a+lowreg)

findVar :: Var -> Vector [Var] -> Maybe(Register)
findVar Fp  _ = Just fp
findVar var v = (findIndex (any (var==)) v) >>= addOffset 

findEmpty :: Vector [Var] -> Maybe(Register)
findEmpty v = (findIndex null v) >>= addOffset


findRegister :: Var -- Variable to store in a register
                  -> Maybe Register -- Register that cannot be used for spill
                   -> MipsGenerator(Register)
findRegister var canRemove = do 
    regs <- gets regDescriptor
    maybe (searchEmpty regs) return (findVar var regs)
  where searchEmpty r    = maybe doSpill loadNReturn (findEmpty r)
        loadNReturn reg  = do emit (load var reg)
                              updateRegDescriptors reg (const [var])
                              -- updateVarDescriptors reg (const [var])
                              return reg
        doSpill   = maybe getAnyReg getAnyButThis canRemove
        getAnyReg :: MipsGenerator(Register)
        getAnyReg = do regFound <- liftIO $ randomRIO (0,numRegs-1) -- Se queda pegado si ninguno de los 16 registros
                       hbu      <- hasBackUp regFound    -- Vive en sus posiciones de memoria
                       if hbu then (loadNReturn regFound) else getAnyReg
        getAnyButThis reg = do regFound <- liftIO $ randomRIO (0,numRegs)
                               hbu      <- hasBackUp regFound
                               if hbu && (regFound /= reg) 
                                    then loadNReturn regFound
                                    else getAnyButThis reg


newRegisters :: Vector [Var]
newRegisters = replicate numRegs [(Int_Cons 5)] -- 23-8+1

initDescriptor :: Descriptor
initDescriptor = Descriptor newRegisters M.empty T.empty

type MipsGenerator = StateT Descriptor IO

-- Remove defitions 
-- killVar :: Var -> MipsGenerator()
-- killVar _ = undefined


-- Copy descript
-- copyDescriptor :: Var -> Var -> MipsGenerator()
-- copyDescriptor = undefined

-- Check in var descriptor if v doesn't live only in a register
-- livesInMem :: Var -> MipsGenerator(Bool)
-- livesInMem = undefined

-- Check if all vars in that register lives in memory
hasBackUp :: Register -> MipsGenerator(Bool)
hasBackUp r = do  reg  <- gets regDescriptor
                  vars <- gets varDescriptor
                  return $ and $ map (locateBackUp vars) (reg ! r)
    where  locateBackUp desc var = maybe True -- (error "culito") REVISAR Es un error... porque no está agregado
                                         (\ (vars,r1) -> ((not . null) vars) || (any (r /=) r1))
                                         (lookup var desc) 

clearDescriptor :: MipsGenerator()
clearDescriptor = do 
    state     <- get 
    put state { regDescriptor = newRegisters, varDescriptor = M.empty }

updateRegDescriptors :: Int -> ([Var] -> [Var]) -> MipsGenerator()
updateRegDescriptors reg func = do 
    registers <- gets regDescriptor
    state     <- get 
    -- liftIO $ putStrLn "update reg descriptors"
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

compile :: Seq(Program) -> MipsGenerator(Mips)
compile ps = mapM_ (\ b -> processBlock b >> clearDescriptor) ps 
                >> gets assembly 
                    >>= return

processBlock :: Program -> MipsGenerator ()
processBlock p = mapM_ processIns p

processIns :: IntIns -> MipsGenerator ()
processIns ins = 
    case ins of 
      (Addi r1 r2 (Int_Cons c))  -> get1reg "addi" r1 r2 c
      (Subi r1 r2 (Int_Cons c))  -> get1reg "addi" r1 r2 (-c)
      (Addi r1 (Int_Cons c)r2)   -> get1reg "addi" r1 r2 c
      (Subi r1 (Int_Cons c)r2)   -> get1reg "addi" r1 r2 (-c)
      (Subi r1 r2 r3)            -> get2regs "sub" r1 r2 r3
      (Addi r1 r2 r3)            -> get2regs "add" r1 r2 r3
      (Comment str)              -> emit $ "# " ~~ (T.pack str) ~~ "\n"
      (Tag     lb)               -> emit $ "_tag" ~~ (stt lb) ~~ ":\n"
      (TagS   str)               -> emit $ (T.pack str) ~~ ":\n"
      Nop                        -> emit "#nop\n"
      otherwise                  -> return ()
      -- (TagSC) tag para strings, usado en data, no aqui
   where get2regs str d r1 r2 = do 
            fstReg <- findRegister r1 Nothing 
            sndReg <- findRegister r2 (Just fstReg)
            emit $ build3Mips str 42 fstReg sndReg -- CAMBIAR, DESTINO DE CALCULO MAL COLCADO
            -- kill and update def de r1 con d
         get1reg str d r1 cons = do 
            fstReg <- findRegister r1 Nothing
            emit $ buildiMips str 42 fstReg cons


-- processIns :: IntIns -> MipsGenerator ()
-- processIns ins = do

  -- emit (template ins [0,5,13]) 
-- ver cuantos registros necesita la instruccion (regNeeded)
-- buscar los registros a usar
-- actualizar descriptores

-- processIns i@(Comment s)       = emit (template ins)
-- processIns i@(Tag     Label)   = emit (template ins)
-- processIns i@(TagS    String)  
-- processIns i@(TagSC   String String)
-- processIns i@(Jump     Label)
-- processIns i@(Jz       Src1 Label)
-- processIns i@(Jnotz    Src1 Label)
-- processIns i@(JLt      Src1 Src2 Label)
-- processIns i@(JGt      Src1 Src2 Label)
-- processIns i@(JLEq     Src1 Src2 Label)
-- processIns i@(JGEq     Src1 Src2 Label)
-- processIns i@(JEq      Src1 Src2 Label)
-- processIns i@(JNEq     Src1 Src2 Label)

runCompiler =  runStateT

-- loads, stores, array, separados

-- findRegister var v = maybe (maybe (error "no hay spills") id (findEmpty v))
--                            id
--                            (findVar var v)
