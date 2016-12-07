{-# LANGUAGE OverloadedStrings #-}
module TacToMips(module TacToMips) where

import Prelude hiding(replicate,mapM_,lookup)
import Data.Word
import Tac
import qualified Tac as Taa
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

showComments = True

lowreg  = 8
highreg = 23

-- numRegs = highreg - lowreg + 1
numRegs  = 16
numfRegs = 31

gp :: Register
gp = 28
sp :: Register
sp = 29
fp :: Register
fp = 30
ra :: Register
ra = 31

fReserved :: Register
fReserved = 12


magicReg :: Register
magicReg = 22

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

stringsToMips :: Program -> Mips
stringsToMips strings = F.foldl convert ".data\n" strings
    where convert oS (TagSC l val) = oS~~"_"~~T.pack l~~":  .asciiz \""~~T.pack val~~ "\"\n"
          convert oS _             = ""

{- auxiliaries for templates -}
(~~) :: Mips -> Mips -> Mips
a ~~ b = a `T.append` b

stt :: Show a => a -> Mips
stt = T.pack . show

showReg :: Register -> Mips
showReg 28 = "$gp"
showReg 29 = "$sp"
showReg 30 = "$fp"
showReg 31 = "$ra"
showReg r  = "$" ~~ (stt r)

showfReg :: Register -> Mips
showfReg r  = "$f" ~~ (stt r)

build3Mips :: Mips -> Register -> Register -> Register -> Mips
build3Mips m r1 r2 r3 = "    "~~m~~" "~~(showReg r1) ~~","~~(showReg r2)~~","~~(showReg r3)~~"\n"

build3Mipsf :: Mips -> Register -> Register -> Register -> Mips
build3Mipsf m r1 r2 r3 = "    "~~m~~" "~~(showfReg r1) ~~","~~(showfReg r2)~~","~~(showfReg r3)~~"\n"

build3MipsB :: Mips -> Register -> Mips -> Word -> Mips
build3MipsB m r1 r2 lb = "    "~~m~~" "~~(showReg r1) ~~","~~r2~~","~~(showTag lb)~~"\n"

buildiMips :: Mips -> Register -> Register -> Int -> Mips
buildiMips m r1 r2 c = "    "~~m~~" "~~(showReg r1) ~~","~~(showReg r2)~~", "~~(stt c)~~"\n"

buildfMips :: Mips -> Register -> Register -> Float -> Mips
buildfMips m r1 r2 c = "    "~~m~~" "~~(showfReg r1) ~~","~~(showfReg r2)~~", "~~(stt c)~~"\n"

partition :: Program -> Seq(Program)
partition program = build $ F.foldl includeInLast (S.empty,S.empty) program
    where includeInLast (prg,actualBlock) ins 
            | isTag     ins = (prg |> actualBlock,   S.empty |> ins)
            | isJump    ins = (prg |> (actualBlock |> ins), S.empty) 
            | otherwise     = (prg, actualBlock |> ins)
          build (blocks,ablock)
            | not (S.null ablock) = blocks |> ablock
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
                             , regfDescriptor :: Vector [Var]
                             , assembly       :: Mips } 
                     deriving (Show)

load :: Var -> Register -> Mips
load (Int_Cons   i) reg = "    li "   ~~ showReg reg ~~ "," ~~ stt i ~~ "\n" -- Constantes
load (Float_Cons f) reg = "    li.s f" ~~ showReg reg ~~ "," ~~ stt f ~~ "\n" -- Constantes
load _ _ = "" -- Globales

addOffset :: Int -> Maybe Int
addOffset a = Just (a+lowreg)

findVar :: Var -> Vector [Var] -> Maybe(Register)
findVar Fp  _ = Just fp
findVar var v = (findIndex (any (var==)) v) >>= addOffset 

findEmpty :: Vector [Var] -> Maybe(Register)
findEmpty v = (findIndex null v) >>= addOffset


-- -> Bool -- revisar si el reg no comparte lugar
getReg' :: Var -- Var to find register for
          -> Bool -- is in normal procesor? if false coprocessor
              -> MipsGenerator(Register)
getReg' Fp  b = return fp
getReg' var b = isInRegister
    where isInRegister = do
            varDesc <- gets varDescriptor -- could be float descriptor
            maybe searchEmpty
                  (\ (_,rL) ->  if null rL
                                then  error err1 >> return (-1)
                                else  return ((Prelude.head rL) + lowreg))
                  (lookup var varDesc)
          updateWith i = do 
              emit (load var (i+lowestRegister))
              (if b then updateRegDescriptors else updatefRegDescriptors) i  (const [var])
              updateVarDescriptors var (const ([var],[i]))
              r <- gets regDescriptor
              v <- gets varDescriptor
  
              liftIO(putStr "# ====")
              liftIO(putStrLn $ (show r) ++ "   " ++ show v)
              return (i+lowestRegister)
          searchEmpty = do 
              vectDesc <- if b then gets regDescriptor else gets regfDescriptor
              let(found,index) = F.foldl findVec (False,0) vectDesc
              if found
                 then updateWith index
                 else error err2 >> return (-1)
          findVec (True ,x) _  = (True,x)
          findVec (False,x) [] = if b then (True,x) else ( (x/=fReserved) && (x/=0) , x )
          findVec (False,x)  _ = (False,x+1)
          err1 = "Empty registers list for Ry"
          err2 = "No empty registers and no spills :( sorry Novich"
          lowestRegister = if b then lowreg else 0

getReg  = (flip getReg') True
getfReg = (flip getReg') False

killVar :: Var -> MipsGenerator()
killVar _ = undefined

createTemp :: Var -> Register -> MipsGenerator ()
createTemp = undefined


newRegisters :: Vector [Var]
newRegisters = replicate numRegs [] -- 23-8+1

newfRegisters :: Vector [Var]
newfRegisters = replicate numfRegs [] -- 23-8+1

initDescriptor :: Descriptor
initDescriptor = Descriptor newRegisters M.empty newfRegisters T.empty

type MipsGenerator = StateT Descriptor IO

-- Remove defitions 


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
    put state { regDescriptor = newRegisters, varDescriptor = M.empty, regfDescriptor = newfRegisters }

updateRegDescriptors :: Int -> ([Var] -> [Var]) -> MipsGenerator()
updateRegDescriptors reg func = do 
    registers <- gets regDescriptor
    state     <- get
    put state { regDescriptor = registers // [(reg,(func (registers ! reg)))]}
    -- liftIO $ putStrLn "update reg descriptors"

updatefRegDescriptors :: Int -> ([Var] -> [Var]) -> MipsGenerator()
updatefRegDescriptors reg func = do 
    registers <- gets regfDescriptor
    state     <- get
    put state { regfDescriptor = registers // [(reg,(func (registers ! reg)))]}

updateVarDescriptors :: Var -> (([Var],[Register]) -> ([Var],[Register])) -> MipsGenerator()
updateVarDescriptors var func = do 
    varsD  <- gets varDescriptor
    state  <- get

    let newDesc = if member var varsD
        then Data.Map.Strict.adjust func var varsD
        else insert var (func ([],[])) varsD
    -- liftIO $ putStrLn "update reg descriptors"
    put state { varDescriptor = newDesc }

emit :: Mips -> MipsGenerator()
emit code = do
    acc    <- gets assembly
    state  <- get
    put state { assembly = T.append acc code }

emiti :: Mips -> MipsGenerator()
emiti code = do
    acc    <- gets assembly
    state  <- get
    put state { assembly = T.append acc ("    "~~code) }


-- clearDescriptor :: MipsGenerator ()
-- clearDescriptor = put $ Descriptor newRegisters M.empty 

-- getReg :: [Var] -> MipsGenerator([Registers])
-- getReg (lval:rval:[])      = undefined
-- getReg (lval:exp1:exp2:[]) = undefined

compile :: Seq(Program) -> MipsGenerator()
compile ps = (mapM_ (\ b -> processBlock b >> clearDescriptor) ps) 

processBlock :: Program -> MipsGenerator ()
processBlock p = mapM_ processIns p

showTag :: Word -> Mips
showTag l = "_tag" ~~ (stt l)

processIns :: IntIns -> MipsGenerator ()
processIns ins = 
    case ins of 
      -- Aritmetic
      (Addi r1 r2 (Int_Cons c))  -> get2reg "addi" r1 r2 c
      (Subi r1 r2 (Int_Cons c))  -> get2reg "addi" r1 r2 (-c)
      -- (Divi r1 r2 (Int_Cons c))  -> return () -- get2reg "div" r1 r2 (-c)
      (Addi r1 (Int_Cons c) r2)  -> get2reg "addi" r1 r2 c
      (Subi r1 (Int_Cons c) r2)  -> get2reg "addi" r1 r2 (-c)
      -- (Divi r1 (Int_Cons c) r2)  -> return () -- get2reg "div" r1 r2 (-c)
      (Subi r1 r2 r3)            -> get3regs "sub" r1 r2 r3
      (Addi r1 r2 r3)            -> get3regs "add" r1 r2 r3
      (Divi r1 r2 r3)            -> get3regs "div" r1 r2 r3

      -- Float aritmetic
      (Addf r1 r2 (Float_Cons c))  -> get2regf "add.s" r1 r2 c
      (Subf r1 r2 (Float_Cons c))  -> get2regf "sub.s" r1 r2 c
      (Divf r1 r2 (Float_Cons c))  -> get2regf "div.s" r1 r2 c
      (Addf r1 (Float_Cons c) r2)  -> get2regf "add.s" r1 r2 c
      (Subf r1 (Float_Cons c) r2)  -> get2regf "sub.s" r1 r2 c
      (Divf r1 (Float_Cons c) r2)  -> get2regf "div.s" r1 r2 c
      (Subf r1 r2 r3)              -> get3regsf "sub.s" r1 r2 r3
      (Addf r1 r2 r3)              -> get3regsf "add.s" r1 r2 r3
      (Divf r1 r2 r3)              -> get3regsf "div.s" r1 r2 r3

      (Comment str)              -> if showComments then emit  ("# " ~~ (T.pack str) ~~ "\n") else return ()
      -- (Tag     0 )               -> emit $ "main:\n"
      (Tag     lb)               -> emit $ "_tag" ~~ (stt lb) ~~ ":\n"
      (TagS   str)               -> emit $ (T.pack str) ~~ ":\n"

      -- Jumps
      (Jump    lb)               -> emit $ "    j _tag" ~~ (stt lb) ~~ "\n"
      (JEq r1 (Int_Cons c) lb)  -> get1branch "beq" r1 (stt c)  lb
      (JEq (Int_Cons c) r1 lb)  -> get1branch "beq" r1 (stt c)  lb
      (JEq r1 r2 lb)            -> get2branch "beq" r1 r2       lb

      (JNEq r1 (Int_Cons c) lb)  -> get1branch "bne" r1 (stt c)  lb
      (JNEq (Int_Cons c) r1 lb)  -> get1branch "bne" r1 (stt c)  lb
      (JNEq r1 r2 lb)            -> get2branch "bne" r1 r2       lb

      (JLt r1 (Int_Cons c) lb)  -> get1branch "blt" r1 (stt c)  lb
      (JLt (Int_Cons c) r1 lb)  -> get1branch "blt" r1 (stt c)  lb
      (JLt r1 r2 lb)            -> get2branch "blt" r1 r2       lb

      (JGt r1 (Int_Cons c) lb)  -> get1branch "bgt" r1 (stt c)  lb
      (JGt (Int_Cons c) r1 lb)  -> get1branch "bgt" r1 (stt c)  lb
      (JGt r1 r2 lb)            -> get2branch "bgt" r1 r2       lb

      (JLEq r1 (Int_Cons c) lb)  -> get1branch "ble" r1 (stt c)  lb
      (JLEq (Int_Cons c) r1 lb)  -> get1branch "ble" r1 (stt c)  lb
      (JLEq r1 r2 lb)            -> get2branch "ble" r1 r2       lb

      (JGEq r1 (Int_Cons c) lb)  -> get1branch "bge" r1 (stt c)  lb
      (JGEq (Int_Cons c) r1 lb)  -> get1branch "bge" r1 (stt c)  lb
      (JGEq r1 r2 lb)            -> get2branch "bge" r1 r2       lb

      (Jz    r1 lb)  -> get1branch "beq" r1 (showReg 0)  lb
      (Jnotz r1 lb)  -> get1branch "bne" r1 (showReg 0)  lb

      -- Mem access
      (Mv           d (Int_Cons i)) -> mv d i 
      (Mv           d _ )           -> emit "#AQUI HAY UN MOVE"
      -- (ReadPointer  d i )    -> getReg d >>= (\r -> emiti$"lw "~~(showReg r)~~",0("~~(showReg magicReg)~~")\n")
      (ReadPointer  d r1)    -> getReg d >>= (\r -> getReg r1 >>= (\r1 -> emiti$"lw "~~(showReg r)~~",0("~~(showReg r1)~~")\n"))
      (StorePointer d r1)    -> storeInP d r1
      (ReadArray    d Fp (Int_Cons c)) -> readLocal d c
      (ReadArray    d r1 r2)           -> readArr d r1 r2
      -- (StoreArray   d r1 r2) -> 
      (Param      (Int_Cons   s) i)  -> moveSp (-i) >> emiti ("li $a3,"~~stt s~~"\n")     >> emiti "sw $a3,0($sp)\n"
      (Param      (Float_Cons s) i)  -> moveSp (-i) >> emiti ("li $a3,"~~stt s~~"\n")     >> emiti "sw $a3,0($sp)\n"
      (Param      (MemAdress  s) i)  -> moveSp (-i) >> emiti ("la $a3,_"~~T.pack s~~"\n") >> emiti "sw $a3,0($sp)\n"
      (Param      t0 i)              -> paramGen t0 i
      
      (ReturnE      s ) -> emiti ("j  "~~T.pack s~~"_epilogue\n")
      (ReturnS    a s ) -> getReg a >>= (\r -> emiti ("move $a3,"~~showReg r~~"\n") )>> emiti ("j "~~T.pack s~~"\n")
      (Save         i ) -> save i
      (SaveRet      i ) -> moveSp (-i)
      (Clean        i ) -> moveSp (i) -- revisar o nuevo
      (Epilogue     i ) -> emiti ("sw $a3,"~~stt i~~"($sp)\n") >> emiti "jr $ra\n"
      -- este i es k+i = tam de arg + tam de retorno
      (TACCall    str_lab      i k) -> saveRegs >> emiti ("jal "~~T.pack str_lab~~"\n") >> restoreRegs -- >>  emiti ("addi $sp,$sp," ~~ stt (i+k+8) ~~ "\n")  -- Potencialmente hacer algo con ese i
      (CallExp  dest  str_lab  i k) -> saveRegs >> emiti ("jal "~~T.pack str_lab~~"\n") -- >> restoreRegs >> getReg dest >>= (\ r -> emiti ("lw "~~ showReg r~~","~~stt (i+8)~~"($sp)\n")) >> emiti ("addi $sp,$sp," ~~ stt (i+k+8) ~~ "\n") -- Mover lo que se tenga a dest
      (Restore  dest  str_lab  i k) -> restoreRegs >> getReg dest >>= (\ r -> emiti ("lw "~~ showReg r~~","~~stt (i+8)~~"($sp)\n")) -- >> emiti ("addi $sp,$sp," ~~ stt (i+k+8) ~~ "\n") -- Mover lo que se tenga a dest
      TacExit                    -> emiti "li $v0,10\n" >> emiti "syscall\n"
      Nop                        -> emit "# nop\n">> restoreRegs
      a                  -> return ()
      -- este i es k+i = tam de arg + tam de retorno
      -- (Save         i ) -> moveSp (-i-8) >> emiti ("sw $fp,"~~ stt (i+4) ~~"($sp)\n") >> emiti ("sw $ra,"~~ stt i ~~"($sp)\n") >> emiti ("addi $fp,$sp,"~~ stt (i+8) ~~"\n")
      -- (Param      (Temp s))  -> moveSp (-4) >> emit ("la $t0,"~~T.pack s~~"\n") >> emit $ "    sw $t0,0($sp)" -- really? bueno, hay que buscar el registro
      -- (TagSC) tag para strings, usado en data, no aqui
    where get3regs str d r1 r2 = do 
            fstReg <- getReg r1
            sndReg <- getReg r2
            dReg   <- getReg d
            emit $ build3Mips str dReg fstReg sndReg 
          get2reg str d r1 cons = do 
            fstReg <- getReg r1 
            dest   <- getReg d 
            emit $ buildiMips str dest fstReg cons
          get3regsf str d r1 r2   = do
            fstReg <- getfReg r1
            sndReg <- getfReg r2
            dReg   <- getfReg d
            emit $ build3Mipsf str dReg fstReg sndReg 
          get2regf str d r1 cons  = do
            fstReg <- getfReg r1 
            dest   <- getfReg d 
            emit $ buildfMips str dest fstReg cons
          get1branch str r1 cons lab = do 
            -- fstReg <- findRegister r1 Nothing
            fstReg <- getReg r1
            -- liftIO $ putStrLn $ (show r1) ++ " está en " ++ show fstReg ++ " \n"
            emit $ build3MipsB str fstReg cons lab
          get2branch str r1 r2 lab = do 
            -- fstReg <- findRegister r1 Nothing
            -- sndReg <- findRegister r2 Nothing
            fstReg <- getReg r1
            sndReg <- getReg r2
            emit $ build3MipsB str fstReg (showReg sndReg) lab
          mv d i = do
            dest <- getReg d
            emiti $ "li " ~~ showReg dest ~~ ", " ~~ stt i
          storeInP d r1 = do
            op1   <- getReg r1
            dest  <- getReg d
            emiti $ "sw "~~ showReg op1~~",0("~~showReg dest~~")\n"
          readLocal d c = do
            dest  <- getReg d
            emiti $ "lw "~~showReg dest~~","~~stt c~~"("~~(showReg fp)~~")\n"
          readArr d r1 r2 = do
            dest  <- getReg d
            op1   <- getReg r1
            op2   <- getReg r2
            emiti $ "add $a3,"~~ showReg op1 ~~ "," ~~ showReg op2 ~~ "\n" 
            emiti $ "lw "~~showReg dest~~",0($a3)\n"
          paramGen t0 i = do
            moveSp (-i)
            -- tomar en cuenta arreglos
            source  <- getReg t0
            emiti $ "sw "~~ showReg source ~~",0($sp)\n"
          save i = do 
            moveSp (-i-8)
            emiti $ "sw $fp,"~~ stt (i+4) ~~"($sp)\n"
            emiti $ "sw $ra,"~~ stt i ~~"($sp)\n"
            emiti $ "addi $fp,$sp,"~~ stt (i+8) ~~"\n"
          saveRegs = do 
            moveSp (-8)
            emiti ("sw $fp,0($sp)\n")
            emiti ("sw $ra,4($sp)\n")
            emiti ("addi $fp,$sp,8\n")
          restoreRegs = emiti ("lw $fp,0($sp)") >> emiti ("lw $fp,4($sp)")

          moveSp n = emiti $ "addi $sp,$sp," ~~ stt n ~~ "\n"
          moveFp n = emiti $ "addi $fp,$fp," ~~ stt n ~~ "\n"




runCompiler =  runStateT

-- compilerState = evalStateT


-- loads, stores, array, separados

-- findRegister var v = maybe (maybe (error "no hay spills") id (findEmpty v))
--                            id
--                            (findVar var v)
