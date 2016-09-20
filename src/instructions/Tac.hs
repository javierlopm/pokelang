{-# LANGUAGE DeriveGeneric #-}

module Tac(
    IntIns(..),
    Memory(..),
    Program,
    pComment,
    showP,
    saveProgram,
    loadProgram,
    backPatch
) where 

import Prelude hiding(foldr)
import Data.Sequence
import Data.Foldable(foldr)
import Data.Binary as B(get)
import Data.Binary hiding(get)
import Control.Monad(liftM2,liftM3)
import Data.ByteString.Lazy as Wf(writeFile)
--import System.Directory(removeFile)

{- Memory access -}
data Memory = MemIndexR      Int Int    --   0(R0) 0 + contents(R0)
            | MemIndex       String Int --  lb(R0) lb + contents(R0)
            | MemIndirIndex  String Int -- *lb(R0) contents(lb + contents(R0))
            | MemIndirIndexR Int Int    --  *0(R1) contents( 0 + contents(R1))

instance Show Memory where
    show (MemIndexR      o  r0) = show  o ++ "(R" ++ show r0 ++ ")"
    show (MemIndex       lb r0) =       lb ++ "(R" ++ show r0 ++ ")"
    show (MemIndirIndex  lb r0) = '*' :      lb ++ "(R" ++ show r0 ++ ")"
    show (MemIndirIndexR o  r0) = '*' : show  o ++ "(R" ++ show r0 ++ ")"

instance Binary Memory where
    put (MemIndexR      o  r0) = putWord8 0 >> put o  >> put r0
    put (MemIndirIndexR o  r0) = putWord8 1 >> put o  >> put r0 
    put (MemIndex       lb r0) = putWord8 2 >> put lb >> put r0
    put (MemIndirIndex  lb r0) = putWord8 3 >> put lb >> put r0

    get = do key <- getWord8
             case key of
                0 ->  build2get MemIndexR     
                1 ->  build2get MemIndirIndexR
                2 ->  build2get MemIndex      
                3 ->  build2get MemIndirIndex 

{- Intermediate machine -}
data IntIns = -- Dest Src1 Src2  -  Reg,Reg,Reg
              FlMult   Int Int Int  -- Float
            | FlAdd    Int Int Int
            | FlSub    Int Int Int
            | FlDiv    Int Int Int
            | IntMul   Int Int Int  -- Integers
            | IntAdd   Int Int Int
            | IntSub   Int Int Int
            | IntDiv   Int Int Int
            | And      Int Int Int  -- Generic logic bitwise operations
            | Or       Int Int Int
            | XOr      Int Int Int
            | Eql      Int Int Int
            | NotEql   Int Int Int
            | Not      Int Int
            | ShiftL   Int Int Int -- Reg - Reg - Int
            | ShiftR   Int Int Int -- Reg - Reg - Int
            -- Compare integers
            | Lt      Int Int Int  -- lower
            | Gt      Int Int Int  -- greater
            | LEq     Int Int Int  -- lower or equal
            | GEq     Int Int Int  -- equal 
            -- Jumps - Registers label
            | Jump     (Maybe(Int))
            | Jz       Int (Maybe(Int))
            | Jnotz    Int (Maybe(Int))
            | JLt      Int Int (Maybe(Int))
            | JGt      Int Int (Maybe(Int))
            | JLEq     Int Int (Maybe(Int))
            | JGEq     Int Int (Maybe(Int))
            -- Const Int Oper Dest Src Cons
            | Addi     Int Int Int -- Operations between register and  constants
            | Subi     Int Int Int 
            | Multi    Int Int Int
            | Divi     Int Int Int
            -- Const Float Oper Dest Src Cons
            | Addf     Int Int Float -- Operations between register and  constants
            | Subf     Int Int Float 
            | Multf    Int Int Float
            | Divf     Int Int Float
            -- Src Dest
            | Load     Int Memory -- Loading oper
            | Store    Memory Int
            | Mv       Int    Int
            -- Load long value into registers
            | Loadi    Int Int
            -- Function calls
            | Call    String -- Call function from label STRING OR MAYBE INT?
            | Param   Int    -- Push for calling
            -- Extras
            | Comment String
            | Tag     (Maybe(Int))
            | Nop 
            -- Prints
            | Printic   Int    -- Print Integer constant
            | Printi    Int    -- Print integer inside register
            | Printfc   Float  -- Print float constant
            | Printflr  Int    -- Print float inside register
            | PrintStr  Int    -- Print string pointed from 
            | PrintEnum Memory -- Print enum at label
            -- Do we need this?
            -- | AccessArray R0     := R1[R2]
            -- | StoreArray  R3[R4] := R5
            -- PROCEDURES TO BE IMPLEMENTED IN MIPS
            -- Missing conversionFunctions
            -- Floor
            -- Ceil
            -- IntToChar
            -- CharToInt
            -- IntToFloat
            -- Memory Operations
            -- Free   
            -- Malloc 
            -- Enum operations Src Dest - Reg Reg
            -- Succ  Int Int --  Stores en r0, sucessor of enum pointed by R1
            -- Prec  Int Int 
            

instance Show      IntIns where
    show (FlMult   r0 r1 r2)  =  showTAC r0 r1 "f*" r2
    show (FlAdd    r0 r1 r2)  =  showTAC r0 r1 "f+" r2
    show (FlSub    r0 r1 r2)  =  showTAC r0 r1 "f-" r2
    show (FlDiv    r0 r1 r2)  =  showTAC r0 r1 "f/" r2
    show (IntMul   r0 r1 r2)  =  showTAC r0 r1 "*"  r2
    show (IntAdd   r0 r1 r2)  =  showTAC r0 r1 "+"  r2
    show (IntSub   r0 r1 r2)  =  showTAC r0 r1 "-"  r2
    show (IntDiv   r0 r1 r2)  =  showTAC r0 r1 "/"  r2
    show (And      r0 r1 r2)  =  showTAC r0 r1 "&"  r2
    show (Or       r0 r1 r2)  =  showTAC r0 r1 "|"  r2
    show (XOr      r0 r1 r2)  =  showTAC r0 r1 "XOr"r2
    show (Not      r0 r1   )  =  show2AC r0 r1 "~" 
    show (Eql      r0 r1 r2)  =  showTAC r0 r1 "="  r2
    show (NotEql   r0 r1 r2)  =  showTAC r0 r1 "/=" r2
    show (Lt       r0 r1 r2)  =  showTAC r0 r1 "<"  r2
    show (Gt       r0 r1 r2)  =  showTAC r0 r1 ">"  r2
    show (LEq      r0 r1 r2)  =  showTAC r0 r1 "<=" r2
    show (GEq      r0 r1 r2)  =  showTAC r0 r1 ">=" r2
    show (Jump     i    )   = "goto " ++ showJust i
    show (Jz       r0 i )   = "if R" ++ show r0 ++ "z  goto" ++ showJust i 
    show (Jnotz    r0 i )   = "if R" ++ show r0 ++ "nz goto" ++ showJust i 
    show (JLt      r0 r1 i) = showIf r0 r1 "<"   i
    show (JGt      r0 r1 i) = showIf r0 r1 ">"   i
    show (JLEq     r0 r1 i) = showIf r0 r1 "<="  i
    show (JGEq     r0 r1 i) = showIf r0 r1 ">="  i
    show (Addi     r0 r1 i)   = showTACi r0 r1 "+"  i
    show (Subi     r0 r1 i)   = showTACi r0 r1 "-"  i
    show (Multi    r0 r1 i)   = showTACi r0 r1 "*"  i
    show (Divi     r0 r1 i)   = showTACi r0 r1 "/"  i
    show (Addf     r0 r1 f)   = showTACi r0 r1 "f+" f
    show (Subf     r0 r1 f)   = showTACi r0 r1 "f-" f
    show (Multf    r0 r1 f)   = showTACi r0 r1 "f*" f
    show (Divf     r0 r1 f)   = showTACi r0 r1 "f/" f
    show (Mv       r0 r1  )   = "MV R" ++ show r0 ++ " R" ++ show r1
    show (Load     r0 m)      = "LD R" ++ show r0 ++ " " ++ show m
    show (Store    m r0)      = "ST "  ++ show m  ++ " R" ++ show r0
    show (Loadi    r0 c)      = "LDI R" ++ show r0 ++ " #" ++show c
    show (Call     str )      = "CALL " ++ str
    show (Param    par )      = "PARAM R" ++ show par
    show (Tag      i   )      = '\n': (showJust i) ++ ":"
    show (Comment  str )      = ';': str
    show (Printic   c  )      = "PRINT INT CONST "   ++ show c
    show (Printi    r0 )      = "PRINT INT R"        ++ show r0
    show (Printfc   fc )      = "PRINT FLOAT CONST " ++ show fc
    show (Printflr  fr )      = "PRINT FLOAT R"      ++ show fr
    show (PrintStr  r0 )      = "PRINT STR at *R"    ++ show r0
    show (PrintEnum lb )      = "PRINT ENUM at "     ++ show lb
    show (ShiftL r0 r1 i)     = showTACi r0 r1 "<<" i
    show (ShiftR r0 r1 i)     = showTACi r0 r1 ">>" i
    show Nop                  = "NoOp"

-- Ewwwww, it might be improved with Generics?
instance Binary IntIns where
    put  Nop                 = putWord8 0 
    put (FlMult   r0 r1 r2)  = putWord8 1  >> put r0 >> put r1 >> put r2
    put (FlAdd    r0 r1 r2)  = putWord8 2  >> put r0 >> put r1 >> put r2
    put (FlSub    r0 r1 r2)  = putWord8 3  >> put r0 >> put r1 >> put r2
    put (FlDiv    r0 r1 r2)  = putWord8 4  >> put r0 >> put r1 >> put r2
    put (IntMul   r0 r1 r2)  = putWord8 5  >> put r0 >> put r1 >> put r2
    put (IntAdd   r0 r1 r2)  = putWord8 6  >> put r0 >> put r1 >> put r2
    put (IntSub   r0 r1 r2)  = putWord8 7  >> put r0 >> put r1 >> put r2
    put (IntDiv   r0 r1 r2)  = putWord8 8  >> put r0 >> put r1 >> put r2
    put (And      r0 r1 r2)  = putWord8 9  >> put r0 >> put r1 >> put r2
    put (Or       r0 r1 r2)  = putWord8 10 >> put r0 >> put r1 >> put r2
    put (XOr      r0 r1 r2)  = putWord8 11 >> put r0 >> put r1 >> put r2
    put (Not      r0 r1   )  = putWord8 12 >> put r0 >> put r1 
    put (Eql      r0 r1 r2)  = putWord8 13 >> put r0 >> put r1 >> put r2
    put (NotEql   r0 r1 r2)  = putWord8 14 >> put r0 >> put r1 >> put r2
    put (Lt       r0 r1 r2)  = putWord8 15 >> put r0 >> put r1 >> put r2
    put (Gt       r0 r1 r2)  = putWord8 16 >> put r0 >> put r1 >> put r2
    put (LEq      r0 r1 r2)  = putWord8 17 >> put r0 >> put r1 >> put r2
    put (GEq      r0 r1 r2)  = putWord8 18 >> put r0 >> put r1 >> put r2
    put (Jump     str    )   = putWord8 19 >> put str
    put (Jz       r0 str )   = putWord8 20 >> put r0 >>  put str
    put (Jnotz    r0 str )   = putWord8 21 >> put r0 >>  put str
    put (JLt      r0 r1 str) = putWord8 22 >> put r0 >>  put r1 >> put str
    put (JGt      r0 r1 str) = putWord8 23 >> put r0 >>  put r1 >> put str
    put (JLEq     r0 r1 str) = putWord8 24 >> put r0 >>  put r1 >> put str
    put (JGEq     r0 r1 str) = putWord8 25 >> put r0 >>  put r1 >> put str 
    put (Addi     r0 r1 i)   = putWord8 26 >> put r0 >>  put r1 >> put i
    put (Subi     r0 r1 i)   = putWord8 27 >> put r0 >>  put r1 >> put i
    put (Multi    r0 r1 i)   = putWord8 28 >> put r0 >>  put r1 >> put i
    put (Divi     r0 r1 i)   = putWord8 29 >> put r0 >>  put r1 >> put i
    put (Addf     r0 r1 i)   = putWord8 30 >> put r0 >>  put r1 >> put i
    put (Subf     r0 r1 i)   = putWord8 31 >> put r0 >>  put r1 >> put i
    put (Multf    r0 r1 i)   = putWord8 32 >> put r0 >>  put r1 >> put i
    put (Divf     r0 r1 i)   = putWord8 33 >> put r0 >>  put r1 >> put i
    put (Mv       r0 r1  )   = putWord8 34 >> put r0 >>  put r1 
    put (Load     r0 m)      = putWord8 35 >> put r0 >>  put m
    put (Store    m r0)      = putWord8 36 >> put m  >>  put r0
    put (Loadi    r0 c)      = putWord8 37 >> put r0 >> put c
    put (Call     str )      = putWord8 38 >> put str
    put (Param    par )      = putWord8 39 >> put par
    put (Comment  str )      = putWord8 40 >> put str
    put (Tag      str )      = putWord8 41 >> put str
    put (Printic   c  )      = putWord8 42 >> put c
    put (Printi    r0 )      = putWord8 43 >> put r0
    put (Printfc   fc )      = putWord8 44 >> put fc
    put (Printflr  fr )      = putWord8 45 >> put fr
    put (PrintStr  r0 )      = putWord8 46 >> put r0
    put (PrintEnum lb )      = putWord8 47 >> put lb
    put (ShiftL r0 r1 i)     = putWord8 48 >> put r0 >> put r1 >> put i
    put (ShiftR r0 r1 i)     = putWord8 49 >> put r0 >> put r1 >> put i

    get = do 
    key <- getWord8
    case key of
       0  ->  return    Nop
       1  ->  buildTac  FlMult
       2  ->  buildTac  FlAdd
       3  ->  buildTac  FlSub
       4  ->  buildTac  FlDiv
       5  ->  buildTac  IntMul
       6  ->  buildTac  IntAdd
       7  ->  buildTac  IntSub
       8  ->  buildTac  IntDiv
       9  ->  buildTac  And
       10 ->  buildTac  Or
       11 ->  buildTac  XOr
       12 ->  build2get Not
       13 ->  buildTac  Eql
       14 ->  buildTac  NotEql
       15 ->  buildTac  Lt
       16 ->  buildTac  Gt
       17 ->  buildTac  LEq
       18 ->  buildTac  GEq
       19 ->  B.get >>= return .  Jump
       20 ->  build2get Jz
       21 ->  build2get Jnotz
       22 ->  buildTac  JLt
       23 ->  buildTac  JGt
       24 ->  buildTac  JLEq
       25 ->  buildTac  JGEq
       26 ->  buildTac  Addi
       27 ->  buildTac  Subi
       28 ->  buildTac  Multi
       29 ->  buildTac  Divi
       30 ->  buildTac  Addf
       31 ->  buildTac  Subf
       32 ->  buildTac  Multf
       33 ->  buildTac  Divf
       34 ->  build2get Mv
       35 ->  build2get Load
       36 ->  build2get Store
       37 ->  build2get Loadi
       38 ->  B.get >>= return . Call
       39 ->  B.get >>= return . Param
       40 ->  B.get >>= return . Comment
       41 ->  B.get >>= return . Tag
       42 ->  B.get >>= return . Printic   
       43 ->  B.get >>= return . Printi    
       44 ->  B.get >>= return . Printfc   
       45 ->  B.get >>= return . Printflr  
       46 ->  B.get >>= return . PrintStr  
       47 ->  B.get >>= return . PrintEnum 
       48 ->  buildTac ShiftL 
       49 ->  buildTac ShiftR 

-- Print auxiliaries
shwAsgn  int        = "R" ++ show int ++ ":="
showTAC  r0 r1 s r2 = 'R' : show r0 ++ " := R" ++ show r1 ++ " " ++ s ++ " R" ++ show r2
showTACi r0 r1 s i  = 'R' : show r0 ++ " := R" ++ show r1 ++ " " ++ s ++ " #" ++ show i
show2AC  r0 r1 s    = 'R' : show r0 ++ " := "  ++ s ++" R" ++  show r1
showIf r0 r1 s labl = "if R" ++ show r0 ++ " " ++ s ++ " R" ++ show r1 ++ " goto " ++ showJust labl
showJust (Just i) = "tag_" ++ show i
showJust Nothing  = "___"

-- Monad auxiliaries for put functions
build2get :: (Binary a1, Binary a2) => (a1 -> a2 -> r) -> Get r
build2get  constructor = liftM2 constructor B.get B.get

buildTac::(Binary a1, Binary a2, Binary a3) => (a1 -> a2 -> a3 -> r) -> Get r
buildTac constructor = liftM3 constructor B.get B.get B.get

-- Intermediate Instruction helpers
pComment :: Int -> String
pComment l = "Found at line" ++ show l

isTag :: IntIns -> Bool
isTag (Tag a) = True
isTag _       = False

-- Program as a sequence of instructions
type Program = Seq IntIns

-- Prety print
showP :: Program -> String
showP = foldr mapCon ""
    where mapCon ins base= (if isTag ins then""else"    ")++show ins++"\n"++base


patchI :: IntIns -> Int -> IntIns
patchI (Jump   Nothing)       i = (Jump   (Just i))
patchI (Tag    Nothing)       i = (Tag    (Just i))
patchI (Jz     r0 Nothing)    i = (Jz    r0 (Just i))
patchI (Jnotz  r0 Nothing)    i = (Jnotz r0 (Just i))
patchI (JLt    r0 r1 Nothing) i = (JLt   r0 r1 (Just i))
patchI (JGt    r0 r1 Nothing) i = (JGt   r0 r1 (Just i))
patchI (JLEq   r0 r1 Nothing) i = (JLEq  r0 r1 (Just i))
patchI (JGEq   r0 r1 Nothing) i = (JGEq  r0 r1 (Just i))
patchI a _ = a

backPatch :: Program -> Int -> Program
backPatch p lb = fmap (\ ins -> patchI ins lb ) p 

programExample :: Program
programExample = fromList [ Nop ,(FlMult   1 3 0),(FlAdd 2 4  1) ,(FlSub 3  5  2) ,(FlDiv    4  6  3) ,(IntMul   5  7  4) ,(IntAdd   6  8  5) ,(IntSub   7  9  6) ,(IntDiv   8  10 7) ,(And      9  11 8) ,(Or       10 12 9),(Tag Nothing) ,(XOr      11 13 9) ,(Not      12 14   ) ,(Eql      13 15 20), (Tag (Just 5)) ,(NotEql   14 16 21) ,(Lt       15 17 22) ,(Gt       16 18 23) ,(LEq      17 19 24) ,(GEq      18 20 25) ,(Jump     (Just 1)  )  ,(Jz       50 (Just 2) )  ,(Jnotz    51 (Just 3) )  ,(JLt      52 90 (Just 5)),(JGt      53 91 (Just 6)),(JLEq     54 92 Nothing),(JGEq     55 93 Nothing),(Addi     56 94 42)  ,(Subi     57 95 42)  ,(Multi    58 96 45)  ,(Divi     59 97 100)  ,(Addf     60 98 54)  ,(Subf     61 99 0.3)  ,(Multf    62 100 0.5)  ,(Divf     63 101 42.0)  ,(Mv       64 102  )  ,(Load     42 (MemIndex "Tail" 64) )     ,(Store  (MemIndirIndexR 42 30) 20)     ,(Loadi    0 42)     ,(Call     "fibo" )     ,(Param    59 )     ,(Comment  (pComment 59) ) ]

-- Binary store
saveProgram :: FilePath -> Program -> IO()
saveProgram = encodeFile

-- Binary load
loadProgram :: FilePath -> IO(Program)
loadProgram fp = do res <- decodeFile fp
                    --removeFile fb
                    return res

