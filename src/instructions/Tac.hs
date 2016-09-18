{-# LANGUAGE DeriveGeneric #-}

module Tac(
    IntIns(..),
    Memory
) where 

import Data.Sequence
import Data.Binary
import Control.Monad(liftM2,liftM3)

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
                0 ->  buildMem MemIndexR     
                1 ->  buildMem MemIndirIndexR
                2 ->  buildMem MemIndex      
                3 ->  buildMem MemIndirIndex 

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
            -- Compare integers
            | Lt      Int Int Int  -- lower
            | Gt      Int Int Int  -- greater
            | LEq     Int Int Int  -- lower or equal
            | GEq     Int Int Int  -- equal 
            -- Jumps - Registers label
            | Jump     String
            | Jz       Int String
            | Jnotz    Int String
            | JLt      Int Int String
            | JGt      Int Int String
            | JLEq     Int Int String
            | JGEq     Int Int String
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
            | Call    String -- Call function from label
            | Param   Int    -- Push for calling
            -- Extras
            | Comment String
            | Nop 

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
    show (Jump     str    )   = "goto " ++ str
    show (Jz       r0 str )   = "if R" ++ show r0 ++ "z  goto" ++ str 
    show (Jnotz    r0 str )   = "if R" ++ show r0 ++ "nz goto" ++ str 
    show (JLt      r0 r1 str) = showIf r0 r1 "<"  str
    show (JGt      r0 r1 str) = showIf r0 r1 ">"  str
    show (JLEq     r0 r1 str) = showIf r0 r1 "<=" str
    show (JGEq     r0 r1 str) = showIf r0 r1 ">=" str
    show (Addi     r0 r1 i)   = showTAC r0 r1 "+ #"  i
    show (Subi     r0 r1 i)   = showTAC r0 r1 "- #"  i
    show (Multi    r0 r1 i)   = showTAC r0 r1 "* #"  i
    show (Divi     r0 r1 i)   = showTAC r0 r1 "/ #"  i
    show (Addf     r0 r1 f)   = showTAC r0 r1 "f+ #" f
    show (Subf     r0 r1 f)   = showTAC r0 r1 "f- #" f
    show (Multf    r0 r1 f)   = showTAC r0 r1 "f* #" f
    show (Divf     r0 r1 f)   = showTAC r0 r1 "f/ #" f
    show (Mv       r0 r1  )   = "MV R" ++ show r0 ++ " R" ++ show r1
    show (Load     r0 m)      = "LD R" ++ show r0 ++ " " ++ show m
    show (Store    m r0)      = "ST "  ++ show m  ++ " R" ++ show r0
    show (Loadi    r0 c)      = "LDI R" ++ show r0 ++ " #" ++show c
    show (Call     str )      = "CALL " ++ str
    show (Param    par )      = "PARAM R" ++ show par
    show (Comment  str )      = ';': str
    show Nop                  = "NoOp"

-- Ewwwww, it might be improved with Generics
instance Binary IntIns where
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
    put (Jz       r0 str )   = putWord8 20 >> put r0 >> put str
    put (Jnotz    r0 str )   = putWord8 21 >> put r0 >> put str
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
    put Nop                  = putWord8 41 

-- Print auxiliaries
shwAsgn  int = "R" ++ show int ++ ":="
showTAC  r0 r1 s r2 = 'R' : show r0 ++ " := R" ++ show r1 ++ " " ++ s ++ " R" ++ show r2
showTACi r0 r1 s i  = 'R' : show r0 ++ " := R" ++ show r1 ++ " " ++ s ++ " #" ++ show i
show2AC  r0 r1 s    = 'R' : show r0 ++ " := "  ++ s ++" R" ++  show r1
showIf r0 r1 s label = "if R" ++ show r0 ++ " " ++ s ++ " R" ++ show r1 ++ " goto " ++ show label

-- Monad auxiliaries for put functions
buildMem ::(Binary a1, Binary a2) => (a1 -> a2 -> r) -> Get r
buildMem constructor = liftM2 constructor get get

buildTac::(Binary a1, Binary a2, Binary a3) => (a1 -> a2 -> a3 -> r) -> Get r
buildTac constructor = liftM3 constructor get get get

type Program = Seq IntIns