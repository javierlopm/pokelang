module Tac(
    IntIns(..)
) where 

data Memory = MemDir       Int      -- Plain memory direction
            | MemOffs      Int Int  -- Memory with offset
            | MemIndir     Int      -- Pointer Follow
            | MemIndirOffs Int Int  -- Follow pointer, then add offset
            | MemLabel     String   -- Data label 
            | MemLabelOffs Int String  -- Add offset to data label

instance Show Memory where
    show (MemDir       m    ) = show m
    show (MemOffs      m o  ) = '(' : (show o) ++ ')' : show m 
    show (MemIndir     m    ) = '*' : show m
    show (MemIndirOffs m o  ) = '*' : '(' : show o ++ ')' : show m 
    show (MemLabel     str  ) = str 
    show (MemLabelOffs o str) = "(" ++ show o ++ ")" ++ str 

newtype ConsVal = Cons Int

instance Show ConsVal where
    show (Cons a) = '#' : show a 

            -- Dest Src1 Src2  -  Reg,Reg,Reg
data IntIns = FlMult   Int Int Int  -- Float
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
            -- Load long value into registers
            | Loadi    Int ConsVal
            -- Function calls
            | Call  String -- Call function from label
            | Param Int    -- Push for calling

instance Show IntIns where
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
    show (Jz       r0 str )   = "if $" ++ show r0 ++ "z  goto" ++ str 
    show (Jnotz    r0 str )   = "if $" ++ show r0 ++ "nz goto" ++ str 
    show (JLt      r0 r1 str) = showIf r0 r1 "<"  str
    show (JGt      r0 r1 str) = showIf r0 r1 ">"  str
    show (JLEq     r0 r1 str) = showIf r0 r1 "<=" str
    show (JGEq     r0 r1 str) = showIf r0 r1 ">=" str
    show (Addi     r0 r1 i) = showTAC r0 r1 "+" i
    show (Subi     r0 r1 i) = showTAC r0 r1 "-" i
    show (Multi    r0 r1 i) = showTAC r0 r1 "*" i
    show (Divi     r0 r1 i) = showTAC r0 r1 "/" i
    show (Addf     r0 r1 f) = showTAC r0 r1 "f+" f
    show (Subf     r0 r1 f) = showTAC r0 r1 "f-" f
    show (Multf    r0 r1 f) = showTAC r0 r1 "f*" f
    show (Divf     r0 r1 f) = showTAC r0 r1 "f/" f
    show (Load     r0 m) = "LD $" ++ show r0 ++ " " ++ show m
    show (Store    m r0) = "ST "  ++ show m  ++ " $" ++ show r0
    show (Loadi    r0 c) = "LDI $" ++ show r0 ++ show c
    show (Call     str ) = "CALL " ++ str
    show (Param    par ) = "PARAM$" ++ show par

shwAsgn int = "$" ++ show int ++ ":="

showTAC  r0 r1 s r2 = '$' : show r0 ++ " := $" ++ show r1 ++ " " ++ s ++ " $" ++ show r2
showTACi r0 r1 s i  = '$' : show r0 ++ " := $" ++ show r1 ++ " " ++ s ++ " #" ++ show i
show2AC  r0 r1 s    = '$' : show r0 ++ " := "  ++ s ++" $" ++  show r1

showIf r0 r1 s label = "if $" ++ show r0 ++ " " ++ s ++ " $" ++ show r1 ++ " goto " ++ show label