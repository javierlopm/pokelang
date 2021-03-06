module Tac(
    IntIns(..),
    Var(..),
    Program,
    Label,
    pComment,
    showP,
    saveProgram,
    loadProgram,
    jumpTrueFalse,
    isCons,
    isTag,
    isJump,
    getCons,
    regNeeded
) where 

import Prelude hiding(foldr)
import Data.Word
import Data.Sequence
import Data.Foldable(foldr)
import Data.Binary as B(get)
import Data.Binary hiding(get)
import Control.Monad(liftM2,liftM3)
import Data.ByteString.Lazy as Wf(writeFile)

data Var = Int_Cons   Int
         | Float_Cons Float
         | MemAdress  String
         | Temp       Word   -- Non negative: t0, t1 ..
         | Fp   -- Frame pointer
         deriving(Eq,Ord)

getCons :: Var -> Int
getCons (Int_Cons   c) = c
-- getCons (Float_Cons c) = c
getCons _              = error "cannot get constant at Var"

instance Show Var where
    show (Int_Cons   i  ) = "#"  ++ show i
    show (Float_Cons f  ) = "f#" ++ show f
    show (MemAdress  lb ) = lb
    show (Temp       t0 ) = "t"  ++ show t0
    show Fp = "FP"

instance Binary Var where
    put (Int_Cons   i  ) = putWord8 0 >> put i 
    put (Float_Cons f  ) = putWord8 1 >> put f 
    put (MemAdress  lb ) = putWord8 2 >> put lb
    put (Temp       t0 ) = putWord8 3 >> put t0
    put  Fp              = putWord8 4

    get = do key <- getWord8
             case key of
                0 ->  B.get >>= return . Int_Cons     
                1 ->  B.get >>= return . Float_Cons
                2 ->  B.get >>= return . MemAdress      
                3 ->  B.get >>= return . Temp 
                4 ->  return Fp

regNeeded :: Var -> Int
regNeeded (Int_Cons   _) = 0
regNeeded (Float_Cons _) = 0
regNeeded (MemAdress  _) = 1
regNeeded (Temp       _) = 1
regNeeded Fp             = 0

type Src1  = Var 
type Src2  = Var
type Dest  = Var
type Label = Word -- Non-negative tags: tag_0, tag_1, tag_2... 

{- Intermediate machine -}
data IntIns = Addi     Dest Src1 Src2 -- Aritmetic Operations over Ints
            | Subi     Dest Src1 Src2
            | Divi     Dest Src1 Src2
            | Mod      Dest Src1 Src2
            | Multi    Dest Src1 Src2
            | Pot      Dest Src1 Src2
            | Negai    Dest Src1
            -- Float Operations over Ints
            | Addf     Dest Src1 Src2 
            | Subf     Dest Src1 Src2
            | Divf     Dest Src1 Src2
            | Multf    Dest Src1 Src2
            | Negaf    Dest Src1
            -- Logic bitwise operations
            | And      Dest Src1 Src2  
            | Or       Dest Src1 Src2
            | XOr      Dest Src1 Src2
            | Eql      Dest Src1 Src2
            | NotEql   Dest Src1 Src2
            | Not      Dest Src1
            | ShiftL   Dest Src1 Word -- shifts are always postive constants
            | ShiftR   Dest Src1 Word 
            -- Compare integers
            | Lt      Dest Src1 Src2
            | Gt      Dest Src1 Src2
            | LEq     Dest Src1 Src2  -- lower or equal
            | GEq     Dest Src1 Src2  -- equal 
            -- Jumps
            | Jump     Label
            | Jz       Src1 Label
            | Jnotz    Src1 Label
            | JLt      Src1 Src2 Label
            | JGt      Src1 Src2 Label
            | JLEq     Src1 Src2 Label
            | JGEq     Src1 Src2 Label
            | JEq      Src1 Src2 Label
            | JNEq     Src1 Src2 Label
            -- Copy / Read / Store
            | Mv           Dest Src1
            | ReadPointer  Dest Src1
            | StorePointer Dest Src1
            | ReadArray    Dest Src1 Src2
            | StoreArray   Dest Src1 Src2
            -- Function calls
            | TACCall    String  Int Int
            | CallExp    Dest String  Int Int
            | Restore    Dest String  Int Int
            | Clean   Int
            | Epilogue   Int Int
            | Prologue
            | Param   Src1 Int    
            | ReturnE  String
            | ReturnS  Src1 String
            -- Extras
            | Comment String
            | Tag     Label
            | TagS    String
            | TagSC   String String
            | Nop 
            | TacExit 
            -- Prints
            | Print     Src1        -- Print Integer constant
            | PrintEnum String Src1 -- Print enum at label
            -- save
            | Save Int -- save on stack?
            | SaveRet Int -- save on stack?
            | IniFp Int -- save on stack?
            
instance Show      IntIns where
    show (Addi    r0 r1 r2)  =  showTAC r0 r1 "+" r2
    show (Subi    r0 r1 r2)  =  showTAC r0 r1 "-" r2
    show (Divi    r0 r1 r2)  =  showTAC r0 r1 "/" r2
    show (Multi   r0 r1 r2)  =  showTAC r0 r1 "*"  r2
    show (Addf    r0 r1 r2)  =  showTAC r0 r1 "f+" r2
    show (Subf    r0 r1 r2)  =  showTAC r0 r1 "f-" r2
    show (Divf    r0 r1 r2)  =  showTAC r0 r1 "f/" r2
    show (Multf   r0 r1 r2)  =  showTAC r0 r1 "f*"  r2
    show (Pot    r0 r1 r2)  =  showTAC r0 r1 "^"  r2
    show (And    r0 r1 r2)  =  showTAC r0 r1 "&"  r2
    show (Or     r0 r1 r2)  =  showTAC r0 r1 "|"  r2
    show (XOr    r0 r1 r2)  =  showTAC r0 r1 "XOr"r2
    show (Not    r0 r1   )  =  show2AC r0 r1 "~" 
    show (Negai  r0 r1   )  =  show2AC r0 r1 "-" 
    show (Negaf  r0 r1   )  =  show2AC r0 r1 "-f" 
    show (Eql    r0 r1 r2)  =  showTAC r0 r1 "="  r2
    show (NotEql r0 r1 r2)  =  showTAC r0 r1 "/=" r2
    show (ShiftL r0 r1 i)   = showTACi r0 r1 "<<" i
    show (ShiftR r0 r1 i)   = showTACi r0 r1 ">>" i
    show (Lt     r0 r1 r2)  =  showTAC r0 r1 "<"  r2
    show (Gt     r0 r1 r2)  =  showTAC r0 r1 ">"  r2
    show (LEq    r0 r1 r2)  =  showTAC r0 r1 "<=" r2
    show (GEq    r0 r1 r2)  =  showTAC r0 r1 ">=" r2
    show (Jump   i    )     = "goto tag_" ++ show i
    show (Jz     r0 i )     = "if " ++ show r0 ++ " is z  goto tag_" ++ show i 
    show (Jnotz  r0 i )     = "if " ++ show r0 ++ " is nz goto tag_" ++ show i 
    show (JLt    r0 r1 i)   = showIf r0 r1 "<"   i
    show (JGt    r0 r1 i)   = showIf r0 r1 ">"   i
    show (JLEq   r0 r1 i)   = showIf r0 r1 "<="  i
    show (JGEq   r0 r1 i)   = showIf r0 r1 ">="  i
    show (JEq    r0 r1 i)   = showIf r0 r1 "=="  i
    show (JNEq   r0 r1 i)   = showIf r0 r1 "/="  i
    show (Mv     r0 r1  )   =  show r0 ++ " := " ++ show r1
    show (ReadPointer  d s1 )   =  show2AC d s1 "*"
    show (StorePointer d s1 )   = '*' : show d ++ " := " ++ show s1 
    show (ReadArray    d s1 s2) = show d ++" := "++show s1++'[' : show s2 ++ "]"
    show (StoreArray   d s1 s2) = show d ++'[':show s1 ++ "] := " ++ show s2
    show (TACCall     str  i k)  = "Call " ++ str ++ " #" ++ show i ++ ", #" ++ show k
    show (CallExp   d  str  i k) = "CallExp " ++ str ++ " #" ++ show i ++ ", #" ++ show k
    show (Restore   d  str  i k) = show d ++ " := Restore " ++ str ++ " #" ++ show i ++ ", #" ++ show k
    show (Clean         i )  = "Clean " ++ "#" ++ show i
    show (Epilogue i k )      = "Epilogue " ++ "#" ++ show i ++ "with return " ++ show k
    show (ReturnS   s1  s )   = "Return  " ++ show s1 ++" tag_"++ s  ++ "_epilogue"
    show (ReturnE    s      ) = "ReturnE tag_" ++ show s ++ "_epilogue"
    show (Param    par i)     = "Param " ++ show par ++" #"++show i
    show (Tag      i   )      = '\n': "tag_" ++ show i ++ ":"
    show (TagS     s   )      = '\n': "tag_" ++ s ++ ":" 
    show (TagSC    s v )      = '\n': s ++ ": \n  \"" ++ v ++ "\""
    show (Comment  str )      =  "\n# "++ str
    show (Print     c  )      = "Print "      ++ show c
    show (PrintEnum c i)      = "Print enum " ++ show c ++ "[" ++ show i ++"]"
    show Nop                  = "Nop"
    show TacExit              = "TacExit"
    show Prologue              = "Prologue"
    show (Save i)             = "Save #" ++ show i
    show (SaveRet i)          = "SaveRet #" ++ show i
    show (IniFp i)            = "IniFp " ++ show i
    show _                    = "FOUND SOMETHING WEIRD"

-- Ewwwww, it might be improved with Generics?
instance Binary IntIns where
    put  Nop                 = putWord8 0 
    put (Multi    r0 r1 r2)  = putWord8 1  >> put r0 >> put r1 >> put r2
    put (Addi     r0 r1 r2)  = putWord8 2  >> put r0 >> put r1 >> put r2
    put (Subi     r0 r1 r2)  = putWord8 3  >> put r0 >> put r1 >> put r2
    put (Divi     r0 r1 r2)  = putWord8 4  >> put r0 >> put r1 >> put r2
    put (Pot      r0 r1 r2)  = putWord8 5  >> put r0 >> put r1 >> put r2
    put (And      r0 r1 r2)  = putWord8 6  >> put r0 >> put r1 >> put r2
    put (Or       r0 r1 r2)  = putWord8 7  >> put r0 >> put r1 >> put r2
    put (XOr      r0 r1 r2)  = putWord8 8  >> put r0 >> put r1 >> put r2
    put (Not      r0 r1   )  = putWord8 9  >> put r0 >> put r1 
    put (Eql      r0 r1 r2)  = putWord8 10 >> put r0 >> put r1 >> put r2
    put (NotEql   r0 r1 r2)  = putWord8 11 >> put r0 >> put r1 >> put r2
    put (ShiftR   r0 r1 i)   = putWord8 12 >> put r0 >> put r1 >> put i
    put (ShiftL   r0 r1 i)   = putWord8 13 >> put r0 >> put r1 >> put i
    put (Lt       r0 r1 r2)  = putWord8 14 >> put r0 >> put r1 >> put r2
    put (Gt       r0 r1 r2)  = putWord8 15 >> put r0 >> put r1 >> put r2
    put (LEq      r0 r1 r2)  = putWord8 16 >> put r0 >> put r1 >> put r2
    put (GEq      r0 r1 r2)  = putWord8 17 >> put r0 >> put r1 >> put r2
    put (Jump     str    )   = putWord8 18 >> put str
    put (Jz       r0 str )   = putWord8 19 >> put r0 >>  put str
    put (Jnotz    r0 str )   = putWord8 20 >> put r0 >>  put str
    put (JLt      r0 r1 str) = putWord8 21 >> put r0 >>  put r1 >> put str
    put (JGt      r0 r1 str) = putWord8 22 >> put r0 >>  put r1 >> put str
    put (JLEq     r0 r1 str) = putWord8 23 >> put r0 >>  put r1 >> put str
    put (JGEq     r0 r1 str) = putWord8 24 >> put r0 >>  put r1 >> put str 
    put (Mv       r0 r1  )   = putWord8 25 >> put r0 >>  put r1 
    put (ReadPointer  r0 r1) = putWord8 26 >> put r0 >>  put r1 
    put (StorePointer r0 r1) = putWord8 27 >> put r0 >>  put r1 
    put (ReadArray  r0 r1 r2) = putWord8 28 >> put r0 >>  put r1 >> put r2 
    put (StoreArray r0 r1 r2) = putWord8 29 >> put r0 >>  put r1 >> put r2 
    -- put (TACCall     str i )  = putWord8 30 >> put str >> put i
    put (Param    par i )     = putWord8 31 >> put par >> put i
    put (Comment  str )      = putWord8 32 >> put str
    put (Tag      str )      = putWord8 33 >> put str
    put (Print     r0 )      = putWord8 34 >> put r0
    put (PrintEnum lb i)     = putWord8 35 >> put lb >> put i
    put (Addf    r0 r1 r2)   = putWord8 36  >> put r0 >> put r1 >> put r2
    put (Subf    r0 r1 r2)   = putWord8 37  >> put r0 >> put r1 >> put r2
    put (Divf    r0 r1 r2)   = putWord8 38  >> put r0 >> put r1 >> put r2
    put (Multf   r0 r1 r2)   = putWord8 39  >> put r0 >> put r1 >> put r2
    put (Negai   r0 str )    = putWord8 40 >> put r0 >>  put str
    put (Negaf   r0 str )    = putWord8 41 >> put r0 >>  put str
    put (JEq     r0 r1 str)  = putWord8 42 >> put r0 >>  put r1 >> put str
    put (JNEq     r0 r1 str) = putWord8 43 >> put r0 >>  put r1 >> put str
    put (Clean     i    )    = putWord8 44 >> put i
    put (TagS      str  )    = putWord8 45 >> put str 
    put TacExit              = putWord8 46
    put (TagSC     str v)    = putWord8 47 >> put str >> put v
    -- put (CallExp    r0 str i ) = putWord8 48 >> put r0 >> put str >> put i
    put (ReturnS   s1  s )     =  putWord8 49 >> put s1 >> put s 
--    put (ReturnE    s   )     =  putWord8 52 >> put s
    put (SaveRet    i  )     =  putWord8 53  >> put i

    get = do 
    key <- getWord8
    case key of
       0  ->  return    Nop
       1  ->  buildTac  Multi
       2  ->  buildTac  Addi
       3  ->  buildTac  Subi
       4  ->  buildTac  Divi
       5  ->  buildTac  Pot
       6  ->  buildTac  And
       7  ->  buildTac  Or
       8  ->  buildTac  XOr
       9  ->  build2get Not
       10 ->  buildTac  Eql
       11 ->  buildTac  NotEql
       12 ->  buildTac  ShiftL 
       13 ->  buildTac  ShiftR 
       14 ->  buildTac  Lt
       15 ->  buildTac  Gt
       16 ->  buildTac  LEq
       17 ->  buildTac  GEq
       18 ->  B.get >>= return .  Jump
       19 ->  build2get Jz
       20 ->  build2get Jnotz
       21 ->  buildTac  JLt
       22 ->  buildTac  JGt
       23 ->  buildTac  JLEq
       24 ->  buildTac  JGEq
       25 ->  build2get Mv
       26 ->  build2get ReadPointer
       27 ->  build2get StorePointer
       28 ->  buildTac  ReadArray
       29 ->  buildTac  StoreArray
       -- 30 ->  build2get TACCall 
       31 ->  build2get Param
       32 ->  B.get >>= return . Comment
       33 ->  B.get >>= return . Tag
       34 ->  B.get >>= return . Print
       35 ->  build2get PrintEnum 
       36 -> buildTac Addf   
       37 -> buildTac Subf   
       38 -> buildTac Divf   
       39 -> buildTac Multf  
       40 -> build2get Negai  
       41 -> build2get Negaf  
       42 -> buildTac JEq  
       43 -> buildTac JNEq 
       44 -> B.get >>= return . Clean 
       45 -> B.get >>= return . TagS
       46 -> return TacExit
       47 -> build2get TagSC
       -- 48 -> buildTac CallExp
       49 ->  build2get ReturnS
      -- 50 ->  buildTac ReturnE
       51 -> B.get >>= return . Save 
       -- 52 -> B.get >>= return . Epilogue 
       53 -> B.get >>= return . SaveRet 

-- Print auxiliaries
showTAC  d s1 op s2 = show d ++" := "++ show s1 ++" "++ op ++ " " ++ show s2
showTACi d s1 op i  = show d ++ " := " ++ show s1 ++ " " ++ op ++ " #" ++ show i
show2AC  d s1 op    = show d ++ " := "  ++ op ++" " ++  show s1
showIf r0 r1 s labl = "if " ++ show r0 ++ " " ++ s ++ " " ++ show r1 ++ " goto tag_" ++ show labl


isJump :: IntIns -> Bool
isJump (Jump    _ ) = True
isJump (Jz    _ _ ) = True
isJump (Jnotz _ _ ) = True
isJump (JLt  _ _ _) = True
isJump (JGt  _ _ _) = True
isJump (JLEq _ _ _) = True
isJump (JGEq _ _ _) = True
isJump (JEq  _ _ _) = True
isJump (JNEq _ _ _) = True
isJump (TACCall _  _ _ ) = True
isJump (CallExp _ _ _ _) = True
isJump (ReturnE _)      = True
isJump (ReturnS  _ _)   = True
isJump _            = False

isTag  :: IntIns -> Bool
isTag (Tag     _)   = True
isTag (TagS    _)   = True
isTag (TagSC   _ _) = True
isTag _             = False

-- Monad auxiliaries for put functions
build2get :: (Binary a1, Binary a2) => (a1 -> a2 -> r) -> Get r
build2get  constructor = liftM2 constructor B.get B.get

buildTac::(Binary a1, Binary a2, Binary a3) => (a1 -> a2 -> a3 -> r) -> Get r
buildTac constructor = liftM3 constructor B.get B.get B.get

-- Intermediate Instruction helpers
pComment :: Int -> String
pComment l = "Found at line" ++ show l

-- isTag :: IntIns -> Bool
-- isTag (Tag a) = True
-- isTag _       = False

isCons :: Var -> Bool
isCons (Int_Cons   a) = True
isCons (Float_Cons a) = True
isCons _              = False

-- Program as a sequence of instructions
type Program = Seq IntIns

-- Prety print
showP :: Program -> String
showP = foldr mapCon ""
    where mapCon ins base= (if isTag ins then""else"    ")++show ins++"\n"++base

jumpTrueFalse :: Word       -- true label
                  -> Word     -- false label
                     -> Word   -- finish label
                       -> Word  -- Temp var
                         -> Bool -- What label goes first, true or false
                          -> Program
jumpTrueFalse tl fl fil t True  = empty |> (Tag tl) |> (Mv (Temp t) (Int_Cons 1)) |> (Jump fil) |> (Tag fl) |> (Mv (Temp t) (Int_Cons 0)) |> (Tag fil)
jumpTrueFalse tl fl fil t False = empty |> (Tag fl) |> (Mv (Temp t) (Int_Cons 0)) |> (Jump fil) |> (Tag tl) |> (Mv (Temp t) (Int_Cons 1)) |> (Tag fil)


cu = Int_Cons 42
pic = Float_Cons 3.14
a  = MemAdress "a"
x  = MemAdress "x"
z  = MemAdress "z"
t0 = Temp 0
t1 = Temp 1
t2 = Temp 2

programExample :: Program
programExample = fromList stuff
    where stuff = [Nop,            
                  (Multi     cu t0 t1),
                 -- (TACCall     "fibo_3"  ),
                  (JLt      a x 5 ),
                  (Jump     3 ),
                  (Eql      t0 t0 t1),
                  (Addi      a pic a),
                  (And      z x cu),
                  (PrintEnum "pokeDaysLaborables" (Int_Cons 1)),
                  (Print     a ),
                  (Tag       58 )  ,
                  (Lt       a x z),
                  (ShiftL a x 4) ,
                  (Gt       a x cu),
                  (Comment  "This came from line 305" ),
                  (Pot      z pic cu),
                  (GEq      a x cu),
                  (Jz       a   69 ),
                  (Subi      z x cu),
                  (Not      x x ),
                  (Mv       a x    ),
                  (Tag      5 ) ,
                  (Or       z x cu),
                  (StoreArray z a z),
                  (JGEq     a x 50 ),
                  (ShiftR a x 4) ,
                  (JGt      a x 3  ),
                  (Tag      20 ),
                  (NotEql   a cu t2),
                  (Divi      z x cu),
                  (Tag      59 ),
                  (JLEq     a x 56  ),
                  (LEq      a x z),
                  (ReadArray  a (Int_Cons 3) x),
                  (Jnotz    a   42 ),
                  (XOr      z x cu),
                  (StorePointer a z),
                  -- (Param    x ),
                  (ReadPointer  z a)]

-- Binary store
saveProgram :: FilePath -> Program -> IO()
saveProgram = encodeFile

-- Binary load
loadProgram :: FilePath -> IO(Program)
loadProgram fp = do res <- decodeFile fp
                    --removeFile fb
                    return res

