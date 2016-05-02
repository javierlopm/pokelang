{-# LANGUAGE DeriveDataTypeable #-}

module Tokens(
    Token     (..),
    checkErrors,
    checkErrors',
    createNum,
    createFloat,
    createChar,
    Pos
) where

import Data.Data(toConstr,Data,Typeable)
import System.IO(hPrint,stderr)  

type Pos = (Int,Int)

showPos :: Pos -> String
showPos (l,c) = "    line:   " ++ show l  ++ "\n" ++
                "    column: " ++ show c  ++ "\n"

data Token =  TkLBracket  { position :: Pos }
            | TkRBracket  { position :: Pos }
            | TkLCurly    { position :: Pos }
            | TkRCurly    { position :: Pos }
            | TkLRound    { position :: Pos }
            | TkRRound    { position :: Pos }
            | TkPipe      { position :: Pos }
            | TkDColon    { position :: Pos }
            | TkColon     { position :: Pos }
            | TkSColon    { position :: Pos }
            | TkComma     { position :: Pos }
            | TkTEQ       { position :: Pos }
            | TkPEQ       { position :: Pos }
            | TkDot       { position :: Pos }
            | TkExcMark   { position :: Pos }
            | TkNEQ       { position :: Pos }
            | TkDAmp      { position :: Pos }
            | TkAnd       { position :: Pos }
            | TkPOr       { position :: Pos }
            | TkOr        { position :: Pos }
            | TkDEQ       { position :: Pos }
            | TkGE        { position :: Pos }
            | TkLE        { position :: Pos }
            | TkGT        { position :: Pos }
            | TkLT        { position :: Pos }
            | TkIDiv      { position :: Pos }
            | TkDiv       { position :: Pos }
            | TkSum       { position :: Pos }
            | TkMin       { position :: Pos }
            | TkPower     { position :: Pos }
            | TkTimes     { position :: Pos }
            | TkMod       { position :: Pos }
            | TkEq        { position :: Pos }
            | TkAssign    { position :: Pos }
            | TkInt       { position :: Pos }
            | TkBool      { position :: Pos }
            | TkChar      { position :: Pos }
            | TkVoid      { position :: Pos }
            | TkFloat     { position :: Pos }
            | TkStruct    { position :: Pos }
            | TkUnion     { position :: Pos }
            | TkEnum      { position :: Pos }
            | TkNull      { position :: Pos }
            | TKGlobal    { position :: Pos }
            | TkFunc      { position :: Pos }
            | TkIf        { position :: Pos }
            | TkElif      { position :: Pos }
            | TkElse      { position :: Pos }
            | TkEnd       { position :: Pos }
            | TkWhile     { position :: Pos }
            | TkFor       { position :: Pos }
            | TkBegin     { position :: Pos }
            | TkBreak     { position :: Pos }
            | TkContinue  { position :: Pos }
            | TkReturn    { position :: Pos }
            | TkExit      { position :: Pos }
            | TkRead      { position :: Pos }
            | TkWrite     { position :: Pos }
            | TkPrint     { position :: Pos }
            | TkAlloc     { position :: Pos }
            | TkFree      { position :: Pos }
            | TkSizeOf    { position :: Pos }
            | TkGet       { position :: Pos }
            | TkTrue      { position :: Pos }
            | TkFalse     { position :: Pos }
            | TkDId       { position :: Pos, lexeme :: String }
            | TkId        { position :: Pos, lexeme :: String }
            | TkCharVal   { position :: Pos, char   :: Char   }
            | TkEnumCons  { position :: Pos, lexeme :: String }
            | TkString    { position :: Pos, content:: String }
            | TkNum       { position :: Pos, value  :: Integer}
            | TkFloatVal  { position :: Pos, rep    :: Float  }
            | TkError     { position :: Pos, content:: String, message::String }
            deriving(Data,Typeable)

-- floating points missing
-- number size check missing
-- function to check if there is any error missing

instance Show Token where
  show (TkId  pos con) = "Identifier\n" ++
                         "    lexeme: " ++ con ++ "\n" ++ 
                         showPos pos

  show (TkNum pos v) = "Integer\n" ++
                       "    value:  " ++ show v ++ "\n" ++ 
                       showPos pos
                           
  show (TkFloatVal pos v) = "Integer\n" ++
                            "    value:  " ++ show v  ++ "\n" ++
                            showPos pos

  show (TkCharVal pos '\0' ) = "Empty Character sequence\n" ++
                             showPos pos

  show (TkCharVal pos c ) = "Character\n" ++
                            "    value:  " ++ [c] ++ "\n" ++
                            showPos pos
                           

  show (TkString pos [] ) = "Empty String\n" ++
                            showPos pos

  show (TkString pos str) = "String\n" ++
                            "    value:  " ++ str ++ "\n" ++
                            showPos pos
                           

  show (TkError (l,c) con  m) = "Error:" ++ show l ++ ":" ++ show c ++ " " ++ con ++". " ++ "(" ++ m ++ ")"

  show generic = show (toConstr generic ) ++ "\n" ++ showPos (position generic)

createNum :: Pos -> String -> Token
createNum p s = if number <= 2147483648 then TkNum    p number
                                        else TkError  p s "Number overflow"
    where number = read s :: Integer

createFloat :: Pos -> String -> Token
createFloat pos num = if  double > largest 
                        then TkError  pos num "Floating point overflow"
                        else checkUnderflow
  where largest  = 3.402823566e38
        double   = read num :: Double
        (number,ex) = break (=='e') num
        exp'        = (read . tail) ex :: Int
        signigicand = read number :: Float
        checkUnderflow 
            | null ex = TkFloatVal pos (read num)
            | exp' < -45 ||  (exp' == (-45) && signigicand <= 1.4013) = TkError pos num "Floating point underflow"
            | otherwise = TkFloatVal pos (read num)

-- Create a character from a string without 
createChar ::  Pos -> String -> Token
createChar p str
    | is "\\a"  = TkCharVal p '\a'
    | is "\\b"  = TkCharVal p '\b'
    | is "\\t"  = TkCharVal p '\t'
    | is "\\f"  = TkCharVal p '\f'
    | is "\\n"  = TkCharVal p '\n'
    | is "\\r " = TkCharVal p '\r'
    | is "\\v"  = TkCharVal p '\v'
    | is "\\\\" = TkCharVal p '\\'
    | is "\\\'" = TkCharVal p '\''
    | is "\\\"" = TkCharVal p '\"'
    | is "\\0"  = TkCharVal p '\0'
    | otherwise = TkCharVal p $ head str
    where is str2 = str == str2


checkErrors :: Token -> (IO(),Int)
checkErrors myTok@TkError{} =  (hPrint stderr myTok, 1)
checkErrors myTok =  (print myTok,0)

checkErrors' :: [Token] -> ([Token],[Token],Int)
checkErrors' = foldr pickGoods ([],[],0)
    where pickGoods myTok@TkError{} (gs,bs,bcount) = (gs,myTok:bs,bcount+1)
          pickGoods myTok           (gs,bs,bcount) = (myTok:gs,bs,bcount)