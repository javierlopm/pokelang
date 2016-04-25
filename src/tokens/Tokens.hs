module Tokens(
    Token     (..),
    Pos
) where

data Token =  TkString   String Pos
            | TkLB       String Pos
            | TkRB       String Pos
            | TkLCurly   String Pos
            | TkRCurly   String Pos
            | TkLP       String Pos
            | TkRP       String Pos
            | TkDColon   String Pos
            | TkColon    String Pos
            | TkSColon   String Pos
            | TkTEQ      String Pos
            | TkPEQ      String Pos
            | TkDot      String Pos
            | TkExcMark  String Pos
            | TkNEQ      String Pos
            | TkDAmp     String Pos
            | TkAnd      String Pos
            | TkPOr      String Pos
            | TkOr       String Pos
            | TkDEQ      String Pos
            | TkGE       String Pos
            | TkLE       String Pos
            | TkGT       String Pos
            | TkLT       String Pos
            | TkIDiv     String Pos
            | TkDiv      String Pos
            | TkSum      String Pos
            | TkMin      String Pos
            | TkPower    String Pos
            | TkTimes    String Pos
            | TkMod      String Pos
            | TkEQ       String Pos
            | TkInt      String Pos
            | TkBool     String Pos
            | TkChar     String Pos
            | TkVoid     String Pos
            | TkFloat    String Pos
            | TkStruct   String Pos
            | TkUnion    String Pos
            | TkEnum     String Pos
            | TkNull     String Pos
            | TKGlobal   String Pos
            | TkFunc     String Pos
            | TkIf       String Pos
            | TkElif     String Pos
            | TkElse     String Pos
            | TkEnd      String Pos
            | TkWhile    String Pos
            | TkFor      String Pos
            | TkBegin    String Pos
            | TkBreak    String Pos
            | TkContinue String Pos
            | TkReturn   String Pos
            | TkExit     String Pos
            | TkRead     String Pos
            | TkWrite    String Pos
            | TkPrint    String Pos
            | TkAlloc    String Pos
            | TkFree     String Pos
            | TkSizeOf   String Pos
            | TkGet      String Pos
            | TkTruFal   String Pos
            | TkNum      Int    Pos  
            | TkDId      String Pos
            | TkId       String Pos
            | TkError    String Pos
            deriving(Show)

type Pos = (Int,Int)