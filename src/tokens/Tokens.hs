{-# LANGUAGE DeriveDataTypeable #-}

module Tokens(
    Token     (..),
    Pos
) where

import Data.Data(toConstr,Data,Typeable)

type Pos = (Int,Int)

data Token = TkInt         { position :: Pos }
           | TkBool        { position :: Pos }
           | TkChar        { position :: Pos }
           | TkVoid        { position :: Pos }
           | TkFloat       { position :: Pos }
           | TkStruct      { position :: Pos }
           | TkUnion       { position :: Pos }
           | TkEnum        { position :: Pos }
           | TkNull        { position :: Pos }
           | TkGlobal      { position :: Pos }
           | TkProcedure   { position :: Pos }
           | TkIf          { position :: Pos }
           | TkElif        { position :: Pos }
           | TkElse        { position :: Pos }
           | TkBegin       { position :: Pos }
           | TkEnd         { position :: Pos }
           | TkWhile       { position :: Pos }
           | TkBreak       { position :: Pos }
           | TkContinue    { position :: Pos }
           | TkReturn      { position :: Pos }
           | TkExit        { position :: Pos }
           | TkRead        { position :: Pos }
           | TkWrite       { position :: Pos }
           | TkLBracket    { position :: Pos }
           | TkRBracket    { position :: Pos }
           | TkLCurly      { position :: Pos }
           | TkRCurly      { position :: Pos }
           | TkLRound      { position :: Pos }
           | TkRRound      { position :: Pos }
           | TkDefine      { position :: Pos }
           | TkColon      { position :: Pos }
           | TkSemiColon      { position :: Pos }
           | TkMultiComment  { position :: Pos }
           | TkSingleComment { position :: Pos }
           | TkNum         Pos Int
           | TkIdentifier  Pos String
           | Error         Pos String
           deriving(Data,Typeable)



instance Show Token where
  show generic = show (toConstr generic )++ "\n" ++
                 "    line:   " ++ show l ++ "\n" ++
                 "    column: " ++ show c ++ "\n"
                where (l,c) = position generic