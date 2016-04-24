module Tokens(
    Token     (..),
    Pos
) where

data Token = TkInt    Pos
           | TkBool   Pos
           | TkChar   Pos
           | TkVoid   Pos
           | TkFloat  Pos
           | TkStruct Pos
           | TkUnion  Pos
           | TkEnum   Pos
           | TkNull   Pos
           | TkNum    Pos Int
           deriving(Show)

type Pos = (Int,Int)