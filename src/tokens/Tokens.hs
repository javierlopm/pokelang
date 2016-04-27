{-# LANGUAGE DeriveDataTypeable #-}

module Tokens(
    Token     (..),
    checkErrors,
    createNum,
    Pos
) where

import Data.Data(toConstr,Data,Typeable)
import System.IO(hPutStrLn,stderr)  

type Pos = (Int,Int)

data Token =  TkString    {content::String, position :: Pos }
            | TkLBracket  {content::String, position :: Pos }
            | TkRBracket  {content::String, position :: Pos }
            | TkLCurly    {content::String, position :: Pos }
            | TkRCurly    {content::String, position :: Pos }
            | TkLRound    {content::String, position :: Pos }
            | TkRRound    {content::String, position :: Pos }
            | TkPipe      {content::String, position :: Pos }
            | TkDColon    {content::String, position :: Pos }
            | TkColon     {content::String, position :: Pos }
            | TkSColon    {content::String, position :: Pos }
            | TkComma     {content::String, position :: Pos }
            | TkTEQ       {content::String, position :: Pos }
            | TkPEQ       {content::String, position :: Pos }
            | TkDot       {content::String, position :: Pos }
            | TkExcMark   {content::String, position :: Pos }
            | TkNEQ       {content::String, position :: Pos }
            | TkDAmp      {content::String, position :: Pos }
            | TkAnd       {content::String, position :: Pos }
            | TkPOr       {content::String, position :: Pos }
            | TkOr        {content::String, position :: Pos }
            | TkDEQ       {content::String, position :: Pos }
            | TkGE        {content::String, position :: Pos }
            | TkLE        {content::String, position :: Pos }
            | TkGT        {content::String, position :: Pos }
            | TkLT        {content::String, position :: Pos }
            | TkIDiv      {content::String, position :: Pos }
            | TkDiv       {content::String, position :: Pos }
            | TkSum       {content::String, position :: Pos }
            | TkMin       {content::String, position :: Pos }
            | TkPower     {content::String, position :: Pos }
            | TkTimes     {content::String, position :: Pos }
            | TkMod       {content::String, position :: Pos }
            | TkEq        {content::String, position :: Pos }
            | TkAssign    {content::String, position :: Pos }
            | TkInt       {content::String, position :: Pos }
            | TkBool      {content::String, position :: Pos }
            | TkChar      {content::String, position :: Pos }
            | TkCharVal   {content::String, position :: Pos }
            | TkVoid      {content::String, position :: Pos }
            | TkFloat     {content::String, position :: Pos }
            | TkStruct    {content::String, position :: Pos }
            | TkUnion     {content::String, position :: Pos }
            | TkEnum      {content::String, position :: Pos }
            | TkEnumCons  {content::String, position :: Pos }
            | TkNull      {content::String, position :: Pos }
            | TKGlobal    {content::String, position :: Pos }
            | TkFunc      {content::String, position :: Pos }
            | TkIf        {content::String, position :: Pos }
            | TkElif      {content::String, position :: Pos }
            | TkElse      {content::String, position :: Pos }
            | TkEnd       {content::String, position :: Pos }
            | TkWhile     {content::String, position :: Pos }
            | TkFor       {content::String, position :: Pos }
            | TkBegin     {content::String, position :: Pos }
            | TkBreak     {content::String, position :: Pos }
            | TkContinue  {content::String, position :: Pos }
            | TkReturn    {content::String, position :: Pos }
            | TkExit      {content::String, position :: Pos }
            | TkRead      {content::String, position :: Pos }
            | TkWrite     {content::String, position :: Pos }
            | TkPrint     {content::String, position :: Pos }
            | TkAlloc     {content::String, position :: Pos }
            | TkFree      {content::String, position :: Pos }
            | TkSizeOf    {content::String, position :: Pos }
            | TkGet       {content::String, position :: Pos }
            | TkTrue      {content::String, position :: Pos }
            | TkFalse     {content::String, position :: Pos }
            | TkNum       {content::String, position :: Pos, value::Integer}
            | TkDId       {content::String, position :: Pos }
            | TkId        {content::String, position :: Pos }
            | TkError     {content::String, position :: Pos, message::String }
            deriving(Data,Typeable)

-- floating points missing
-- number size check missing
-- function to check if there is any error missing

instance Show Token where
  show (TkId con (l,c)) = "Identifier\n" ++
                           "    lexeme: " ++ con ++ "\n" ++
                           "    line:   " ++ show l  ++ "\n" ++
                           "    column: " ++ show c  ++ "\n"

  show (TkNum con (l,c) v) = "Integer\n" ++
                           "    value:  " ++ show con ++ "\n" ++
                           "    line:   " ++ show l  ++ "\n" ++
                           "    column: " ++ show c  ++ "\n"

  show (TkCharVal [] (l,c)) = "Empty Character sequence\n" ++
                           "    line:   " ++ show l  ++ "\n" ++
                           "    column: " ++ show c  ++ "\n"

  show (TkCharVal con (l,c) ) = "Character\n" ++
                           "    value:  " ++ con ++ "\n" ++
                           "    line:   " ++ show l  ++ "\n" ++
                           "    column: " ++ show c  ++ "\n"

  show (TkString [] (l,c) ) = "Empty String\n" ++
                           "    line:   " ++ show l  ++ "\n" ++
                           "    column: " ++ show c  ++ "\n"

  show (TkString con (l,c) ) = "String\n" ++
                           "    value:  " ++ con ++ "\n" ++
                           "    line:   " ++ show l  ++ "\n" ++
                           "    column: " ++ show c  ++ "\n"

  show (TkError con (l,c) m) = "Error: " ++ m ++". " ++ "\" " ++ con ++ " \" " ++ "at " ++ show l ++ ":" ++ show c ++ "\n"

  show generic = show (toConstr generic )++ "\n" ++
                 "    line:   " ++ show l ++ "\n" ++
                 "    column: " ++ show c ++ "\n"
                where (l,c) = position generic

createNum :: String -> Pos -> Token
createNum s p = if number <= 2147483648 
                    then (TkNum  s p number)
                    else (TkError s p "Number overflow")
    where number = read s :: Integer

-- hace falta
createFloat = undefined

checkErrors :: Token -> IO()
checkErrors myTok@(TkError con (l,c) v) =  (hPutStrLn stderr . show ) myTok
checkErrors myTok =  (putStrLn . show ) myTok
