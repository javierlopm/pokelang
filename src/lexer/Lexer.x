{
module Lexer(lexer) where
import Tokens
}

%wrapper "posn"

$digit   = 0-9                       -- digits
--@number = [1-9][0-9]{0,9} | 0
$lc  = [a-z]                         --LowerCase
$uc  = [A-z]                         --UpperCase
$boolean = [squirtrue squirfalse]    --Boolean
@mlComment = \-\-(( [^\-\}] | [^\-]\-|\-[^\-] | $white)* | \-$white* | \-$white* )\-\-
@id = $lc($lc|$uc|$digit)*\??
@dataId = poke($lc|$uc|$digit)+\??
@string = (\'($printable # [\'])\'|\"($printable # [\"])\")

-- Me faltan las declaraciones de tipos

tokens :-
  $white+                  ; 
  \#[^\n]*                 ; 
  @mlComment               ;
  @string                  {\p s-> TkString    s    (getPos p)}
  \[                       {\p s-> TkLB        s    (getPos p)}
  \]                       {\p s-> TkRB        s    (getPos p)}
  \{                       {\p s-> TkLCurly    s    (getPos p)}
  \}                       {\p s-> TkRCurly    s    (getPos p)}
  \(                       {\p s-> TkLP        s    (getPos p)}
  \)                       {\p s-> TkRP        s    (getPos p)}
  \:\:                     {\p s-> TkDColon    s    (getPos p)}
  \:                       {\p s-> TkColon     s    (getPos p)}
  \;                       {\p s-> TkSColon    s    (getPos p)}
  \*\=                     {\p s-> TkTEQ       s    (getPos p)}
  \+\=                     {\p s-> TkPEQ       s    (getPos p)}
  \.                       {\p s-> TkDot       s    (getPos p)}
  \!                       {\p s-> TkExcMark   s    (getPos p)}
  \!\=                     {\p s-> TkNEQ       s    (getPos p)}
  \&\&                     {\p s-> TkDAmp      s    (getPos p)}
  and                      {\p s-> TkAnd       s    (getPos p)}
  \|\|                     {\p s-> TkPOr       s    (getPos p)}
  or                       {\p s-> TkOr        s    (getPos p)}
  \=\=                     {\p s-> TkDEQ       s    (getPos p)}
  \>\=                     {\p s-> TkGE        s    (getPos p)}
  \<\=                     {\p s-> TkLE        s    (getPos p)}
  \>                       {\p s-> TkGT        s    (getPos p)}
  \<                       {\p s-> TkLT        s    (getPos p)}
  \/\/                     {\p s-> TkIDiv      s    (getPos p)}
  \/                       {\p s-> TkDiv       s    (getPos p)}
  \+                       {\p s-> TkSum       s    (getPos p)}
  \-                       {\p s-> TkMin       s    (getPos p)}
  \^                       {\p s-> TkPower     s    (getPos p)}
  \*                       {\p s-> TkTimes     s    (getPos p)}
  \%                       {\p s-> TkMod       s    (getPos p)}
  \=                       {\p s-> TkEQ        s    (getPos p)}
  pINTachu                 {\p s-> TkInt       s    (getPos p)}
  BOOLbasaur               {\p s-> TkBool      s    (getPos p)}
  CHARmander               {\p s-> TkChar      s    (getPos p)}
  VOIDtorb                 {\p s-> TkVoid      s    (getPos p)}
  butterFloat              {\p s-> TkFloat     s    (getPos p)}
  STRUCTtabuzz             {\p s-> TkStruct    s    (getPos p)}
  arcticUNION              {\p s-> TkUnion     s    (getPos p)}
  ENUManyte                {\p s-> TkEnum      s    (getPos p)}
  nullikarp                {\p s-> TkNull      s    (getPos p)}
  GLOBAt                   {\p s-> TKGlobal    s    (getPos p)}
  funcball                 {\p s-> TkNull      s    (getPos p)}
  si                       {\p s-> TkIf        s    (getPos p)}
  y\_si                    {\p s-> TkElif      s    (getPos p)}
  si\_no                   {\p s-> TkElse      s    (getPos p)}
  vamo\_a\_calmano         {\p s-> TkEnd       s    (getPos p)}
  vamo\_mientra            {\p s-> TkWhile     s    (getPos p)}
  vamo\_a\_itera           {\p s-> TkFor       s    (getPos p)}
  vamo\_a\_empeza          {\p s-> TkBegin     s    (getPos p)}
  vamo\_a\_para            {\p s-> TkBreak     s    (getPos p)}
  vamo\_a\_segui           {\p s-> TkContinue  s    (getPos p)}
  vamos\_a\_retorna        {\p s-> TkReturn    s    (getPos p)}
  vamo\_a\_sali            {\p s-> TkExit      s    (getPos p)}
  vamo\_a\_lee             {\p s-> TkRead      s    (getPos p)}
  vamo\_a\_escribi         {\p s-> TkWrite     s    (getPos p)}
  vamo\_a\_imprimi         {\p s-> TkPrint     s    (getPos p)}
  atrapar                  {\p s-> TkAlloc     s    (getPos p)}
  liberar                  {\p s-> TkFree      s    (getPos p)}
  SIZEther                 {\p s-> TkSizeOf    s    (getPos p)}
  pidget                   {\p s-> TkGet       s    (getPos p)}
  $boolean                 {\p s-> TkTruFal    s    (getPos p)}
  $digit+                  {\p s-> TkNum  (read s)  (getPos p)}
  @dataId                  {\p s-> TkDId       s    (getPos p)}
  @id                      {\p s-> TkId        s    (getPos p)}
  .                        {\p s-> TkError     s    (getPos p)}


{

getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ l c) = (l,c)
    
    
lexer :: String -> [Token] 
lexer s = alexScanTokens s
}