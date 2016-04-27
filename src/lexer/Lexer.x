{
module Lexer(lexer) where
import Tokens
}

%wrapper "posn"

$lc     = [a-z]                      --LowerCase
$uc     = [A-Z]                      --UpperCase
$alpha  = [a-zA-Z]
$digit  = [0-9]                     -- digits
$dquotable = $printable # [\"]
$quotable  = $printable # [\']


@number    = [1-9][0-9]{0,9} | 0
@float     = ($digit+\.$digit+ | $digit+\.$digit+(e | E)\-?$digit+ | $digit+(e | E)\-?$digit+ )
@boolean   = (squirtrue | squirfalse)    --Boolean
@mlComment = \-\-(( [^\-\-] | [^\-]\-|\-[^\-] | $white)* | \-$white* | \-$white* )\-\-

@identifier = $lc [ $alpha $digit \_ ]*  \??
@dataId     = poke [ $lc $uc $digit \_ ]+ \??
@enum       = $uc   [$alpha $digit \_]*
@string     = \" ( $dquotable | \\\" | \' )* \" 
@char       = \' ( $quotable  | \\a | \\b | \\t | \\f | \\n | \\r | \\v | \\\\ | \\\' | \\\" ) \'
@singlecomment    = \# .* [\n]?

@emptychar    = \'\'
@emptystring  = \"\"
@badstring    = \" $dquotable* \n
@badchar      = \' $quotable*  \n
@longchar     = \' $quotable{2,} \'
@badnumber = 0 [$digit]+
@badComment = \-\-(( [^\-\-] | [^\-]\-|\-[^\-] | $white)* | \-$white* | \-$white* )[^\-]
@badComment2 = \-\-(( [^\-\-] | [^\-]\-|\-[^\-] | $white)* | \-$white* | \-$white* )\-
@badIdentifier = $digit+$uc*@identifier
@badfloat      = @float$uc*@identifier
-- faltan puntos flotante
-- faltan pipes
-- faltan los char
-- char mal cerrados
-- comentarios mal cerrados DONE
-- mumeros seguidos de cosas  DONE


tokens :-
  $white+                  ; 
  \#[^\n]*                 ; 
  @mlComment               ;
  @badComment              {\p s-> TkError     s    (getPos p) "Comment not closed"}
  @badComment2             {\p s-> TkError     s    (getPos p) "Comment not closed properly. Please use '--' to close comments"}
  @emptystring             {\p s-> TkString        []         (getPos p)}
  @emptychar               {\p s-> TkString        []         (getPos p)}
  @string                  {\p s-> TkString    (extract s)    (getPos p)}
  @char                    {\p s-> TkCharVal   (extract s)    (getPos p)}

  pINTachu                 {\p s-> TkInt       s    (getPos p)}
  BOOLbasaur               {\p s-> TkBool      s    (getPos p)}
  CHARmander               {\p s-> TkChar      s    (getPos p)}
  VOIDtorb                 {\p s-> TkVoid      s    (getPos p)}
  butterFloat              {\p s-> TkFloat     s    (getPos p)}
  STRUCTtabuzz             {\p s-> TkStruct    s    (getPos p)}
  arcticUNION              {\p s-> TkUnion     s    (getPos p)}
  ENUManyte                {\p s-> TkEnum      s    (getPos p)}
  GLOBAt                   {\p s-> TKGlobal    s    (getPos p)}
  
  nullikarp                {\p s-> TkNull      s    (getPos p)}
  
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


  funcball                 {\p s-> TkNull      s    (getPos p)}

  si                       {\p s-> TkIf        s    (getPos p)}
  y\_si                    {\p s-> TkElif      s    (getPos p)}
  si\_no                   {\p s-> TkElse      s    (getPos p)}
  atrapar                  {\p s-> TkAlloc     s    (getPos p)}
  liberar                  {\p s-> TkFree      s    (getPos p)}
  SIZEther                 {\p s-> TkSizeOf    s    (getPos p)}
  pidget                   {\p s-> TkGet       s    (getPos p)}
  
  and                      {\p s-> TkAnd       s    (getPos p)}
  or                       {\p s-> TkOr        s    (getPos p)}

  squirtrue                {\p s-> TkTrue      s    (getPos p)}
  squirfalse               {\p s-> TkFalse     s    (getPos p)}

  @float                   {\p s-> TkFloat     s    (getPos p)}
  @badfloat                {\p s-> TkError     s    (getPos p)  "Bad formed float"}
  @badIdentifier           {\p s-> TkError     s    (getPos p) "Invalid identifier"}
  @badnumber               {\p s-> TkError     s    (getPos p)  "Bad formed number"}
  @badchar                 {\p s-> TkError (init s)  (getPos p) "No single quote close found"}
  @longchar                {\p s-> TkError     s  (getPos p)    "Character sequence too long"}
  @badstring               {\p s-> TkError (init s)  (getPos p) "No double quote close found"}
  @number                  {\p s-> createNum   s    (getPos p)}
  @enum                    {\p s-> TkEnumCons  s    (getPos p)}
  @dataId                  {\p s-> TkDId       s    (getPos p)}
  poke                     {\p s-> TkError     s    (getPos p) "Invalid identifier. Did you mean 'pokeSomething' ?"}
  @identifier              {\p s-> TkId        s    (getPos p)}

  \[                       {\p s -> TkLBracket s    (getPos p)}
  \]                       {\p s -> TkRBracket s    (getPos p)}
  \{                       {\p s -> TkLCurly   s    (getPos p)}
  \}                       {\p s -> TkRCurly   s    (getPos p)}
  \(                       {\p s -> TkLRound   s    (getPos p)}
  \)                       {\p s -> TkRRound   s    (getPos p)}
  \|                       {\p s -> TkPipe     s    (getPos p)}

  \:\:                     {\p s-> TkDColon    s    (getPos p)}
  \:                       {\p s-> TkColon     s    (getPos p)}
  \;                       {\p s-> TkSColon    s    (getPos p)}
  \,                       {\p s-> TkComma     s    (getPos p)}
  \*\=                     {\p s-> TkTEQ       s    (getPos p)}
  \+\=                     {\p s-> TkPEQ       s    (getPos p)}
  \.                       {\p s-> TkDot       s    (getPos p)}
  \!                       {\p s-> TkExcMark   s    (getPos p)}
  \!\=                     {\p s-> TkNEQ       s    (getPos p)}
  \&\&                     {\p s-> TkDAmp      s    (getPos p)}
  \|\|                     {\p s-> TkPOr       s    (getPos p)}
  \=\=                     {\p s-> TkEq        s    (getPos p)}
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
  \=                       {\p s-> TkAssign    s    (getPos p)}

  .                        {\p s-> TkError     s    (getPos p) "Unexpected character"}


{

getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ l c) = (l,c)
    
    
lexer :: String -> [Token] 
lexer s = alexScanTokens s

extract :: String -> String
extract = init . tail

}