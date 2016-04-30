{
module Lexer(lexer) where
import Tokens
}

%wrapper "posn"

$lc     = [a-z]                      --LowerCase
$uc     = [A-Z]                      --UpperCase
$alpha  = [a-zA-Z]
$digit  = [0-9]                     -- digits
$dquotable = [\x00-\x7F] # [\"]
$quotable  = [\x00-\x7F] # [\']


@number    = [1-9][0-9]{0,9} | 0
@float     = ($digit+\.$digit+ | $digit+\.$digit+(e | E)\-?$digit+ | $digit+(e | E)\-?$digit+ )
@boolean   = (squirtrue | squirfalse)    --Boolean
@mlComment = \-\-(( [^\-\-] | [^\-]\-|\-[^\-] | $white)* | \-$white* | \-$white* )\-\-

@identifier = $lc [ $alpha $digit \_ ]*  \??
@dataId     = poke [ $lc $uc $digit \_ ]+ \??
@enum       = $uc   [$alpha $digit \_]*
@string     = \" ( $dquotable | \\\" | \' )* \" 
@char       = \' ( $quotable  | \\a | \\b | \\t | \\f | \\n | \\r | \\v | \\\\ | \\\' | \\\" | \\0 ) \'
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


tokens :-
  $white+                  ; 
  \#[^\n]*                 ; 
  @mlComment               ;
  @badComment              {\p s-> TkError    (getPos p) s "Comment not closed"}
  @badComment2             {\p s-> TkError    (getPos p) s "Comment not closed properly. Please use '--' to close comments"}
  @emptystring             {\p s-> TkString   (getPos p) [] }
  @emptychar               {\p s-> TkCharVal  (getPos p) '\0' }
  @string                  {\p s-> TkString   (getPos p) (extract s) }
  @char                    {\p s-> createChar (getPos p) (extract s) }

  pINTachu                 { \p _ -> TkInt       (getPos p) }
  BOOLbasaur               { \p _ -> TkBool      (getPos p) }
  CHARmander               { \p _ -> TkChar      (getPos p) }
  VOIDtorb                 { \p _ -> TkVoid      (getPos p) }
  butterFloat              { \p _ -> TkFloat     (getPos p) }
  STRUCTtabuzz             { \p _ -> TkStruct    (getPos p) }
  articUNION               { \p _ -> TkUnion     (getPos p) }
  ENUManyte                { \p _ -> TkEnum      (getPos p) }
  GLOBAt                   { \p _ -> TKGlobal    (getPos p) }
  
  nullikarp                { \p _ -> TkNull      (getPos p) }
  
  vamo\_a\_calmano         { \p _ -> TkEnd       (getPos p) }
  vamo\_mientra            { \p _ -> TkWhile     (getPos p) }
  vamo\_a\_itera           { \p _ -> TkFor       (getPos p) }
  vamo\_a\_empeza          { \p _ -> TkBegin     (getPos p) }
  vamo\_a\_para            { \p _ -> TkBreak     (getPos p) }
  vamo\_a\_segui           { \p _ -> TkContinue  (getPos p) }
  vamo\_a\_retorna         { \p _ -> TkReturn    (getPos p) }
  vamo\_a\_sali            { \p _ -> TkExit      (getPos p) }
  vamo\_a\_lee             { \p _ -> TkRead      (getPos p) }
  vamo\_a\_escribi         { \p _ -> TkWrite     (getPos p) }
  vamo\_a\_imprimi         { \p _ -> TkPrint     (getPos p) }


  funcball                 { \p _ -> TkNull      (getPos p) }

  si                       { \p _ -> TkIf        (getPos p) }
  y\_si                    { \p _ -> TkElif      (getPos p) }
  si\_no                   { \p _ -> TkElse      (getPos p) }
  atrapar                  { \p _ -> TkAlloc     (getPos p) }
  liberar                  { \p _ -> TkFree      (getPos p) }
  SIZEther                 { \p _ -> TkSizeOf    (getPos p) }
  pidget                   { \p _ -> TkGet       (getPos p) }
  
  and                      { \p _ -> TkAnd       (getPos p) }
  or                       { \p _ -> TkOr        (getPos p) }

  squirtrue                { \p _ -> TkTrue      (getPos p) }
  squirfalse               { \p _ -> TkFalse     (getPos p) }

  @float                   {\p s-> createFloat (getPos p) s }
  @badfloat                {\p s-> TkError    (getPos p)  "Bad formed float"               s     }
  @badIdentifier           {\p s-> TkError    (getPos p)  "Invalid identifier"             s     }
  @badnumber               {\p s-> TkError    (getPos p)  "Bad formed number"              s     }
  @badchar                 {\p s-> TkError    (getPos p)  "No single quote close found" (init s) }
  @longchar                {\p s-> TkError    (getPos p)  "Character sequence too long"    s    }
  @badstring               {\p s-> TkError    (getPos p)  "No double quote close found" (init s) }
  @number                  {\p s-> createNum  (getPos p) s }
  @enum                    {\p s-> TkEnumCons (getPos p) s }
  @dataId                  {\p s-> TkDId      (getPos p) s }
  poke                     {\p s-> TkError    (getPos p)  "Invalid identifier. Did you mean 'pokeSomething' ?"  s  }
  @identifier              {\p s-> TkId       (getPos p) s }

  \[                       { \p _  -> TkLBracket (getPos p)}
  \]                       { \p _  -> TkRBracket (getPos p)}
  \{                       { \p _  -> TkLCurly   (getPos p)}
  \}                       { \p _  -> TkRCurly   (getPos p)}
  \(                       { \p _  -> TkLRound   (getPos p)}
  \)                       { \p _  -> TkRRound   (getPos p)}
  \|                       { \p _  -> TkPipe     (getPos p)}

  \:\:                     { \p _ -> TkDColon    (getPos p)}
  \:                       { \p _ -> TkColon     (getPos p)}
  \;                       { \p _ -> TkSColon    (getPos p)}
  \,                       { \p _ -> TkComma     (getPos p)}
  \*\=                     { \p _ -> TkTEQ       (getPos p)}
  \+\=                     { \p _ -> TkPEQ       (getPos p)}
  \.                       { \p _ -> TkDot       (getPos p)}
  \!                       { \p _ -> TkExcMark   (getPos p)}
  \!\=                     { \p _ -> TkNEQ       (getPos p)}
  \&\&                     { \p _ -> TkDAmp      (getPos p)}
  \|\|                     { \p _ -> TkPOr       (getPos p)}
  \=\=                     { \p _ -> TkEq        (getPos p)}
  \>\=                     { \p _ -> TkGE        (getPos p)}
  \<\=                     { \p _ -> TkLE        (getPos p)}
  \>                       { \p _ -> TkGT        (getPos p)}
  \<                       { \p _ -> TkLT        (getPos p)}
  \/\/                     { \p _ -> TkIDiv      (getPos p)}
  \/                       { \p _ -> TkDiv       (getPos p)}
  \+                       { \p _ -> TkSum       (getPos p)}
  \-                       { \p _ -> TkMin       (getPos p)}
  \^                       { \p _ -> TkPower     (getPos p)}
  \*                       { \p _ -> TkTimes     (getPos p)}
  \%                       { \p _ -> TkMod       (getPos p)}
  \=                       { \p _ -> TkAssign    (getPos p)}

  .                        { \p s -> TkError (getPos p)  s "Unexpected character"}


{

getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ l c) = (l,c)
    
    
lexer :: String -> [Token] 
lexer s = alexScanTokens s

extract :: String -> String
extract = init . tail

extractChar :: String -> Char
extractChar s = '\0'

}
