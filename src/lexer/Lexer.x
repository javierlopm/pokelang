{
module Lexer(lexer) where
import Tokens
}

%wrapper "posn"

$lower   = [a-z]
$upper   = [A-Z]
$digit   = [0-9]        -- digits
$alpha   = [a-zA-Z]     -- alphabetic characters

@number           = [1-9][0-9]{0,9} | 0
@identifier       = $lower   [$alpha $digit]*  '?'?
@dataindentifier  = "poke"   [$alpha $digit]+
@enum             = $upper   [$alpha $digit]*
@multilinecomment = \-\- [. \n]* \-\-
@singlecomment    = \# .* [\n]?
-- strings

@badnumber = 0 [$digit]+
-- just poke debería ser un error
-- error de comentarios
-- error de strings

-- Me faltan las declaraciones de tipos

tokens :-
  $white+  ;

  pINTachu           {\p _ -> TkInt        (getPos p)}
  BOOLbasaur         {\p _ -> TkBool       (getPos p)}
  CHARmander         {\p _ -> TkChar       (getPos p)}
  VOIDtorb           {\p _ -> TkVoid       (getPos p)}
  butterFloat        {\p _ -> TkFloat      (getPos p)}
  STRUCTtabuzz       {\p _ -> TkStruct     (getPos p)}
  arcticUNION        {\p _ -> TkUnion      (getPos p)}
  ENUManyte          {\p _ -> TkEnum       (getPos p)}
  GLOBAt             {\p _ -> TkGlobal     (getPos p)}
  procball           {\p _ -> TkProcedure  (getPos p)}

  nullikarp          {\p _ -> TkNull       (getPos p)}

  si                 {\p _ -> TkIf         (getPos p)}
  y\_si              {\p _ -> TkElif       (getPos p)}
  si\_no             {\p _ -> TkElse       (getPos p)}

  vamo\_a\_empeza    {\p _ -> TkBegin      (getPos p)}
  vamo\_a\_calmano   {\p _ -> TkEnd        (getPos p)}
  vamo\_mientra      {\p _ -> TkWhile      (getPos p)}
  vamo\_a\_para      {\p _ -> TkBreak      (getPos p)}
  vamo\_a\_segui     {\p _ -> TkContinue   (getPos p)}
  vamos\_a\_retorna  {\p _ -> TkReturn     (getPos p)}
  vamo\_a\_sali      {\p _ -> TkExit       (getPos p)}
  vamo\_a\_lee       {\p _ -> TkRead       (getPos p)}
  vamo\_a\_escribi   {\p _ -> TkWrite      (getPos p)}

  \[                 {\p _ -> TkLBracket   (getPos p)}
  \]                 {\p _ -> TkRBracket   (getPos p)}
  \{                 {\p _ -> TkLCurly     (getPos p)}
  \}                 {\p _ -> TkRCurly     (getPos p)}
  \(                 {\p _ -> TkLRound     (getPos p)}
  \)                 {\p _ -> TkRRound     (getPos p)}

  \:\:               {\p _ -> TkDefine     (getPos p)}
  \:                 {\p _ -> TkColon      (getPos p)}
  \;                 {\p _ -> TkSemiColon  (getPos p)}
  @multilinecomment  {\p _ -> TkMultiComment    (getPos p)}
  @singlecomment     {\p _ -> TkSingleComment   (getPos p)}
  \*\=               ;
  \+\=               ;
  \-\>               ;
  \.                 ;
  \!                 ;
  \;                 ;
  \!\=               ;
  \&\&               ;
  \|\|               ;
  \=\=               ;
  \>\=               ;
  \<\=               ;
  \=                 ;
  \>                 ;
  \<                 ;
  \/\/               ;
  \/                 ;
  \+                 ;
  \-                 ;
  \^                 ;
  \*\*               ;
  \*                 ;
  \%                 ;


{

getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ l c) = (l,c)
    
    
lexer :: String -> [Token] 
lexer s = alexScanTokens s

}