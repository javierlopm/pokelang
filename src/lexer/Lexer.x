{
module Lexer(lexer) where
import Tokens
}

%wrapper "posn"

$digit   = 0-9          -- digits
@number = [1-9][0-9]{0,9} | 0

-- Me faltan las declaraciones de tipos

tokens :-
  $white+  ;    
  pINTachu       {\p _ -> TkInt    (getPos p) }
  BOOLbasaur     {\p _ -> TkBool   (getPos p) }
  CHARmander     {\p _ -> TkChar   (getPos p) }
  VOIDtorb       {\p _ -> TkVoid   (getPos p) }
  butterFloat    {\p _ -> TkFloat  (getPos p) }
  STRUCTtabuzz   {\p _ -> TkStruct (getPos p) }
  arcticUNION    {\p _ -> TkUnion  (getPos p) }
  ENUManyte      {\p _ -> TkEnum  (getPos p)}
  nullikarp      {\p _ -> TkNull  (getPos p)}
  GLOBAt   ;
  procball   ;
  funcball   ;
  si   ;
  y\_si   ;
  si\_no   ;
  vamo\_a\_calmano   ;
  vamo\_mientra   ;
  vamo\_a\_empeza   ;
  vamo\_a\_para   ;
  vamo\_a\_segui   ;
  vamos\_a\_retorna   ;
  vamo\_a\_sali   ;
  vamo\_a\_lee   ;
  vamo\_a\_escribi   ;
  \[   ;
  \]   ;
  \{   ;
  \}   ;
  \(   ;
  \)   ;
  \:\:   ;
  \:   ;
  \;   ;
  \-\-   ;
  \*\=   ;
  \+\=    ;
  \-\>    ;
  \.   ;
  \!    ;
  \;    ;
  \!\=    ;
  \&\&    ;
  \|\|    ;
  \=\=    ;
  \>\=    ;
  \<\=    ;
  \=    ;
  \>    ;
  \<    ;
  \/\/    ;
  \/    ;
  \+    ;
  \-    ;
  \^    ;
  \*\*   ;
  \*    ;
  \%    ;


{

getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ l c) = (l,c)
    
    
lexer :: String -> [Token] 
lexer s = alexScanTokens s
}