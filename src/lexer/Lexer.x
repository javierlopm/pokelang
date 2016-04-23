{
module Lexer(lexer) where
import Tokens
}

%wrapper "posn"

$digit   = 0-9          -- digits
$alpha   = [a-zA-Z]     -- alphabetic characters
@string  = \"(. # \" )*\"
@identifier =    [$alpha _] [$digit $alpha _ \?]*
@badidentifier = [$digit] [$alpha _]+ [$digit $alpha _]*
@intersect = \>\<
@number = [1-9][0-9]{0,9} | 0

-- Me faltan las declaraciones de tipos

tokens :-
  $white+  ;
  @comment ;
  @badidentifier {\p s -> Error                    (getPos p) s           }
  @number        
  pINTachu       {\p _ -> TkInt    (getPos p) }
  BOOLbasaur     {\p _ -> TkBool   (getPos p) }
  CHARmander     {\p _ -> TkChar   (getPos p) }
  VOIDtorb       {\p _ -> TkVoid   (getPos p) }
  butterFloat    {\p _ -> TkFLOAT  (getPos p) }
  STRUCTtabuzz   {\p _ -> TkSTRUCT (getPos p) }
  arcticUNION    {\p _ -> TkUNION  (getPos p) }
  ENUManyte
  nullikarp
  GLOBAt
  procball
  funcball
  si
  y_si
  si_no
  vamo_a_calmano
  vamo_mientra
  vamo_a_empeza
  vamo_a_para
  vamo_a_segui
  vamos_a_retorna
  vamo_a_sali
  vamo_a_lee
  vamo_a_escribi
  [
  ]
  {
  }
  (
  )
  ::
  :
  \;
  --
  *=
  += 
  -> 
  \.
  \! 
  \; 
  \!= 
  && 
  || 
  == 
  >= 
  <= 
  = 
  > 
  < 
  // 
  / 
  + 
  - 
  ^ 
  **
  * 
  % 


{
    -- Each action has type :: String -> Token

    getPos :: AlexPosn -> (Int,Int)
    getPos (AlexPn _ l c) = (l,c)
    
    
    lexer :: String -> [Token] 
    lexer s = alexScanTokens s
}