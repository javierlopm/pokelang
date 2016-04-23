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
  @number        {\p s -> IntTok      IntConst     (getPos p) (read s)    }
  pINTachu       {\p _ -> TkINt      Loe          (getPos p) Empty Empty }
  BOOLbasaur
  CHARmander
  VOIDtorb
  butterFloat
  STRUCTtabuzz
  arcticUNION
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