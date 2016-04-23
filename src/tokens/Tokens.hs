type Pos = (Int,Int)

data TokenType = TkInt    Pos
               | TkBool   Pos
               | TkChar   Pos
               | TkVoid   Pos
               | TkFLOAT  Pos
               | TkSTRUCT Pos
               | TkNum    Pos Int
