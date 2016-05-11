{-|

    UNIVERSIDAD SIMON BOLIVAR
    Departamento de Computacion y Tecnologia de la Informacion.
    CI3725 - Traductores e Interpretadores
    Abril - Julio 2015

    AUTORES:
        Edward Fernandez.   Carnet: 10-11121
        Nabil Marquez.      Carnet: 11-10683

    DESCRIPCION: Typos y constructores necesarios pala en analizador sintaxico.


	Versión 0.2 16-06-2015
-}

module AST 
( Identifs(..),
  Decl(..),
  Type(..),
  Inst(..),
  Exp(..),
  Terminal(..),
  Canvas(..),
  Variable(..)
  ) where 

--------------------------------------------------------------------------------------

type Identifs = String

data Decl
	= Decl       [(Type,[Identifs])]
	deriving(Eq,Show)

data Type
	= Bool
	| Canvas
	| Int
	deriving(Eq,Show)

type Canvas = String

data Inst =
	Assign     Identifs Exp
	| Scan     Identifs
	| Print    Exp
	| Ifthen 	   Exp [Inst] 
	| Ifelse 	   Exp [Inst] [Inst]
	| While    Exp [Inst] 
	| While2   Exp Exp [Inst]
	| For      Identifs Exp Exp [Inst]
	| Alcance  (Decl,[Inst])
	deriving(Eq,Show)

data Exp 
	= Plus     Exp Exp
	| Mins     Exp Exp
	| Times    Exp Exp
	| Div      Exp Exp
	| Mod      Exp Exp
	| Or       Exp Exp
	| And      Exp Exp
	| Not      Exp
	| Negative Exp
	| CompLT   Exp Exp
	| CompLE   Exp Exp
	| CompGT   Exp Exp
	| CompGE   Exp Exp
	| CompEQ   Exp Exp
	| CompNQ   Exp Exp
	| VConcat  Exp Exp
	| HConcat  Exp Exp
	| Rotate   Exp
	| Trans    Exp
	| Brack    Exp
	| Term 	   Terminal
	| Var      Variable
	deriving(Eq,Show)

data Terminal
	= Canvas'  Canvas
	| Int'     String
	| TRUE   
	| FALSE
	deriving(Eq,Show)

data Variable = Var' String
	deriving(Eq,Show)

--------------------------------------------------------------------------------------