{-|

    UNIVERSIDAD SIMON BOLIVAR
    Departamento de Computacion y Tecnologia de la Informacion.
    CI3725 - Traductores e Interpretadores
    Abril - Julio 2015

    AUTORES:
        Edward Fernandez.   Carnet: 10-11121
        Nabil Marquez.      Carnet: 11-10683

    DESCRIPCION: Analizador Sintáctico para Lanscii.

    			 Este módulo, desarrollado en Happy, implanta un Analizador
    			 Sintáctico para el lenguaje Lanscii siguiendo las 
    			 especificaciones del mismo.

	Versión 0.2 16-06-2015

-}

{
module Parser
( parser
, parser1
) where

import Lexer
import System.IO
import AST
--import System.Enviroment
}

%name parser1 				-- Nombre de la funcion que devuelve el Parser.
%tokentype { Token }		-- Token a aceptar. 
%error { parserError }		-- Función a llamar cuando ocurre un error de parser.

-- .---------------------------------------------------------------------------.

%token
	-- Definición de los identificadores.
	-- Lenguaje.
--	program 	{ TkPROGRAM _ }
	eq     		{ TkEQUALS _ }

	-- Declaraciones
    '!'    {TkEXCLAMATION_MARK _}
    '@'    {TkAT _}

	-- Asociatividades
	'('		{ TkLPARENTHESIS _ }
	')'		{ TkRPARENTHESIS _ }
	'['		{ TkLB _ }
	']'		{ TkRB _ }
	'{'		{ TkLCURLY _ }
	'}'		{ TkRCURLY _ }

	-- Operadores aritmeticos. 
	'+' 	{ TkSUM _ }
	'-' 	{ TkMINUS _ }
	'*' 	{ TkMULT _ }
	'/' 	{ TkDIV _ }
	'%' 	{ TkPERCENT _ }

	-- Operadores sobre lienzos.
	'&'  	{ TkET _ }
	'~'	 	{ TkTILDE _ }
	'$' 	{ TkDOLAR _ }
	ap  	{ TkAPOSTROPHE _}

	-- Operadores Booleanos.
	and 	{ TkAND _ }
	or 		{ TkOR _ }
	not 	{ TkLOGICAL_NOT _ }

	-- Operadores Relacionales.
	lt  	{ TkLTHAN _ }
	le  	{ TkLEQUAL _ }
	gt  	{ TkGTHAN _ }
	ge  	{ TkGEQUAL _ }
	neq  	{ TkNEQUALS _ }

	-- Constantes.
	true 	{ TkTRUE  $$ }
	false 	{ TkFALSE $$ }

	-- Lienzos.
	canvas  { TkCANVAS $$ }

	-- Separadores.
	';' 	{ TkSEMICOLON 	_ }
	'?' 	{ TkQUESTIONMARK _ }
	':' 	{ TkCOLON _ }
	'|' 	{ TkPIPE _ }
	'..'	{ TkDOUBLEDOT _ }

	-- Funciones de Entrada/Salida.
	read 	{ TkREAD  _ }
	write 	{ TkWRITE _ }

	-- Identificadores
	var     { TkIDENTIFIER $$ }
	int     { TkNUMBER $$ }

-- .---------------------------------------------------------------------------.
-- Definición de las precedencias de los operadores:

-- Para las expresiones relacionales.
--%nonassoc '<' <\=' '>' '>\=' '=' '\/=' '..'
%nonassoc eq neq
%nonassoc lt le gt ge

-- Para los booleanos.
%left  or
%left  and
%right not

-- Para los enteros.
%left '+' '-'
%left '*' '/' '%'
%right NEG 			-- Para el - unario.

-- Para las expresiones sobre lienzos.
%left  '&' '~'
%right '$'
%left ap


%%

-- .---------------------------------------------------------------------------.
-- Definición para la gramática.

-- Gramática para las Expresiones.

Program  -- Programa principal
  : Alcance                    { $1 }

Exp:
	-- Expresiones Aritméticas.
	  Exp '+' Exp 			{ Plus  $1 $3  }
	| Exp '-' Exp 			{ Mins  $1 $3  }
	| Exp '*' Exp 			{ Times $1 $3  }
	| Exp '/' Exp 			{ Div   $1 $3  }
	| Exp '%' Exp 			{ Mod   $1 $3  }
	| '-' Exp %prec NEG 	{ Negative $2  }

	-- Expresiones Booleanas.
	| Exp or Exp 		    { Or $1 $3     }
	| Exp and Exp 		    { And  $1 $3   }
		| Exp not 		    { Not $1       }

	-- Expresiones relacionales.
	| Exp lt Exp 			{ CompLT $1 $3 }
	| Exp le Exp  		    { CompLE $1 $3 }
	| Exp gt Exp  			{ CompGT $1 $3 }
	| Exp ge Exp  		    { CompGE $1 $3 }
	| Exp eq Exp  		    { CompEQ $1 $3 }
	| Exp neq Exp  		    { CompNQ $1 $3 }

	-- Expresiones sobre lienzo.
	| Exp '&' Exp 			{ VConcat $1 $3 }
	| Exp '~' Exp  			{ HConcat $1 $3 }
		| '$' Exp  			{ Rotate  $2    }
		| Exp ap  			{ Trans   $1    }

	-- Asociatividad.
	| '(' Exp ')' 			{ Brack $2      }


	-- Constantes.
	| Term                  {   Term $1      }
	| Var					{   Var  $1      }
Var:
	var                     {   Var' $1      } 

Term:  --Simbolos terminales
	  true 					{  TRUE   		 }
	| false 				{  FALSE         }
	| canvas	     		{  Canvas' $1    }
	| int                   {  Int' $1       }

-- Gramática para las instrucciones.
Inst:
	-- Asignación.
	  var eq Exp 								{ Assign $1 $3      }

	-- Entrada/Salida.
 	| read var 									{ Scan $2           }
 	| write Exp									{ Print $2          }	  

	-- Condicional. 	
	| '(' Exp '?' Secuenc ')' 	    			{ Ifthen $2 $4         }
	| '(' Exp '?' Secuenc ':' Secuenc ')'	    { Ifelse $2 $4 $6      } 
	
	-- Iteración.
	| '[' Exp '|' Secuenc ']' 					{ While  $2 $4      }	
	| '[' Exp '..' Exp '|' Secuenc ']'			{ While2 $2 $4 $6   }
	| '[' var ':' Exp '..' Exp '|' Secuenc ']' 	{ For $2 $4 $6 $8   } 
	|  Alcance                                  {        $1         }

Secuenc:
	-- Secuenciación.
	  Secuenc ';' Inst 							{    $1++[$3]          }
	| Inst 		                                {    [$1]           }

Alcance:
	-- Incorporación de Alcance.
	  '{' Decl '|' Secuenc '}' 		            { Alcance (Decl $2, $4)   }
	| '{' Secuenc '}' 							{ Alcance (Decl [], $2)   }

Type:
	  '!' 										{ Bool       }
	| '@' 										{ Canvas     }
	| '%' 								    	{ Int        }

Decl:
	  Type Identifs Decl 						{ ($1,$2):$3 }
	| Type Identifs  							{ [($1,$2)]  }

Identifs:
	   Identifs var                                 {  $1++[$2]     }
	|  var                                      {    [$1]      }

{


--Función para imprimir los errores sintácticos.
parserError :: [Token] -> a
parserError listToken = error $ " Error Sintactico en el Token: " ++ show (head listToken)++"\n"

--Funcion principal para imprimir parser.
parser :: [Token] -> String -> IO ()
parser tok name = do
  putStrLn $ "Parser (" ++ name ++ "):\n"
  putStrLn $ show $ parser1 $ tok 

}