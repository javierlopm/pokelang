{-|
   	
   	UNIVERSIDAD SIMON BOLIVAR
    Departamento de Computacion y Tecnologia de la Informacion.
    CI3725 - Traductores e Interpretadores
    Abril - Julio 2015

    AUTORES:
        Edward Fernandez.   Carnet: 10-11121
        Nabil Marquez.      Carnet: 11-10683


	DESCRIPCION: Programa Principal para el interpretador de Lanscii.

 -}
	
module Main (
	-- * Función Principal.
  	main
) where

import System.IO
import Lexer
import AST
import SymbolTable
import System.Environment   
import Parser
import ST_functions  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).
--------------------------------------------------------------------------------

-- main representa la funcion principal.

main :: IO ()

main =
	do
		fileName <- getFilename
		contents <- readFile fileName
		build $ parser1 (lexer contents)
---------------------------
--------------------------------------------------------------------------------	

{-|

   		La funcion getFilename es una funcion auxiliar que se utiliza para obtener
   	el nombre del archivo a procesar. De esta forma el usuario podra introducir
   	el nombre del archivo que desee que se procese. Ademas este espera a que se
   	introduzca una linea de texto la cual sera retornada.

-}

getFilename =
	do
		fileName <- getArgs 
		return $ (fileName !! 0)

--------------------------------------------------------------------------------