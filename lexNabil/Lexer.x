{

{-|

    UNIVERSIDAD SIMON BOLIVAR
    Departamento de Computacion y Tecnologia de la Informacion.
    CI3725 - Traductores e InterprTkETadores
    Abril - Julio 2015

    AUTTkORES:
        Edward Fernandez.   CarnTkET: 10-11121
        Nabil Marquez.      CarnTkET: 11-10683

    DESCRIPCION: Analizador Lexicografico para Lanscii.

                 Este módulo, desarrollado en Alex, implanta un Analizador
                 Lexicografico para el lenguaje Lanscii siguiendo la
                 especificación del mismo.

                 Se implantaron algunas funciones auxiliares no exportadas,
                 cuya documentación se encuentra en el código fuente.

	Versión 0.2 16-06-2015



-}
    module Lexer where
}


%wrapper "posn" 

-- Macros a definir:
$digito = 0-9           -- Un digito
$lTkETra  = [a-zA-Z]      -- Una lTkETra
$canvas = [ \/ \\ \| _ \- \ ]
tokens :-

--------------------------------------------------------------------------------
-- Expresiones regulares a definir.

	$white+                                                                             ;
    \{\-(([^\-\}]|[^\-]\}|\-[^\}]|$white)*|\-$white*|\}$white*)\-\}                   ;
    \{                                { \p s -> TkLCURLY           (tokenFormat s p True)     }
    \|                                { \p s -> TkPIPE             (tokenFormat s p True)     }
    &                                 { \p s -> TkET               (tokenFormat s p True)     }
    \~                                { \p s -> TkTILDE            (tokenFormat s p True)     }
	\}                                { \p s -> TkRCURLY           (tokenFormat s p True)     }
    \!                                { \p s -> TkEXCLAMATION_MARK (tokenFormat s p True)     }
    @                                 { \p s -> TkAT               (tokenFormat s p True)     }
	\[                                { \p s -> TkLB               (tokenFormat s p True)     }
	\]                                { \p s -> TkRB               (tokenFormat s p True)     }
	\;                                { \p s -> TkSEMICOLON        (tokenFormat s p True)     }
    \:                                { \p s -> TkCOLON            (tokenFormat s p True)     }
    \.\.                              { \p s -> TkDOUBLEDOT        (tokenFormat s p True)     }
    \?                                { \p s -> TkQUESTIONMARK     (tokenFormat s p True)     }
    \'                                { \p s -> TkAPOSTROPHE       (tokenFormat s p True)     }
    \$                                { \p s -> TkDOLAR            (tokenFormat s p True)     }
    \+                                { \p s -> TkSUM              (tokenFormat s p True)     }
    \-                                { \p s -> TkMINUS            (tokenFormat s p True)     }
    \*                                { \p s -> TkMULT             (tokenFormat s p True)     }
    \/                                { \p s -> TkDIV              (tokenFormat s p True)     }
    \%                                { \p s -> TkPERCENT          (tokenFormat s p True)     }
    \(                                { \p s -> TkLPARENTHESIS     (tokenFormat s p True)     }
    \)                                { \p s -> TkRPARENTHESIS     (tokenFormat s p True)     }
    \<                                { \p s -> TkLTHAN            (tokenFormat s p True)     }
    \<\=                              { \p s -> TkLEQUAL           (tokenFormat s p True)     }
    \>                                { \p s -> TkGTHAN            (tokenFormat s p True)     }
    \>\=                              { \p s -> TkGEQUAL           (tokenFormat s p True)     }
    \=                                { \p s -> TkEQUALS           (tokenFormat s p True)     }
    \/\=                              { \p s -> TkNEQUALS          (tokenFormat s p True)     }
    \/\\                              { \p s -> TkAND              (tokenFormat s p True)     }
    \^                                { \p s -> TkLOGICAL_NOT      (tokenFormat s p True)     }
    \\\/                              { \p s -> TkOR               (tokenFormat s p True)     }
    write                             { \p s -> TkWRITE            (tokenFormat s p True)     }
    read                              { \p s -> TkREAD             (tokenFormat s p True)     }
    true                              { \p s -> TkTRUE             (tokenFormat s p True)     }
    false                             { \p s -> TkFALSE            (tokenFormat s p True)     }
    \<$canvas\>                       { \p s -> TkCANVAS           (tokenFormat s p True)     }
    \#                                { \p s -> TkCANVAS           (tokenFormat s p True)     }
    $digito+                          {                 lexInt                                }
    [$lTkETra _][ $lTkETra $digito _ ]*   { \p s -> TkIDENTIFIER      (tokenFormat s p True)  }
    .                                 { \p s -> TkError         (tokenFormat s p False)       }

--------------------------------------------------------------------------------

{   

{-|
    TIPOS A DEFINIR:

            El tipo de datos "Token" representa los diferentes tokens
        que producira el Analizador Lexicografico. Cada uno de ellos 
        estara acompañado por una tupla de enteros. Dichos enteros 
        representara la fila y columna en la que fue encontrado dentro 
        del archivo que fue procesado. 

		El tipo de datos "Token" se declara derivando de:
            * "Show" puesto que al invocar la funcion "lexer" la lista 
                     que se genera pueda ser representada como string 
                     para que al final se pueda mostrar directamente
                     en pantalla.
            * "Eq" ya que de esta forma se puedan comparar tipos como
                   iguales. Es fundamental para el funcionamiento del
                   Parser. 
-}

data Token 
        = TkLCURLY            String
        | TkPIPE              String
        | TkET                String
        | TkTILDE             String
        | TkRCURLY            String
        | TkEXCLAMATION_MARK  String
        | TkAT                String
        | TkLB                String
        | TkRB                String
        | TkSEMICOLON         String
        | TkCOLON             String
        | TkDOUBLEDOT         String
        | TkAPOSTROPHE        String
        | TkQUESTIONMARK      String
        | TkDOLAR             String
        | TkSUM               String
        | TkMINUS             String
        | TkMULT              String
        | TkDIV               String
        | TkPERCENT           String
        | TkLPARENTHESIS      String
        | TkRPARENTHESIS      String
        | TkLTHAN             String
        | TkLEQUAL            String
        | TkGTHAN             String
        | TkGEQUAL            String
        | TkEQUALS            String
        | TkAND               String
        | TkLOGICAL_NOT       String
        | TkOR                String
        | TkNEQUALS           String
        | TkWRITE             String
        | TkREAD              String
        | TkTRUE              String
        | TkFALSE             String
        | TkBOOLEAN           String
        | TkCANVAS            String
        | TkNUMBER            String   
        | TkIDENTIFIER        String
        | TkError             String
        | TkIntError          String
        deriving (Eq, Show)

--------------------------------------------------------------------------------

{-|
    
        La funcion printToken se encargara de mostrar por pantalla un mensaje que
    indique cual token es invalido. Asi como tambien la fila y la columna donde
    fue encontrado.
    En el caso de tokens correctos, solo se devolverá el valor de este para su futuro
    uso en el parser.
-}
printToken :: Token -> String
printToken (TkError a)= "Error: Unexpected character "++a++"\n"
printToken (TkIntError a) = "Error: integer out of range (-2^32..2^32-1): "++a++"\n"
printToken (TkLCURLY a) = a
printToken (TkPIPE a) = a
printToken (TkET a) = a
printToken (TkTILDE a) = a
printToken (TkRCURLY a) = a
printToken (TkEXCLAMATION_MARK a) = a
printToken (TkAT a) = a
printToken (TkLB a) = a
printToken (TkRB a) = a
printToken (TkSEMICOLON a) = a
printToken (TkCOLON a) = a
printToken (TkDOUBLEDOT a) = a
printToken (TkAPOSTROPHE a) = a
printToken (TkQUESTIONMARK a) = a
printToken (TkDOLAR a) = a
printToken (TkSUM a) = a
printToken (TkMINUS a) = a
printToken (TkMULT a) = a
printToken (TkDIV a) = a
printToken (TkPERCENT a) = a
printToken (TkLPARENTHESIS a) = a
printToken (TkRPARENTHESIS a) = a
printToken (TkLTHAN a) = a
printToken (TkLEQUAL a) = a
printToken (TkGTHAN a) = a
printToken (TkGEQUAL a) =a
printToken (TkEQUALS a) =a
printToken (TkAND a) =a
printToken (TkLOGICAL_NOT a) =a
printToken (TkOR a) = a
printToken (TkNEQUALS a) = a
printToken (TkWRITE a) =a
printToken (TkREAD a) = a
printToken (TkTRUE a) =a
printToken (TkFALSE a) =a
printToken (TkBOOLEAN a) =a
printToken (TkCANVAS a) =a
printToken (TkNUMBER a) = a
printToken (TkIDENTIFIER a) = a

-------------------------------------------------------------------

{-|

        La funcion isError verifica si un token dado es valido o no. Si este 
    es valido se rTkETorna el booleano "True". De lo contrario "False".

-}

isError :: Token -> Bool
isError (TkError _) = True
isError (TkIntError _) = True
isError _ = False

--------------------------------------------------------------------------------

{-|
    
        La funcion check_errors verifica si los tokens pertenecientes a una lista 
    de tokens son validos. Si estos son validos entonces se rTkETorna el booleano
    "True". De lo contrario se rTkETorna "False".

-}


check_errors :: [Token]-> Bool
check_errors tok =
    if tok==[] then 
        False
    else 
        if isError (tok !! 0) then True
        else check_errors (tail tok)

--------------------------------------------------------------------------------

{-|

        La funcion lexerPrint se encarga de construir mensaje que
    indicara el resultado del analisis lexicografico. 

        * Si no se encontro ningun token invalido entonces se incluyen todos 
          los tokens encontrados. Asi como tambien el tipo al que pertenece, la
          fila y la columna donde fue encontrado.

        * Si se encontro un token invalido entonces se incluyen todos los 
          invalidos mediante la funcion "printToken".

-}
lexerError :: [Token]->Bool->[a]
lexerError tok is = lexerError' tok is ""
    where
        lexerError' tok is acum =
            if tok==[] then error acum
            else 
                lexerError' (tail tok) is $ (printToken $ (tok !! 0)) ++ acum

--------------------------------------------------------------------------------

{-|

        La funcion lexer es la que se encargara de realizar el analisis 
    lexicografico. En esta funcion se leera una cadena de tokens dada y se 
    verificara si hay alguno invalido. Si se consigue ningun error entonces 
    devolverá los mensajes correspondientes. En caso contrario, devuelve el valor de
    los tokens que son correctos.

-}

lexer :: String->[Token]

lexer s = 
    let tok = (alexScanTokens s)
        err = check_errors tok
    in
        if err then (lexerError (filter (\t -> isError t) tok) True)
        else tok

--------------------------------------------------------------------------------

{-|

        La funcion lyc se utilizara para obtener a fila y columna en la que fue 
    encontrado el token dentro del archivo que fue procesado. Esto se podra 
    obtener mediante el wrapper 'posn'. Este genera para cada token el 
    desplazamiento absoluto dentro del archivo, la linea y la columna.
		
    lyc = linea y columna.

-}

lyc :: AlexPosn -> String
lyc (AlexPn _ l c) = " at line: "++show(l)++", column: "++show(c)

--------------------------------------------------------------------------------

{-|

        La funcion lexInt comprueba que un entero encontrado sea representable en 32 bits.
    En caso contrario, devuelve un token de error. 

-}

lexInt :: AlexPosn -> String -> Token
lexInt p s
  | (n < -2^32) || (n > 2^32-1)  = TkIntError (tokenFormat s p False)
  | otherwise  = TkNUMBER (tokenFormat s p True)
  where n = (read s :: (Num a, Read a) => a)

--------------------------------------------------------------------------------    


{-|

        Funcion que permite colocar en formato de string el valor y posicion del
     token a devolver. Si es un token valido no es necesario cambiar su formato.
-}  

tokenFormat :: String -> AlexPosn -> Bool -> String
tokenFormat s p b=
    if b then s
    else "\""++s++"\""++(lyc p)									
--------------------------------------------------------------------------------

}
