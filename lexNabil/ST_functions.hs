--------------------------------------------------------------------------------------
--Funciones getters de la tabla de simbolos.
--------------------------------------------------------------------------------------

{-|
   	
   	UNIVERSIDAD SIMON BOLIVAR
    Departamento de Computacion y Tecnologia de la Informacion.
    CI3725 - Traductores e Interpretadores
    Abril - Julio 2015

    AUTORES:
        Edward Fernandez.   Carnet: 10-11121
        Nabil Marquez.      Carnet: 11-10683


	DESCRIPCION: Funciones necesarias para navegar la tabla de simbolos y revisar el
				 uso correcto de tipos.

 -}


module ST_functions (
	build
) where

--------------------------------------------------------------------------------------
--Importaciones requeredias.
--------------------------------------------------------------------------------------

import System.IO
import AST
import SymbolTable
import System.Environment   
import Data.Maybe
import Data.Map (Map)           
import qualified Data.Map as Map 

--------------------------------------------------------------------------------------
--Main
--------------------------------------------------------------------------------------

build :: Inst -> IO()
build a =  printTable $ ((get_sons (fst sT) !! 0),(snd sT))
      where
            isT=create
            errors=""
            sT=checkInst a errors isT False ""
 
--------------------------------------------------------------------------------------
--Funciones auxiliares
--------------------------------------------------------------------------------------

--Obtener tipo de simbolo terminal
get_type :: Terminal -> String
get_type (Canvas' a) = "Canvas"
get_type (Int' a)    = "Int"
get_type (TRUE)      = "Bool"
get_type (FALSE)      = "Bool"

--Obtener Declaraciones a partir de una lista de dedclaraciones tupla (tipo,identificadores)
getDecl :: Decl -> [(String, (String,String))]
getDecl (Decl list) = concatMap ([]++)  [ [( j, ( show (fst i),"NonValue")) | j<- snd i] | i <- list]

--Insertar declaraciones a una tabla de simbolos.
insertDecl :: [(String, (String,String))] -> String -> SymbolTable -> (SymbolTable,String)
insertDecl list errors sT =  
            if list==[] then (sT,errors)
            else insertDecl (tail list) (snd auxsT) (fst auxsT)
            where
              act   = head list
              errors2 = errors
              auxsT = if (isElem (fst act) sT) then 
                        if ((fst(lookupE (fst act) sT))==(fst(snd act))) then
                          ((update ((fst act)) (snd act) sT),errors2)
                        else
                          ((insert ((fst act)) (snd act) sT),errors2++"Error: Inconsistent type declaration. Identifier '"++(fst act)++"' redeclared from '"++(fst(lookupE (fst act) sT))++"' to: '"++(fst(snd act))++"'\n")
                      else
                        ((insert ((fst act)) (snd act) sT),errors2)


--------------------------------------------------------------------------------------
--Funciones de impresión
--------------------------------------------------------------------------------------

--Impresión de fila.
rowPrint :: Int -> String -> String -> String -> String
rowPrint mx a b c= parta++partb++"\n"
                where
                  mx'=mx+1
                  parta="| "++a++(concatMap (++"") (replicate (mx'-(length a)) " "))++" "
                  partb="| "++b++(concatMap (++"") (replicate (mx'-(length b)) " "))++" |"
                  --partc="| "++c++(concatMap (++"") (replicate (mx'-(length c)) " "))++" |"

--Formatear tabla a formato imprimible.
formatTable :: SymbolTable -> String -> String
formatTable a out = 
                  if (isEmpty a) then 
                    if (get_sons a)==[] then out
                    else concatMap ((""++).(++"\n")) (lines (concatMap (++out) [formatTable i "" | i<-(get_sons a)]))
                  else 
                    if (get_sons a)==[] then out ++ concatMap (++"") arrg
                    else (concatMap (++"") arrg) ++ (concatMap (("---"++).(++"\n")) (lines (concatMap (++out) [formatTable i "" | i<-(get_sons a)])))
                  where
                    lista=Map.toList (get_table a)
                    arrg=[rowPrint (get_maxStr a) (fst i) (fst(snd i)) (snd(snd i)) | i <- lista]

--Imprimir información de tabla o, en caso de que los haya, los errores.
printTable :: (SymbolTable,String) -> IO()
printTable (a,b) = if b/=[] then putStr $ "\n" ++ b ++ "\n"
                   else putStr $ "\n"++ (rowPrint (get_maxStr a) "__Identifier__" "__Type__" "__Value__")++ formatTable a "" ++"\n"

--------------------------------------------------------------------------------------
--Chequeo de expresiones. Mientras realiza el chequeo, en caso de conseguir algún error,
----este es concatenado al string de errores entrada.
----Existe un checkExp para cada constructor de Exp
--------------------------------------------------------------------------------------

checkExp :: Exp  -> String ->  SymbolTable -> ((SymbolTable,String),String)
checkExp (Term a) errors sT = ((sT,errors),(get_type a))
checkExp (Var (Var' a)) errors sT = if upperIsE a sT then ((sT,errors),t )
                                    else ((sT,errors'),t)
                                  where
                                    search = upperLookupE a sT
                                    t     = fst search
                                    errors'=errors++"Error: Identifier '"++a++"' not in scope. (NonType)\n"

checkExp (Brack a) errors sT = checkExp a errors sT

checkExp (Trans a) errors sT = if value=="Canvas" then ((sT,errors),value)
                              else ((sT,errors'),"NonType")
                          where 
                            search = checkExp a errors sT
                            value  = snd search
                            errors2=snd $ fst search
                            errors'=errors2++"Error: Operator `'` expected 'Canvas' but recivied '"++value++"'.\n"

checkExp (Rotate a) errors sT = if value=="Canvas" then ((sT,errors),value)
                              else ((sT,errors'),"NonType")
                          where 
                            search = checkExp a errors sT
                            value  = snd search
                            errors2=snd $ fst search
                            errors'=errors2++"Error: Operator `$` expected 'Canvas' but recivied '"++value++"'.\n"

checkExp (HConcat a b) errors sT = if value1=="Canvas" && value2=="Canvas" then ((sT,errors),value2)
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `~` expected 'Canvas Canvas' but recivied '"++value1++" "++value2++"'.\n"

checkExp (VConcat a b) errors sT = if value1=="Canvas" && value2=="Canvas" then ((sT,errors),value2)
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `&` expected 'Canvas Canvas' but recivied '"++value1++" "++value2++"'.\n"

checkExp (CompNQ a b) errors sT = if value1==value2 then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `/=` can't compare '"++value1++"' with '"++value2++"'.\n"

checkExp (CompEQ a b) errors sT = if value1==value2 then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `=` can't compare '"++value1++"' with '"++value2++"'.\n"

checkExp (CompGE a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `>=` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (CompGT a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `>` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (CompLE a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `<=` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (CompLT a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `<` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Negative a) errors sT = if value=="Int" then ((sT,errors),value)
                              else ((sT,errors'),"NonType")
                          where 
                            search = checkExp a errors sT
                            value  = snd search
                            errors2=snd $ fst search
                            errors'=errors2++"Error: Operator `- (Unary)` expected 'Int' but recivied '"++value++"'.\n"

checkExp (Not a) errors sT = if value=="Bool" then ((sT,errors),value)
                              else ((sT,errors'),"NonType")
                          where 
                            search = checkExp a errors sT
                            value  = snd search
                            errors2=snd $ fst search
                            errors'=errors2++"Error: Operator `^` expected 'Bool' but recivied '"++value++"'.\n"

checkExp (And a b) errors sT = if value1=="Bool" && value2=="Bool" then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `/\\` expected 'Bool Bool' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Or a b) errors sT = if value1=="Bool" && value2=="Bool" then ((sT,errors),"Bool")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `\\/` expected 'Bool Bool' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Mod a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Int")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `%` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Div a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Int")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `/` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Times a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Int")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `*` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Mins a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Int")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `-` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

checkExp (Plus a b) errors sT = if value1=="Int" && value2=="Int" then ((sT,errors),"Int")
                              else ((sT,errors'),"NonType")
                          where 
                            search1 = checkExp a errors sT
                            value1  = snd search1
                            search2 = checkExp b (snd (fst search1)) sT
                            value2  = snd search2
                            errors2=snd $ fst search2
                            errors'=errors2++"Error: Operator `+` expected 'Int Int' but recivied '"++value1++" "++value2++"'.\n"

--------------------------------------------------------------------------------------
--Chequeo de instrucciones. Mientras realiza el chequeo, en caso de conseguir algún error,
----este es concatenado al string de errores entrada.
----Existe un checkInst para cada constructor de Inst.
----En el caso de alcance, se crea una nueva tabla hija.
----En el caso de un for, se asume como una nueva tabla cuya unica declaracion en alcance es la variable de iteracion.
--------------------------------------------------------------------------------------

checkInst :: Inst -> String -> SymbolTable  -> Bool -> String -> (SymbolTable,String)
checkInst (Alcance (a,b)) errors sT t vari= ((fst sTfinal'),(snd sTfinal'))
            where
              listDecl= getDecl a
              listInst= b
              sT'     = create
              aux     = cDad sT' sT
              sTdecl  = insertDecl listDecl errors aux 
              sTdecl' = (cDad (fst sTdecl) sT,(snd sTdecl))
              sTfinal = checkListInst listInst (snd sTdecl') (fst sTdecl') False ""
              sTfinal' = ((addSon sT (fst sTfinal)),(snd sTfinal))

checkInst (Assign a b) errors sT t vari = if vart=="NonType" then (sT,errors5)
                                 else if et=="NonType" then (sT,errors5)
                                      else if et==vart then (sT,errors5)
                                           else (sT,errors5++"Error: Indentifier '"++a++"' expects '"++vart++"' but recivied '"++et++"' insted.\n")
                            where
                              vart    = fst (upperLookupE a sT)
                              search = checkExp b errors sT
                              et  = snd search
                              errors2 = snd $ fst search
                              errors3 = if vart=="NonType" then errors2++"Error: Identifier '"++a++"' not in scope. (NonType)\n"
                                        else errors2
                              errors4 = if et=="NonType" then errors3++"Error: Trying to assing an unespecified type value from expression to identifier '"++a++"'.\n"
                                        else if vart=="NonType" then errors3++"Error: Can't assing value to undeclared identifier '"++a++"'.\n"
                                             else errors3
                              errors5 = if t then 
                                            if a==vari then errors4++"Error: Trying to change value of iterator identifier '"++a++"'.\n" 
                                            else errors4
                                        else errors4

checkInst (Scan a) errors sT t vari = (sT,errors2)
                            where
                              vart    = fst (upperLookupE a sT)
                              errors2 = if vart=="NonType" then errors++"Error: Identifier '"++a++"' not in scope. (NonType)\n"
                                        else if vart/="Int" && vart/="Bool" then errors++"Error: `read` expects 'Bool' or 'Int' but recivied '"++vart++"'.\n"
                                             else errors

checkInst (Print a) errors sT t vari = (sT,errors2)
                            where
                              expT    = checkExp a errors sT
                              errors2 = if (snd expT)/="Canvas" then (snd (fst expT))++"Error: `write` expects 'Canvas' but recivied '"++(snd expT)++"'.\n"
                                             else (snd (fst expT))

checkInst (Ifthen a b) errors sT t vari = sTfin
                            where
                              expT    = checkExp a errors sT
                              errors2 = if (snd expT)/="Bool" then (snd (fst expT))++"Error: if conditional expects `Bool` but recivied '"++(snd expT)++"'.\n"
                                             else (snd (fst expT))
                              sTfin   = checkListInst b errors2 sT False ""

checkInst (Ifelse a b c) errors sT t vari = sTfin
                            where
                              expT    = checkExp a errors sT
                              errors2 = if (snd expT)/="Bool" then (snd (fst expT))++"Error: if conditional expects `Bool` but recivied '"++(snd expT)++"'.\n"
                                             else (snd (fst expT))
                              sTfin1   = checkListInst b errors2 sT False ""
                              sTfin   = checkListInst c (snd sTfin1) (fst sTfin1) False ""

checkInst (While a b) errors sT t vari = sTfin
                            where
                              expT    = checkExp a errors sT
                              errors2 = if (snd expT)/="Bool" then (snd (fst expT))++"Error: While loop condition expects `Bool` but recivied '"++(snd expT)++"'.\n"
                                             else (snd (fst expT))
                              sTfin   = checkListInst b errors2 sT False ""

checkInst (While2 a b c) errors sT t vari = sTfin
                            where
                              expT1    = checkExp a errors sT
                              expT2    = checkExp b (snd (fst expT1)) (fst (fst expT1))
                              errors2 = if (snd expT1)/="Int" || (snd expT2)/="Int" then (snd (fst expT2))++"Error: While loop limits expects `Int Int` but recivied '"++(snd expT1)++" "++(snd expT2)++"'.\n"
                                             else (snd (fst expT2))
                              sTfin   = checkListInst c errors2 (fst (fst expT2)) False ""

checkInst (For a b c d) errors sT t vari = ((fst sTfinal'),(snd sTfinal'))
                            where
                              sT'     = create
                              aux     = cDad sT' sT
                              stN     = insert a ("Int","NonValue") aux
                              vart    = fst (upperLookupE a stN)
                              errors1 = if vart=="NonType" then errors++"Error: Identifier '"++a++"' not in scope. (NonType)\n"
                                        else if vart/="Int" then errors++"Error:  While loop identifier '"++a++"' expects 'Int' but recivied '"++vart++"'.\n"
                                             else errors
                              expT1    = checkExp b errors1 stN
                              expT2    = checkExp c (snd (fst expT1)) (fst (fst expT1))
                              errors2 = if (snd expT1)/="Int" || (snd expT2)/="Int" then (snd (fst expT2))++"Error: While loop limits expects `Int Int` but recivied '"++(snd expT1)++" "++(snd expT2)++"'.\n"
                                             else (snd (fst expT2))
                              sTfinal   = checkListInst d errors2 (fst (fst expT2)) True a
                              sTfinal' = ((addSon sT (fst sTfinal)),(snd sTfinal))


checkListInst :: [Inst] -> String -> SymbolTable  -> Bool -> String -> (SymbolTable,String)
checkListInst list errors sT t vari= if list==[] then (sT,errors)
                               else checkListInst (tail list) (snd sT') (fst sT') t vari
                          where
                            sT'=checkInst (head list) errors sT t vari

--------------------------------------------------------------------------------------