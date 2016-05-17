{
module Grammar(parser,ScopeNZip(..),makeTable,initialState) where
import Tokens
import TableTree
import Types
import Data.Maybe(fromJust)
import Control.Monad.RWS.Strict
import qualified Data.Sequence as S

}


%name parser
%tokentype { Token }

%monad { RWS String (S.Seq(Message)) ScopeNZip }

%error     { parseError }
%token
    ID        { TkId       _ _ }     
    DATAID    { TkDId      _ _ }
    -- Declarations       
    INTDEC    { TkInt      _ } 
    BOOLDEC   { TkBool     _ }     
    CHARDEC   { TkChar     _ }     
    VOIDDEC   { TkVoid     _ }     
    FLOATDEC  { TkFloat    _ }    
    STRUCTDEC { TkStruct   _ }   
    UNIONDEC  { TkUnion    _ }     
    ENUMDEC   { TkEnum     _ }     
    GLOBAL    { TKGlobal   _ }

    "["       { TkLBracket _ }     
    "]"       { TkRBracket _ }     
    "{"       { TkLCurly   _ }     
    "}"       { TkRCurly   _ }     
    "("       { TkLRound   _ }     
    ")"       { TkRRound   _ }     
    "|"       { TkPipe     _ }     
    "::"      { TkDColon   _ }     
    ":"       { TkColon    _ }     
    ";"       { TkSColon   _ }    
    ","       { TkComma    _ }    
    "*="      { TkTEQ      _ }    
    "+="      { TkPEQ      _ }    
    "."       { TkDot      _ }    
    "!"       { TkExcMark  _ }    
    "!!"      { TkExcArr   _ }    
    "!="      { TkNEQ      _ }    
    "&&"      { TkDAmp     _ }    
    "||"      { TkPOr      _ }    
    AND       { TkAnd      _ }    
    OR        { TkOr       _ }    
    --"?"       { TkDEQ      _ }    
    ">="      { TkGE     _ }    
    "<="      { TkLE     _ }    
    ">"       { TkGT     _ }    
    "<"       { TkLT     _ }    
    "/"       { TkIDiv   _ }    
    "//"      { TkDiv    _ }    
    "+"       { TkSum    _ }    
    "-"       { TkMin    _ }    
    "^"       { TkPower  _ }    
    "*"       { TkTimes  _ }    
    "%"       { TkMod    _ }    
    "=="      { TkEq     _ }    
    "="       { TkAssign _ }

    FUNC      { TkFunc   _ }

    -- Control structures
    IF        { TkIf       _ }
    ELIF      { TkElif     _ }
    ELSE      { TkElse     _ }
    END       { TkEnd      _ }
    WHILE     { TkWhile    _ }
    FOR       { TkFor      _ }
    BEGIN     { TkBegin    _ }
    BREAK     { TkBreak    _ }
    CONTINUE  { TkContinue _ }
    RETURN    { TkReturn   _ }
    EXIT      { TkExit     _ }

    -- Built-in functions/Instructions
    READ      { TkRead   _ }
    WRITE     { TkWrite  _ }
    PRINT     { TkPrint  _ }
    MALLOC    { TkAlloc  _ }
    FREE      { TkFree   _ }
    SIZEOF    { TkSizeOf _ }
    GET       { TkGet    _ }
    -- Primitive types
    TRUE      { TkTrue      _   }
    FALSE     { TkFalse     _   }
    CHAR      { TkCharVal   _ _ }
    STRING    { TkString    _ _ }
    INT       { TkNum       _ _ }
    FLOAT     { TkFloatVal  _ _ }

    -- Composed types
    ENUM { TkEnumCons _ _ }

-- Para las expresiones relacionales.
--%nonassoc '<' <\=' '>' '>\=' '=' '\/=' '..'


-- Para los booleanos.
%left  OR
%left  AND
%left  "||"
%left  "&&"
%right "!"

-- Para los enteros.
%left "+" "-"
%left "*" "/" "//" "%"
%left "^"
%right NEG      -- Para el - unario.

--Strucs Union y Arreglos
%left "!!"   
%left "."  
%left ARR

--Llamadas a funciones
%left CALL

--Acceso a apuntadores
%right POINT

%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="

%%

Prog : Dcls  {% return ()}

Ins : {- λ -}                                 {% return () }
    | Ins PRINT "(" STRING PrntArgs ")"   ";" {% onScope $ insert ("_"++(content $4)) (Cons (position $4)) }
    | Ins READ  "("       ID        ")"   ";" {% return ()}
    | Ins WRITE "("       ID        ")"   ";" {% return ()}
    | Ins Exp "=" Exp         ";"         {% return ()}
    | Ins Exp "*=" Exp        ";"         {% return ()}
    | Ins Exp "+=" Exp        ";"         {% return ()}
    | Ins BREAK             ";"         {% return ()}
    | Ins CONTINUE          ";"         {% return ()}
    | Ins RETURN   Exp      ";"         {% return ()}
    | Ins EXIT              ";"         {% return ()}
    | Ins FREE "("ID")"     ";"         {% return ()}
    | Ins FREE "("DATAID")" ";"         {% return ()}
    | Ins READ "("DATAID")" ";"         {% return ()}
    | Ins IF Exp    ":" Ent SmplDcls Ins NextIf Else END            {% exitScope  }
    | Ins WHILE Exp ":" Ent SmplDcls Ins END                        {% exitScope  }
    | Ins FOR ID "=" Exp  "|" Exp "|" Exp ":" Ent SmplDcls Ins  END {% exitScope  }
    | Ins FOR ID "=" Exp  "|" Exp         ":" Ent SmplDcls Ins  END {% exitScope  }
    | Ins FOR ID "=" ENUM "|" ENUM        ":" Ent SmplDcls Ins  END {% exitScope  }
    | Ins BEGIN Ent SmplDcls Ins END                                {% exitScope  } -- No debe aceptar funciones

PrntArgs: {- λ -}             {% return ()}
        | PrntArgs "," Exp    {% return ()} -- Siempre es necesaria una coma a la izq

NextIf: {- λ -}             {% return ()}
      | NextIf ELIF  Exp ":" Ent SmplDcls Ins {% exitScope  }

Else: {- λ -}      {% return ()}
    | ELSE ":" Ent SmplDcls Ins {% exitScope  }


SmplDcls: {- λ -}                                     {% return () }        
    | SmplDcls IsGlob PrimType Ptrs          ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $5) $4 Nothing) $5  $2}
    | SmplDcls IsGlob DataType DATAID  Ptrs  ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $6) $5 (Just (lexeme $4))) $6  $2}
    | SmplDcls IsGlob PrimType EmptyArrs     ID  ";"  {% insertDeclareInScope (makePtrs $3 (position $5) $4 Nothing) $5  $2} -- Azucar sintactico? jeje
    | SmplDcls IsGlob PrimType StaticArrs    ID  ";"  {% insertDeclareInScope (makeArr  $3 (position $5) $4) $5  $2}
    | SmplDcls IsGlob PrimType               ID  ";"  {% insertDeclareInScope (makeDec  $3 (position $4) Nothing) $4  $2}
    | SmplDcls IsGlob DataType DATAID        ID  ";"  {% insertDeclareInScope (makeDec  $3 (position $5) (Just (lexeme $4))) $5  $2}

Dcls:  {- λ -}                                {% return ()}
    | Dcls FUNC PrimType ID "(" Parameters ")" ":" SmplDcls Ins END {% insertFunction $3 $4 }
    | Dcls PrimType Ptrs       ID  ";"  {% return ()}
    | Dcls PrimType EmptyArrs  ID  ";"  {% return ()}
    | Dcls PrimType StaticArrs ID  ";"  {% return ()}
    | Dcls PrimType            ID  ";"   {% return ()}
  --| Dcls DataType    DATAID     ";"          {% return ()}    -- Forward declarations
    | Dcls ENUMDEC DATAID   "{" EnumConsList "}"   {% return ()}
    | Dcls STRUCTDEC  DATAID  "{" FieldsList   "}"   {% return ()}
    | Dcls UNIONDEC  DATAID  "{" FieldsList   "}"   {% return ()}

IsGlob : {- λ -}     { False }
         | GLOBAL    { True  }

PrimType : INTDEC         { $1 }
         | BOOLDEC        { $1 }
         | CHARDEC        { $1 }
         | VOIDDEC        { $1 }
         | FLOATDEC       { $1 }

DataType : ENUMDEC        { $1 }
         | STRUCTDEC      { $1 }
         | UNIONDEC       { $1 }

Parameter: ListParam PrimType ID             {% return ()}
         | ListParam PrimType Ptrs ID        {% return ()}
         | ListParam PrimType EmptyArrs ID   {% return ()}
         | ListParam DataType DATAID         {% return ()}

ListParam: {- λ -}                              {% return ()}
         | ListParam PrimType ID     ","        {% return ()}
         | ListParam DataType DATAID ","        {% return ()}
         | ListParam PrimType Ptrs ID ","       {% return ()}
         | ListParam PrimType EmptyArrs ID ","  {% return ()}

Parameters: {- λ -}       {% onZip enterScope } -- Tiene sentido?
          | Parameter     {% onZip enterScope } 

EnumConsList: ENUM                      {% return ()}
            | EnumConsList "," ENUM     {% return ()}

FieldsList  : ID     "::" PrimType                   {% return ()}
            | DATAID "::" DATAID                     {% return ()}
            | FieldsList  "," ID     "::" PrimType   {% return ()}
            | FieldsList  "," DATAID "::" DATAID     {% return ()}

-- Counts nesting levels
Ptrs: "*"        {    1    }
    | Ptrs "*"   { succ $1 } 

-- Counts nesting levels
EmptyArrs: "[" "]"             {    1    }
         |  EmptyArrs "[" "]"  { succ $1 }

-- Counts dimensions
StaticArrs: "[" INT "]"             { [value $2] }
          | StaticArrs "[" INT "]"  { (value $3):$1}

Exp : 
    -- Expresiones Aritméticas.
      Exp "+" Exp       {% return ()}
    | Exp "-" Exp       {% return ()}
    | Exp "^" Exp       {% return ()}
    | Exp "*" Exp       {% return ()}
    | Exp "/" Exp       {% return ()}
    | Exp "//" Exp      {% return ()}
    | Exp "%" Exp       {% return ()}
    | "-" Exp %prec NEG {% return ()}
    -- Expresiones Booleanas.
    | Exp OR Exp        {% return ()}
    | Exp "||" Exp      {% return ()}
    | Exp AND Exp       {% return ()}
    | Exp "&&" Exp      {% return ()}
    | "!" Exp         {% return ()}
    -- Expresiones relacionales.
    | Exp "<"  Exp      {% return ()}
    | Exp "<=" Exp      {% return ()}
    | Exp ">"  Exp      {% return ()}
    | Exp ">=" Exp      {% return ()}
    | Exp "==" Exp      {% return ()}
    | Exp "!=" Exp      {% return ()}
    -- Expresiones sobre lienzo.
    | Exp "!!" Exp           {% return ()}
    | Exp "."  Exp           {% return ()}
    | ID "[" Exp "]" %prec ARR {% return ()}
    --Llamadas a funciones
    | ID "(" Exp ")"         {% return ()}
    --Acceso a apuntadores
    | "*" Exp %prec POINT  {% return ()}
    -- Asociatividad.
    | "(" Exp ")"    {% return ()}
    -- Constantes.
    | Term           {% return ()}
    -- Llamadas
    | MALLOC "(" Exp ")"       {% return ()}
    | SIZEOF "(" Exp ")"       {% return ()}
    | SIZEOF "(" PrimType ")"  {% return ()}
    | GET    "(" ENUM ")"      {% return ()}

Term: TRUE         {   $1   }
    | FALSE        {   $1   }
    | ID           {   $1   }
    | DATAID       {   $1   }
    | FLOAT        {   $1   }
    | INT          {   $1   }
    | CHAR         {   $1   }

Ent : {- λ -}     {% onZip enterScope }

{

type OurMonad    = RWS String (S.Seq(Message)) ScopeNZip
type SymTable    = Scope Declare  
type TableZipper = Zipper Declare 

-- state from monad State
data ScopeNZip = ScopeNZip { scp      :: SymTable
                           , zipp     :: TableZipper
                           , constGen :: Int } 
                           deriving (Show)

initialState :: ScopeNZip
initialState = ScopeNZip emptyScope (fromScope emptyScope) 0

makeTable :: ScopeNZip -> Scope Declare
makeTable (ScopeNZip s z _) = fuse s z

onZip :: (TableZipper -> TableZipper ) -> OurMonad()
onZip fun = do zipper <- gets zipp
               state  <- get
               put state { zipp = fun zipper }


onScope :: (SymTable -> SymTable) -> OurMonad ()
onScope fun = do scope <- gets scp
                 state <- get
                 put state { scp = fun scope }

succCons ::  OurMonad ()
succCons = do conG  <- gets constGen
              state <- get
              put state { constGen = succ conG }

exitScope :: OurMonad ()
exitScope = onZip (fromJust.goUp)

insertFunction :: Token -> Token -> OurMonad ()
insertFunction typ ident  = do 
    state <- get
    if isMember ((fromScope.scp) state) (lexeme ident)
        then tell error1
        else do tell whathappened
                onScope $ insert (lexeme ident) 
                                 (Function (position ident) 
                                           (makeType typ) 
                                           (fromZipper (zipp state)))
                onZip (const (fromScope emptyScope)) -- Clean zipper
  where error1       = S.singleton $ Left  $ "Error:" ++ linecol ++" redeclaraci'o de " ++ lexeme ident
        whathappened = S.singleton $ Right $ "Agregada la funcion " ++ lexeme ident ++ " en "++ linecol
        linecol      = (show.fst.position) ident ++":"++(show.snd.position) ident 

--se deberia verificar que el valor no esta ya en alguna constante
addStr declare = do id  <- gets constGen
                    succCons
                    onScope $ insert (show id) declare 

insertDeclareInScope :: Maybe Declare -> Token -> Bool -> OurMonad ()
insertDeclareInScope Nothing (TkId (l,c) lexeme ) _ =
    tell $ S.singleton $ Left  $ 
    "Error:" ++show l++":"++show c ++" "  ++ lexeme ++ 
    " es del tipo VOIDtorb, el cual solo puede ser instanciado como referencia."
insertDeclareInScope (Just dcltype) (TkId (l,c) lexeme ) isGlob = do 
    table <- get 
    if isMember (zipp table) lexeme
        then tell error1
        else if isGlob 
                then onScope $ insert lexeme dcltype 
                else onZip $ apply  $ insert lexeme dcltype       
    tell whathappened
    where error1       = S.singleton $ Left  $ "Error:" ++show l++":"++show c ++" redeclaraci'o de " ++ lexeme
          whathappened = S.singleton $ Right $ "Agregado " ++ lexeme ++ " en "++show l++":"++show c

  

parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Parsing error at: \n" ++ show (head l)

parse [] = print "Hola"
parse l = print "Hola"

}