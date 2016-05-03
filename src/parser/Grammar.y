{
module Grammar(parser) where
import Tokens
}


%name parser
%tokentype { Token      }
%error     { parseError }

%token
    ID        { TkDId     _ $$ }     
    DATAID    { TkId      _ $$ }         
    INTDEC    { TkInt     $$ } 
    BOOLDEC   { TkBool    $$ }     
    CHARDEC   { TkChar    $$ }     
    VOIDDEC   { TkVoid    $$ }     
    FLOATDEC  { TkFloat   $$ }    
    STRUCTDEC { TkStruct  $$ }   
    UNIONDEC  { TkUnion   $$ }     
    ENUMDEC   { TkEnum    $$ }     
    GLOBAL    { TKGlobal  $$ }     
    ';'       { TkColon   $$ }     

%%

Prog : Declarations  { [] }
     

Declarations: {- empty -}                                     { [] }        
            | Declarations IsGlobal PrimitiveType ID     ';'  { [] }
            | Declarations IsGlobal DataType      DATAID ';'  { [] }

IsGlobal : {- empty -} { True  }
         | GLOBAL      { False }

PrimitiveType : INTDEC         { [] }
              | BOOLDEC        { [] }
              | CHARDEC        { [] }
              | VOIDDEC        { [] }
              | FLOATDEC       { [] }

DataType : STRUCTDEC      { [] }
         | UNIONDEC       { [] }
         | ENUMDEC        { [] }


{

parseError [] = error $ "EOF Inesperado"
parseError l  = error $ "Error de parseo en " ++ show (head l)

}