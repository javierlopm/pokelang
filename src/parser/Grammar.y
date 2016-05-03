{
module Grammar(parser) where
import Tokens
}


%name parser
%tokentype { Token      }
%error     { parseError }

%token
    ID        { TkId       _ _ }     
    DATAID    { TkDId      _ _ }         
    INTDEC    { TkInt      _ } 
    BOOLDEC   { TkBool     _ }     
    CHARDEC   { TkChar     _ }     
    VOIDDEC   { TkVoid     _ }     
    FLOATDEC  { TkFloat    _ }    
    STRUCTDEC { TkStruct   _ }   
    UNIONDEC  { TkUnion    _ }     
    ENUMDEC   { TkEnum     _ }     
    GLOBAL    { TKGlobal   _ }     
    ';'       { TkSColon   _ }     

%%

Prog : Declarations  { $1 }
     

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