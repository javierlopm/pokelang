
# Planificacion de registros

 Registros| Uso
----------+----------------------------
$zero     | Reservado
$v0-$v1   | Reservado para syscalls
$a0-$a3   | Reservado para syscalls
$t0-$t4   | Epilogo y prologo de llamadas
$t5-$t7   | Uso general, expresiones
$s0-$s7   | Uso general, expresiones
$k0,$k1   | Reservado para OS
$gp       | Reservado para globales
$sp       | Reservado para stackpointer
$fp       | Reservado para frame pointer
$ra       | Reservado para globales
$f12      | Reservado para print de float
$f0-$f11  | Uso general, expresiones floats
$f13-$f32 | Uso general, expresiones floats

Operacion | 2 reg | Registro + constante
----------+-------+--------------------
Addi     Dest Src1 Src2 |  |  
Subi     Dest Src1 Src2 |  |
Divi     Dest Src1 Src2 |  |
Mod      Dest Src1 Src2 |  |
Multi    Dest Src1 Src2 |  |
Pot      Dest Src1 Src2 |  |
Negai    Dest Src1
-- Float Operations over Ints
| Addf     Dest Src1 Src2 
| Subf     Dest Src1 Src2
| Divf     Dest Src1 Src2
| Multf    Dest Src1 Src2
| Negaf    Dest Src1
-- Logic bitwise operations
| And      Dest Src1 Src2  
| Or       Dest Src1 Src2
| XOr      Dest Src1 Src2
| Eql      Dest Src1 Src2
| NotEql   Dest Src1 Src2
| Not      Dest Src1
| ShiftL   Dest Src1 Word -- shifts are always postive constants
| ShiftR   Dest Src1 Word 
-- Compare integers
| Lt      Dest Src1 Src2
| Gt      Dest Src1 Src2
| LEq     Dest Src1 Src2  -- lower or equal
| GEq     Dest Src1 Src2  -- equal 
-- Jumps
| Jump     Label
| Jz       Src1 Label
| Jnotz    Src1 Label
| JLt      Src1 Src2 Label
| JGt      Src1 Src2 Label
| JLEq     Src1 Src2 Label
| JGEq     Src1 Src2 Label
| JEq      Src1 Src2 Label
| JNEq     Src1 Src2 Label
-- Copy / Read / Store
| Mv           Dest Src1
| ReadPointer  Dest Src1
| StorePointer Dest Src1
| ReadArray    Dest Src1 Src2
| StoreArray   Dest Src1 Src2
-- Function calls
| TACCall    String  Int
| Clean   Int
| Param   Src1    
-- | Return  Src1 
-- Extras
| Comment String
| Tag     Label
| TagS    String
| TagSC   String String
| Nop 
| TacExit 
-- Prints
| Print     Src1        -- Print Integer constant
| PrintEnum String Src1 -- Print enum at label