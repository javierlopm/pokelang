
# Planificacion de registros

| Registros| Uso                        |
|:--------:|----------------------------|
|$zero     | Reservado                  |
|$r1       | Reservado                  |
|$v0-$v1   | Reservado para syscalls    |
|$a0-$a3   | Reservado para syscalls    |
|$t0-$t4   | Epilogo y prologo de llamadas|
|$t5-$t7   | Uso general, expresiones   |
|$s0-$s8   | Uso general, expresiones   |
|$k0,$k1   | Reservado para OS          |
|$gp       | Reservado para globales    |
|$sp       | Reservado para stackpointer|
|$fp       | Reservado para frame pointer|
|$ra       | Reservado para globales        |
|$lo $hi ? | Reservado para multiplicaciones|
|$f12      | Reservado para print de float  |
|$f0-$f11  | Uso general, expresiones floats|
|$f13-$f32 | Uso general, expresiones floats|

|Operacion        | 3/2 reg | 2 registros + constante|
|-----------------|:-----:|:-------------------:|
| Addi     D S1 S2 | add r1 r2 r3 | (addi r1 r2 #c) o (li r3 #c; add ) |
| Subi     D S1 S2 | sub r1 r2 r3 | (subi r1 r2 #c) o (li r3 #c; sub ) |
| Divi     D S1 S2 | (pseudo) div r1 r2 r3  | (li r3 #c; div )    |
| Mod      D S1 S2 | (div r2 r3);(mv r1 $lo)| (li r3 #c; div; mv) |
| Multi    D S1 S2 | (pseudo) mulo r1 r2 r3 | (li r3 #c; mulo )   |
| Pot      D S1 S2 | - | - |
| Addf     D S1 S2 | add.d r1 r2 r3 | load float |
| Subf     D S1 S2 | mul.s r1 r2 r3 |  |
| Divf     D S1 S2 | div.s r1 r2 r3 |  |
| Multf    D S1 S2 | |  |
| And      D S1 S2 | |  |
| Or       D S1 S2 | |  |
| XOr      D S1 S2 | |  |

| Negai    D S1    | (pseudo) Neg r1 r2 | - |
| Negaf    D S1    | |  |
| Eql      D S1 S2 | - | - |
| NotEql   D S1 S2 | - | - |
| Not      D S1    | - | - |
| ShiftL   D1 S2 C3| - | sll R1, R2, #C3 |
| ShiftR   D1 S2 C3| |  |
| Lt      D S1 S2
| Gt      D S1 S2
| LEq     D S1 S2  
| GEq     D S1 S2 
| Jump     Label
| Jz       S1 Label
| Jnotz    S1 Label
| JLt      S1 S2 Label
| JGt      S1 S2 Label
| JLEq     S1 S2 Label
| JGEq     S1 S2 Label
| JEq      S1 S2 Label
| JNEq     S1 S2 Label
| Mv           D S1
| ReadPointer  D S1
| StorePointer D S1
| ReadArray    D S1 S2
| StoreArray   D S1 S2
| TACCall    String  Int
| Clean   Int
| Param   S1    
| Return  S1 
| Extras
| Comment String
| Tag     Label
| TagS    String
| TagSC   String String
| Nop 
| TacExit 
| Print     S1        
| PrintEnum String S1 