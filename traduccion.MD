
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
|$f12      | Reservado para print de float  |
|$f0-$f11  | Uso general, expresiones floats|
|$f13-$f32 | Uso general, expresiones floats|

|Operacion        | 3 reg | 2 registros + constante|
|-----------------|:-----:|:-------------------:|
| Addi     D S1 S2 | add r1 r2 r3 | (addi r1 r2 #c) o (li r3 #c; add ) |
| Subi     D S1 S2 | sub r1 r2 r3 | (subi r1 r2 #c) o (li r3 #c; sub ) |
| Divi     D S1 S2 | (pseudo) div r1 r2 r3  | (li r3 #c; div )    |
| Mod      D S1 S2 | (div r2 r3); mflo r1 | (li r3 #c; div; mv) |
| Multi    D S1 S2 | (pseudo) mulo r1 r2 r3 | (li r3 #c; mulo )   |
| Pot      D S1 S2 | - 1* | - |
| Addf     D S1 S2 | add.d r1 r2 r3 | lwcp1 primero |
| Subf     D S1 S2 | sub.s r1 r2 r3 | lwcp1 primero |
| Divf     D S1 S2 | div.s r1 r2 r3 | lwcp1 primero |
| Multf    D S1 S2 | mul.s r1 r2 r3 | lwcp1 primero |
| And      D S1 S2 | |  |
| Or       D S1 S2 | |  |
| XOr      D S1 S2 | |  |

| Negai    D S1    | (pseudo) Neg r1 r2 | - |
| Negaf    D S1       | |  |
| Eql      D S1 S2       | |  |
| NotEql   D S1 S2       | |  |
| Not      D S1       | |  |
| ShiftL   D S1 Word        | |  |
| ShiftR   D S1 Word        | |  |
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

|Operacion          | Memoria/reg    | Reg/Reg       | Mem/Mem|
|-------------------|:--------------:|:-------------:|:------:|
| Mv           D S1 | lw R0,lb  | (pseudo) move R0 R1 | lw R0,lb1; sw R0, lb2|
| ReadPointer  D S1 | lw R0,lb  | lw R0,0(R1)         | lw R0,lb1; sw R0, lb2|
| StorePointer D S1 | sw R0,lb  | sw R0,0(R1)         | lw R1,lb2; sw R0, lb1|
| ReadArray    D S1 S2 | la r1,lb ; add r1,r1,r2; lw r0,0(r1) | - | -|
| StoreArray   D S1 S2 | la r0,lb ; add r0,r0,r1; sw r2,0(r0) | - | -|




| TACCall    String  Int | ?
| Clean   Int            | ?
| Param   S1             | ?
| Return  S1 
| Extras

| Comment String | # str
| Tag     Label  | n_tag:
| TagS    String | 0_str:
| TagSC   String String | DUNNO
| Nop            | nop
| TacExit        | 
| Print     S1        | implemtaci'on en crt
| PrintEnum String S1 | implemtaci'on en crt

* 1: debe ser eliminado del Tac y tansformado en un ciclo de multiplicaciones o hacer uso del siguiente fragmento

```
t1=x^y
    li $t2, #y
tag_i:
 mult $t2,$t2,$t0
 add $t1,$t1,-1
 bneqz $t1, tag_i
tag_i_fn:
  ld $t0, $t1
```
