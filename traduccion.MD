
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

|Operacion        | 3/2 reg | 2 registros + constante|
|:-----------------|:-----|:-------------------|
| Addi     Dx Sy Sz | add rx, ry, rz | li r3 #c <br> addi rx, ry, rz |
| Subi     Dx Sy Sz | sub rx, ry, rz | li r3 #c <br> sub rx, ry, rz  |
| Divi     Dx Sy Sz | (pseudo) div rx, ry, rz  | (li r3 #c; div )    |
| Mod      Dx Sy Sz | (div r2 r3); mflo r1 | (li r3 #c; div; mv) |
| Multi    Dx Sy Sz | (pseudo) mulo rx, ry, rz | (li r3 #c; mulo )   |
| Pot      Dx Sy Sz | - 1* | - |
| Addf     Dx Sy Sz | add.d rx, ry, rz | lwcp1 primero |
| Subf     Dx Sy Sz | sub.s rx, ry, rz | lwcp1 primero |
| Divf     Dx Sy Sz | div.s rx, ry, rz | lwcp1 primero |
| Multf    Dx Sy Sz | mul.s rx, ry, rz | lwcp1 primero |
| And      Dx Sy Sz | |  |
| Or       Dx Sy Sz | |  |
| XOr      Dx Sy Sz | |  |

| Negai    Dx Sy    | (pseudo) neg   rx, ry | - |
| Negaf    Dx Sy    | neg.s rx, ry | - |
| Eql      Dx Sy Sz | - | - |
| NotEql   Dx Sy Sz | - | - |
| Not      Dx Sy    | - | - |
| ShiftL   Dx Sy Cz | - | sll rx, ry, #cz |
| ShiftR   Dx Sy Cz | - | sra rx, ry, #cz |
| Lt      Dx Sy Sz
| Gt      Dx Sy Sz
| LEq     Dx Sy Sz  
| GEq     Dx Sy Sz 
| Jump     Label
| Jz       Sx Label
| Jnotz    Sx Label
| JLt      Sx Sy Label
| JGt      Sx Sy Label
| JLEq     Sx Sy Label
| JGEq     Sx Sy Label
| JEq      Sx Sy Label
| JNEq     Sx Sy Label

|Operacion          | Memoria/reg    | Reg/Reg       | Mem/Mem|
|-------------------|:--------------:|:-------------:|:------:|
| Mv           Dx Sy | lw R0,lb  | (pseudo) move R0 R1 | lw R0,lb1; sw R0, lb2|
| ReadPointer  Dx Sy | lw R0,lb  | lw R0,0(R1)         | lw R0,lb1; sw R0, lb2|
| StorePointer Dx Sy | sw R0,lb  | sw R0,0(R1)         | lw R1,lb2; sw R0, lb1|
| ReadArray    Dx Sy Sz | la r1,lb ; add r1,r1,r2; lw r0,0(r1) | - | -|
| StoreArray   Dx Sy Sz | la r0,lb ; add r0,r0,r1; sw r2,0(r0) | - | -|




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
