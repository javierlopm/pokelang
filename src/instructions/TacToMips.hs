module TacToMips(module TacToMips) where

import Data.Word
import Data.Sequence
import Data.Foldable(foldr)

type Mips = String
type Register = Word
{-
$zero reservado
-------------------------------------
$v0-$v1 | reservado para syscalls
$a0-$a3 | reservado para syscalls
-------------------------------------
$t0-$t4 | epilogo y prologo de llamadas
$t5-$t7 | uso general, expresiones
$s0-$s7 | uso general, expresiones
-------------------------------------
$k0,$k1 | reservado para OS
-------------------------------------
$gp     | reservado para globales
$sp     | reservado para stackpointer
$fp     | reservado para frame pointer
$ra     | reservado para globales

$f12      | reservado para print de float
$f0-$f11  | uso general, expresiones floats
$f13-$f32 | uso general, expresiones floats
-}

crt :: Mips
crt = "\
0print_str:       \
    lw $a0,0($fp) \
    li $v0,4      \
    syscall       \
    jr $ra        \
0read_int:        \
    li $v0,5      \
    syscall       \
    lw $a0,0($fp) \
    st $v0,0($a0) \
    jr $ra        \
0read_float:      \
    li $v0,6      \
    syscall       \
    lw $a0,0($fp) \
    st $f0,0($a0) \
    jr $ra        \
0read_char:       \
    lw $a0,0($fp) \
    li $a1,1      \
    li $v0,8      \
    syscall       \
    jr $ra
"

compile :: Program -> Program -> Mips
compile globs program = ".data\n"   ++ translate globs      ++ 
                        "\n.text\n" ++ translate programcrt ++ 
                        "\n"        ++ crt

translate :: Program -> Mips
translate  = undefined

template :: Program -> [Register] -> Mips
template (Addi     Dest Src1 Src2)  = "addi " 
template (Subi     Dest Src1 Src2)  = ""
template (Divi     Dest Src1 Src2)  = ""
template (Mod      Dest Src1 Src2)  = ""
template (Multi    Dest Src1 Src2)  = ""
template (Pot      Dest Src1 Src2)  = ""
template (Negai    Dest Src1     )  = ""