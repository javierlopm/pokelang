module TacToMips(
    
) where

type Mips = String
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
0print_str:
    lw $a0,4($fp)
    li $v0,4
    syscall
    jr $ra

0read_int:
    lw $a0,0($fp)
    li $v0
    jr $ra
0read_float:
0read_char:
"

compile :: Program -> Program -> Mips
compile globs program = ".data\n"   ++ translate globs      ++ 
                        "\n.text\n" ++ translate programcrt ++ 
                        "\n"        ++ crt

translate :: Program -> Mips
translate = undefined