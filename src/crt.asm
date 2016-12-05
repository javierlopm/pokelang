vamo_a_imprimi:
    lw $a0,0($fp)
    li $v0,4
    syscall
    jr $ra
# idioteces mal escritas
vamo_a_imprimi_i:
    lw $a0,0($fp)
    li $v0,4
    syscall
    jr $ra
vamo_a_imprimi_f:
    lw $a0,0($fp)
    li $v0,4
    syscall
    jr $ra
vamo_a_imprimi_c:
    lw $a0,0($fp)
    li $v0,4
    syscall
    jr $ra
# idioteces end
vamo_a_lee_int:       
    li $v0,5
    syscall
    lw $a0,0($fp)
    st $v0,0($a0)
    jr $ra
vamo_a_lee_float:
    li $v0,6
    syscall
    lw $a0,0($fp)
    st $f0,0($a0)
    jr $ra
vamo_a_lee_char:
    lw $a0,0($fp)
    li $a1,1
    li $v0,8
    syscall
    jr $ra