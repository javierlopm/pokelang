vamo_a_imprimi:
    lw $a0,0($sp)
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
vamo_a_lee_i:       
    li $v0,5
    syscall
    lw $a0,0($fp)
    sw $v0,0($a0)
    jr $ra
vamo_a_lee_b:       
    li $v0,5
    syscall
    lw $a0,0($fp)
    sw $v0,0($a0)
    jr $ra
vamo_a_lee_f:
    li $v0,6
    syscall
    lw $a0,0($fp)
    # sw $f0,0($a0)
    jr $ra
vamo_a_lee_c:
    lw $a0,0($fp)
    li $a1,1
    li $v0,8
    syscall
    jr $ra