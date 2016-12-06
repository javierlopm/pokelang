vamo_a_imprimi:
    lw $a0,0($sp)
    li $v0,4
    syscall
    jr $ra
# idioteces mal escritas
vamo_a_imprimi_i:
    lw $a0,0($sp)
    li $v0,1
    syscall
    jr $ra
vamo_a_imprimi_f:
    lw $a0,0($sp)
    li $v0,2
    syscall
    jr $ra
vamo_a_imprimi_c:
    lw $a0,0($sp)
    li $v0,11
    syscall
    jr $ra
# idioteces end
vamo_a_lee_i:       
vamo_a_lee_b:    
    li $v0,5
    syscall
    lw $a0,0($sp)
    sw $v0,0($a0)
    jr $ra
vamo_a_lee_f:
    li $v0,6
    syscall
    lw $a0,0($sp)
    # sw $f0,0($a0)
    jr $ra
vamo_a_lee_c:
    li $v0,12
    syscall
    lw $v0,0($sp)
    sw $a0,0($v0)
    jr $ra