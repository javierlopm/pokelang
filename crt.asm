0print_str:
    lw $a0,0($fp)
    li $v0,4
    syscall
    jr $ra
0read_int:       
    li $v0,5
    syscall
    lw $a0,0($fp)
    st $v0,0($a0)
    jr $ra
0read_float:
    li $v0,6
    syscall
    lw $a0,0($fp)
    st $f0,0($a0)
    jr $ra
0read_char:
    lw $a0,0($fp)
    li $a1,1
    li $v0,8
    syscall
    jr $ra