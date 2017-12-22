.text
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($sp)
	addi $sp, $sp, -4
	jal main_integer
	li $v0, 10
	syscall
main_integer:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -8
#_main_integer_0
	li $t0, 10
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	sw $v0, -4($fp)
#_main_integer_1
	lw $t0, -4($fp)
	sw $t0, -8($fp)
#_main_integer_2
	li $t0, 0
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, -1
	sw $t1, 0($t0)
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
atoi:
	move $t0, $a0
	li $t1, 0
	li $t2, 10
atoi_loop:
	lbu $t3, 0($t0)
	beq $t3, $zero, atoi_end
	li $t4, 48
	blt $t3, $t4, atoi_error
	li $t4, 57
	bgt $t3, $t4, atoi_error
	addi $t3, $t3, -48
	mul $t1, $t1, $t2
	add $t1, $t1, $t3
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
.data
