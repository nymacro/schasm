        .text
        .globl tst_sym
        .align 4

	jmp 0

        pushq %rbp

        mov 0(%rip), %rax
        leaq 4(%rbx), %rax

	movq 0x11223344(%rax), %rax
	movq (%rax), %rbx
	movq 0x11223344, %rax

        movq %r8, %rsi
        movq %rax, %rax
        testq $0, %rax
        testq $0, %r8
        cmpq $0, %r8
        movq $0, %rax

        addq %rax, %r9

        popq %rbp

        call tst_sym

        retq
