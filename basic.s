        .text
        .globl tst_sym
        .align 4

        pushq %rbp

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
