        .text
        .globl tst_sym
        .align 4
        movq $200000000000, %rax
        push %rbp
        mov %rsp, %rbp
        pop %rbp
        addq $10000001, %rbx
        subq $10000001, %rbx
        movq $0, %r8
        pushq $110000001

        movq $4, %rax
        push $1
        push $0
        push $0
        int $0x80
