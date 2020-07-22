        .text
        .globl tst_sym
        .align 4
        movq $20, %rax
        push %rbp
        mov %rsp, %rbp
        pop %rbp
