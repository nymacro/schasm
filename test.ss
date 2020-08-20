(library-directories ".")
(import (schasm)
	(chezscheme)
        (rnrs))

(define out (make-asm))

(define (print-hex-pretty instrs port)
  (parameterize ((current-output-port port))
    (let ([n 0])
      (for-each (lambda (x)
                  (set! n (fx1+ n))
                  (when (<= x 15)
                    (display "0"))
                  (display (format "~x" x))
                  (when (= 0 (fxmod n 8))
                    (display ":"))
                  (when (= 0 (fxmod n 16))
                    (newline)))
                (bytevector->u8-list instrs)))))

(define (print-hex instrs port)
  (parameterize ((current-output-port port))
    (let ([n 0])
      (for-each (lambda (x)
                  (set! n (fx1+ n))
                  (when (<= x 15)
                    (display "0"))
                  (display (format "~x" x)))
                (bytevector->u8-list instrs)))))

;; (test-schasm)

(define random-string (foreign-alloc 10))

(asm out
     ;; save regs
     (push %rbp)
     (push %rdi)
     (push %rsi)
     (push %rsp)

     ;; register tests
     ;; (mov %rax %rax)
     ;; (mov %rcx %rax)
     ;; (mov %rdx %rax)
     ;; (mov %rbx %rax)
     ;; (mov %rbp %rax)
     ;; (mov %rsi %rax)
     ;; (mov %rdi %rax)

     ;; (test %rax 0)
     ;; (test %rcx 0)
     ;; (test %rdx 0)
     ;; (test %r8 0)

     ;; loop some times
     (mov %r8 10)
     (mov %r9 0)
     (label 'loop)

     ;; syscall to write
     ;; (mov %rax 4) ; write
     ;; (mov %rdi 1) ; fd
     ;; (lea %rsi 'my-data) ; ptr
     ;; (mov %rdx 6) ; len
     ;; (int #x80)

     (lea %rax 'my-data)
     (mov %rdx 6)
     (call 'print)

     (add %r9 %rax)

     (sub %r8 1)
     (cmp %r8 0)
     (jne 'loop)

     (mov %rax %r9)

     ;; restore regs
     (pop %rsp)
     (pop %rsi)
     (pop %rdi)
     (pop %rbp)

     (ret)

     ;; rax <- ptr
     ;; rdx <- len
     (subr 'print
           (mov %rsi %rax)
           (mov %rax 4) ; write
           (mov %rdi 1) ; fd
           (int #x80)
           (ret))

     (data-string 'my-data "hello\n")
     (data-string 'other-data "subr\n")
     (nop))

(define (run)
  (let ([bin (asm-value out)])
    (disasm bin (current-error-port))
    (print-hex bin (current-error-port))(newline)
    (flush-output-port (current-error-port))
    bin))

(define (run-from-shell?)
  (or (null? (command-line))
      (eq? (car (command-line)) "")))

(when (run-from-shell?)
  (test-schasm)
  (run)
  (exit)
  )
