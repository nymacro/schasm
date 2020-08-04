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
     ;; save rbp
     (push %rbp)

     ;; register tests
     ;; (mov %rax %rax)
     ;; (mov %rcx %rax)
     ;; (mov %rdx %rax)
     ;; (mov %rbx %rax)
     ;; (mov %rbp %rax)
     ;; (mov %rsi %rax)
     ;; (mov %rdi %rax)

     ;; ;; loop some times
     ;; (mov %r8 10)
     ;; (label 'loop)

     ;; syscall to exit
     ;; (mov %rax 1) ; exit
     ;; (mov %rdi 1) ; retval
     ;; (int #x80)

     ;; syscall to write
     (mov %rax 4) ; write
     (mov %rdi 1) ; fd
     ;(mov %rsi 0) ; ptr
     (lea %rsi 'my-data) ; ptr
     (mov %rdx 6) ; len
     (int #x80)

     ;; (sub %r8 1)
     ;; (test 0)
     ;; (jne 'loop)

     ;; ;; ;; stuff
     ;; ;; (jmp 'hello)

     ;; ;; (mov %rax 10)
     ;; ;; (je 'hello)
     ;; ;; (mov %rax 10)

     ;; ;; (label 'hello)
     ;; ;; (jmp 'return)
     ;; ;; (mov %rax 40)
     ;; ;; (mov %rax 40)
     ;; ;; (mov %rax 40)
     ;; ;; (label 'return)

     ;; ;; ;; return value
     ;; ;; (mov %rax 20)
     ;; ;; (add %rax 20)
     ;; ;; (sub %rax 2)

     ;; (label 'done)
     ;; (mov %rax %r8)

     ;; restore rbp
     ;; (mov %rbp %rsp)
     (pop %rbp)

     (ret)

     (data-string 'my-data "hello\n")
     (nop))

(define (run)
  (let ([bin (asm-value out)])
    (disasm bin (current-error-port))
    (print-hex bin (current-error-port))(newline)
    (flush-output-port (current-error-port))
    bin))

(when (or (null? (command-line))
          (eq? (car (command-line)) ""))
  (run)
  (exit))
