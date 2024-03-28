;;;; Copyright (c) 2020, Aaron Marks
(library-directories ".")
(import (schasm)
	(chezscheme)
        (rnrs))

(define out (make-asm))

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

(define write-syscall (cond
		       ((equal? (machine-type) 'ta6le) 1)
		       ((equal? (machine-type) 'ta6fb) 4)))
(define stdout 1)
(define stderr 2)
(define stdin 3)

(define random-string (foreign-alloc 10))

(asm out
     ;; save regs
     (push %rbp)
     (push %rdi)
     (push %rsi)
     (push %rsp)

     ;; loop some times
     ;; (mov %r8 10)
     ;; (mov %r9 0)
     ;; (label 'loop)

     (lea %rax 'my-data)
     (mov %rdx 6)
     (call 'print)

     (add %r9 %rax)

     ;; (sub %r8 1)
     ;; (cmp %r8 0)
     ;; (jne 'loop)

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
           (mov %rax write-syscall) ; write
           (mov %rdi stdout) ; fd
           (syscall)
           (ret))

     (data-string 'my-data "hello\n")
     (data-string 'my-data "hello\n")
     )

;; (asm out
;;      ;; (lea %rax 'test)
;;      ;; (lea %rbx 'test)
;;      ;; (mov %rax 0)
;;      (jmp 'test)
;;      (ret)
;;      (data-string 'test "hello\n"))

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
