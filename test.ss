(library-directories ".")
(import (schasm)
	(chezscheme))

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

(asm out
     ;; save rbp
     (push %rbp)

     ;; loop some times
     (mov %rax 10)
     (label 'loop)
     (sub %rax 1)
     (test 0)
     (jne 'loop)

     ;; stuff
     (jmp 'hello)

     (mov %rax 10)
     (je 'hello)
     (mov %rax 10)

     (label 'hello)
     (jmp 'return)
     (mov %rax 40)
     (mov %rax 40)
     (mov %rax 40)
     (label 'return)

     ;; restore rbp
     (mov %rbp %rsp)
     (pop %rbp)

     ;; return value
     (mov %rax 20)
     (add %rax 20)
     (sub %rax 2)

     (ret))

(let ([bin (asm-value out)])
  (disasm bin (current-error-port))
  (print-hex bin (current-output-port)))

(flush-output-port (current-output-port))
(exit)
