(library-directories ".")
(import (schasm)
	(chezscheme))

(define out (make-asm))

(define (print-hex instrs)
  (let ([n 0])
    (for-each (lambda (x)
		(set! n (fx1+ n))
		(when (<= x 15)
		  (display "0" (current-error-port)))
		(display (format "~x" x) (current-error-port))
		(when (= 0 (fxmod n 8))
		  (display ":" (current-error-port)))
		(when (= 0 (fxmod n 16))
		  (newline)))
	      (bytevector->u8-list instrs))))

;; diassemble using
(define (disasm instrs fn)
  (display "Disassembling\n" (current-error-port))
  (print-hex instrs)
  (newline)
  (let ([transcoder (make-transcoder (utf-8-codec) (eol-style lf)
					       (error-handling-mode replace))])
    (let-values ([(stdin stdout stderr pid) (open-process-ports "llvm-mc -disassemble" 'block transcoder)])
      (for-each (lambda (x) (display (format "~a " x) stdin))
		(bytevector->u8-list instrs))
      (close-output-port stdin)

      (let loop ()
	(let ((d (get-line stdout)))
    	  (unless (eof-object? d)
    	    (fn d)
    	    (loop)))))))

;; (test-schasm)

(asm out
     (label 'hello)
     (mov %rax 20)
     (mov %rbx 20)
     (jmp 'hello))

(disasm (asm-value out) (lambda (x) (display x) (newline)))

(exit)
