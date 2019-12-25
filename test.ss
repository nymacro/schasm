(library-directories ".")
(import (schasm)
	(chezscheme))

(define out$
  (let-values ([(op g) (open-bytevector-output-port)])
    (cons op g)))
(define out (car out$))
(define out-value (cdr out$))

(define (print-hex instrs)
  (for-each (lambda (x) (display (format "~2x" x) (current-error-port)))
	    (bytevector->u8-list instrs)))

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

(mov out %rax 20)

(disasm (out-value) (lambda (x) (display x) (newline)))

(test-schasm)

(exit)
