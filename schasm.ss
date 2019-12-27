(library
    (schasm)
  (export ;; instructions
	  mov
	  jmp
	  nop
	  label

	  ;; registers
	  %rax
	  %rbx
	  %rcx
	  %rdx
	  %rbp
	  %rsp
	  %rsi
	  %rdi
	  %r8
	  %r9
	  %r10
	  %r11
	  %r12
	  %r13
	  %r14
	  %r15

	  ;; helpers
	  asm
	  make-asm
	  asm-port
	  asm-value

	  ;; testing
	  test-schasm)
  (import (chezscheme))

  (define-syntax emit
    (syntax-rules ()
      ((emit asm instr)
       (if (list? instr)
	   (for-each (lambda (x) (put-u8 (asm-port asm) x))
		     instr)
	   (put-u8 (asm-port asm) instr)))
      ((emit asm instr xinstr ...)
       (begin
	 (emit asm instr)
	 (emit asm xinstr ...)))))

  (define register (make-record-type "register" '(x)))
  (define make-register (record-constructor register))
  (define register? (record-predicate register))
  (define register-opcode (record-accessor register 0))

  (define (rex-prefix w r x b)
    (fxior #b01000000
	   (fxarithmetic-shift-left w 3)
	   (fxarithmetic-shift-left r 2)
	   (fxarithmetic-shift-left x 1)
	   b))

  (define %rax (make-register 0))
  (define %rbx (make-register 1))
  (define %rcx (make-register 2))
  (define %rdx (make-register 3))
  (define %rbp (make-register 4))
  (define %rsp (make-register 5))
  (define %rsi (make-register 5))
  (define %rdi (make-register 5))

  (define %r8  (make-register 5))
  (define %r9  (make-register 5))
  (define %r10 (make-register 5))
  (define %r11 (make-register 5))
  (define %r12 (make-register 5))
  (define %r13 (make-register 5))
  (define %r14 (make-register 5))
  (define %r15 (make-register 5))

  (define (make-asm)
    (let ([port (let-values ([(op g) (open-bytevector-output-port)])
		  (list op g))])
      (cons (make-eq-hashtable) port)))

  (define (asm-labels asm)
    (car asm))
  (define (asm-port asm)
    (cadr asm))
  (define (asm-value! asm)
    ((caddr asm)))
  (define (asm-value asm)
    ;; TODO is there a better way to do this with a bytevector port??
    (let ([v (asm-value! asm)])
      (put-bytevector (asm-port asm) v)
      v))
  (define (asm-offset asm)
    (bytevector-length (asm-value asm)))

  (define (pp s)
    (cond
     ((hash-table? s)
      (let-values ([(k v) (hashtable-entries s)])
	(display (format "~a ~a" k v))))
     (else
      (display s)))
    (newline))

  (define (asm-label-offset asm label)
    (let ([label (eq-hashtable-ref (asm-labels asm) label #f)])
      (pp label)
      (pp (asm-labels asm))
      (unless label
	(raise "unknown label"))
      label))

  (define (encode-integer x bytes)
    (define (out x count result)
      (if (zero? count)
	  (list result x)
	  (out (fxarithmetic-shift-right x 8)
	       (- count 1)
	       (cons (fxand x #xff) result))))
    (out x bytes '()))

  (define (imm16 x)
    (let* ([m (encode-integer x 1)]
	   [l (encode-integer (cadr m) 1)])
      (append (car m) (car l))))

  (define (imm32 x)
    (let* ([m (imm16 x)]
	   [l (imm16 (fxarithmetic-shift-right x 16))])
      (append m l)))

  (define (imm64 x)
    (let* ([m (imm32 x)]
	   [l (imm32 (fxarithmetic-shift-right x 32))])
      (append m l)))

  (define (imm64->reg asm register imm)
    (emit asm
	  (rex-prefix 1 0 0 0)
	  (+ #xb8 (register-opcode register))
	  (imm64 imm)))

  (define (label asm name)
    (eq-hashtable-set! (asm-labels asm) 
		       name
		       (asm-offset asm))
    (nop asm))

  (define (nop asm)
    (emit asm #x90))

  (define (mov asm a b)
    (cond
     ((and (register? a) (number? b))
      (imm64->reg asm a b))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (jmp asm label)
    (emit asm
	  #xe9 ; 16-bit offset
	  (imm32 (- (asm-label-offset asm label) (asm-offset asm)))))

  (define-syntax assert-equal
    (syntax-rules ()
      ((_ x y)
       (unless (equal? x y)
	 (display (format "failed assertion: ~a != ~a" x y))
	 (newline)))))

  (define-syntax test
    (syntax-rules (assert-equal)
      ((_ name instrs ...)
       (begin
	 instrs ...))))

  (define-syntax asm
    (syntax-rules ()
      ((_ out (operator operands ...) ...)
       (begin
	 (operator out operands ...) ...))))

  (define (test-schasm)
    (test "imm16" (assert-equal (imm16 20) '(20 0)))
    (test "imm16 byte-order"
	  (assert-equal (imm16 1) '(1 0)))
    (test "imm64 byte-order"
	  (assert-equal (imm64 1) '(1 0 0 0 0 0 0 0)))))
