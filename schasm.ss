(library
    (schasm)
  (export ;; instructions
    mov
    push
    pop
    jmp
    nop
    ret
    label

    jmp-relative

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
    with-asm
    resolve-labels
    make-asm
    asm-port
    asm-value

    disasm

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

  ;; TODO what is this again???
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
  (define %rsi (make-register 6))
  (define %rdi (make-register 7))

  (define %r8  (make-register 8))
  (define %r9  (make-register 9))
  (define %r10 (make-register 10))
  (define %r11 (make-register 11))
  (define %r12 (make-register 12))
  (define %r13 (make-register 13))
  (define %r14 (make-register 14))
  (define %r15 (make-register 15))

  (define (make-asm)
    (let ([port (let-values ([(op g) (open-bytevector-output-port)])
		  (cons op g))])
      (list (make-eq-hashtable) port (make-eq-hashtable))))

  (define (asm-labels asm)
    (car asm))

  (define (asm-port asm)
    (caadr asm))

  (define (asm-read-value! asm)
    ((cdr (cadr asm))))
  (define (asm-value asm)
    ;; TODO is there a better way to do this with a bytevector port??
    (let ([v (asm-read-value! asm)])
      (put-bytevector (asm-port asm) v)
      v))
  (define (asm-value! asm value)
    (asm-read-value! asm)
    (put-bytevector (asm-port asm) value)
    value)
  (define (asm-offset asm)
    (bytevector-length (asm-value asm)))

  (define (asm-deferred asm)
    (caddr asm))

  ;; defer jump by adding to deferred label hashtable
  (define (defer-jmp asm label whence)
    (let ([defers (eq-hashtable-ref (asm-deferred asm) label #f)])
      (if defers
        (eq-hashtable-set! (asm-deferred asm) label (cons whence defers))
        (eq-hashtable-set! (asm-deferred asm) label (list whence)))))

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

  (define (imm32->reg asm register imm)
    (emit asm
          (rex-prefix 1 0 0 0)
	  (+ #xc7 (register-opcode register))
          #xc0
	  (imm32 imm)))

  ;; FIXME
  (define (imm64->reg asm register imm)
    (emit asm
          (rex-prefix 1 0 0 0)
	  (+ #xc7 (register-opcode register))
          #xc0
	  (imm32 imm)))

  (define (reg64->reg64 asm dst src)
    (emit asm
          #x48
          (+ #x85 (register-opcode dst))
          (+ #xe0 (register-opcode src))))

  (define (label asm name)
    (eq-hashtable-set! (asm-labels asm) 
		       name
		       (asm-offset asm))
    (nop asm))

  (define (nop asm)
    (emit asm #x90))

  (define (ret asm)
    (emit asm #xc3))

  (define (mov asm a b)
    (cond
     ((and (register? a) (number? b))
      (imm32->reg asm a b))
     ((and (register? a) (register? b))
      (reg64->reg64 asm a b))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (push asm src)
    (cond
     ((register? src)
      (emit asm (+ #x51 (register-opcode src))))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (pop asm dst)
    (cond
     ((register? dst)
      (emit asm (+ #x59 (register-opcode dst))))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (label-defined? asm label)
    (hashtable-contains? (asm-labels asm) label))

  (define (jmp-relative asm offset)
    ;; offset based on next instruction
    ;; -5 is magic value (1 + 4 bytes)
    (emit asm
          #xe9 ; 32-bit offset
          (imm32 (- offset 5))))

  (define (jmp asm label)
    (let* ((offset (asm-offset asm))
           (predefp (label-defined? asm label))
           (label-offset (if predefp
                           (asm-label-offset asm label)
                           0)))
      (jmp-relative asm (- label-offset offset))
      (unless predefp
        (defer-jmp asm label offset))))

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

  (define (resolve-labels asm instrs)
    (let loop ((cells (vector->list (hashtable-cells (asm-deferred asm)))))
      (when (and cells (not (null? cells)))
        (let* ((cell (car cells))
               (label (car cell))
               (offsets (cdr cell))
               (label-offset (asm-label-offset asm label)))
          (let label-loop ((o offsets))
            (unless (null? o)
              (let* ((offset (car o))
                     (patch (with-asm (jmp-relative (- label-offset offset))))
                     (mc (asm-read-value! asm)))
                ;; (disasm patch (current-error-port))
                ;; patch dummy jump
                (bytevector-copy! patch 0
                                  mc offset
                                  (bytevector-length patch))
                (asm-value! asm mc)
                (label-loop (cdr o)))))
          (loop (cdr cells)))))
    instrs)

  (define-syntax asm
    (syntax-rules ()
      ((_ out xs ...)
       (begin
         (resolve-labels out
                         (asm-syntax out xs ...))))))

  (define-syntax with-asm
    (syntax-rules ()
      ((_ xs ...)
       (let ((out (make-asm)))
         (asm out xs ...)
         (asm-value out)))))

  ;; syntax sugar to allow writing:
  ;; > (asm out (instr) (instr))
  ;; instead of:
  ;; > (begin (instr out) (instr out))
  (define-syntax asm-syntax
    (syntax-rules ()
      ((_ out (operator operands ...) ...)
       (begin
         (operator out operands ...) ...))))

  ;; diassemble using llvm-mc
  (define (disasm instrs port)
    (parameterize ((current-output-port port))
      (let ([transcoder (make-transcoder (utf-8-codec) (eol-style lf)
                                         (error-handling-mode replace))])
        (let-values ([(stdin stdout stderr pid) (open-process-ports "llvm-mc -disassemble -show-encoding" 'block transcoder)])
          (for-each (lambda (x) (display (format "~a " x) stdin))
                    (bytevector->u8-list instrs))
          (close-output-port stdin)

          (let loop ()
            (let ((d (get-line stdout)))
              (unless (eof-object? d)
                (display d)
                (newline)
                (loop))))))))


  (define (test-schasm)
    (test "imm16" (assert-equal (imm16 20) '(20 0)))
    (test "imm16 byte-order"
	  (assert-equal (imm16 1) '(1 0)))
    (test "imm64 byte-order"
	  (assert-equal (imm64 1) '(1 0 0 0 0 0 0 0)))))
