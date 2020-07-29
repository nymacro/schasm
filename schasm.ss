(library
    (schasm)
  (export
    ;; instructions
    mov
    push
    pop
    jmp
    je
    jne
    test
    nop
    ret
    label
    add
    sub

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
    resolve-all
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
  (define %rdx (make-register 1))
  (define %rcx (make-register 2))
  (define %rbx (make-register 3))
  (define %rbp (make-register 4))
  (define %rsp (make-register 5))
  (define %rsi (make-register 6))
  (define %rdi (make-register 7))

  (define %r8  (make-register 0))
  (define %r9  (make-register 1))
  (define %r10 (make-register 2))
  (define %r11 (make-register 3))
  (define %r12 (make-register 4))
  (define %r13 (make-register 5))
  (define %r14 (make-register 6))
  (define %r15 (make-register 7))

  (define (make-asm)
    (let ([port (let-values ([(op g) (open-bytevector-output-port)])
		  (cons op g))])
      (list (make-eq-hashtable)
            port
            (box (list)))))

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

  (define asm-offset-
    (make-parameter
     (lambda (asm) (bytevector-length (asm-value asm)))))

  (define (asm-offset asm)
    ((asm-offset-) asm))

  (define (asm-deferred-instr asm)
    (caddr asm))

  (define (defer-instr asm whence defer-fn)
    (let* ((l (asm-deferred-instr asm))
           (v (unbox l)))
      ;; forgive me father, for I have sinned
      (set-box! l (cons (cons defer-fn whence) v))))

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
	  #xc7
          (+ #xc0 (register-opcode register))
	  (imm32 imm)))

  (define (imm64->reg asm register imm)
    (emit asm
          (rex-prefix 1 0 0 0)
	  (+ #xb8 (register-opcode register))
	  (imm64 imm)))

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
      (imm64->reg asm a b))
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
    (jmp-helper asm label jmp-relative))

  (define (jmp-helper asm label fn)
    (let* ((offset (asm-offset asm))
           (predefp (label-defined? asm label))
           (label-offset (if predefp
                           (asm-label-offset asm label)
                           0)))
      (fn asm (- label-offset offset))
      (unless predefp
        (defer-instr asm offset
          (lambda (asm)
            (let ((label-offset (asm-label-offset asm label))
                  (offset (asm-offset asm)))
              (fn asm (- label-offset offset))))))))

  (define (je-relative asm offset)
    (emit asm
          #x0f
          #x84
          (imm32 (- offset 6))))

  (define (je asm label)
    (jmp-helper asm label je-relative))

  (define (jne-relative asm offset)
    (emit asm
          #x0f
          #x85
          (imm32 (- offset 6))))

  (define (jne asm label)
    (jmp-helper asm label jne-relative))

  (define (test asm value)
    (emit asm
          (rex-prefix 1 0 0 0)
          #xa9
          (imm32 value)))

  (define (add asm dst src)
    (cond
     ((and (register? dst) (number? src))
      (emit asm
            (rex-prefix 1 0 0 0)
            #x81
            (+ #xc0 (register-opcode dst))
            (imm32 src)))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (sub asm dst src)
    (cond
     ((and (register? dst) (number? src))
      (emit asm
            (rex-prefix 1 0 0 0)
            #x81
            (+ #xe8 (register-opcode dst))
            (imm32 src)))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define-syntax assert-equal
    (syntax-rules ()
      ((_ x y)
       (unless (equal? x y)
	 (display (format "failed assertion: ~a != ~a" x y))
	 (newline)))))

  (define (patch-asm-stream asm patch offset)
    (let ((mc (asm-read-value! asm)))
      (bytevector-copy! patch 0
                        mc offset
                        (bytevector-length patch))
      (asm-value! asm mc)))

  ;; Replaces placeholder instructions which were unable to be correctly
  ;; encoded at read time. This happens mostly for jump instructions
  ;; which target labels which are not yet defined.
  (define (resolve-deferred-instr asm intr)
    (let loop ((deferred (unbox (asm-deferred-instr asm))))
      (unless (null? deferred)
        (let* ((pair (car deferred))
               (fn (car pair))
               (off (cdr pair))
               (patch (with-asm-labels-offset asm off (lambda (asm) (fn asm)))))
          (patch-asm-stream asm patch off))
        (loop (cdr deferred)))))

  (define (resolve-all asm instrs)
    (resolve-deferred-instr asm instrs))

  (define-syntax asm
    (syntax-rules ()
      ((_ out xs ...)
       (begin
         (resolve-all out
                      (asm-syntax out xs ...))))))

  (define-syntax with-asm
    (syntax-rules ()
      ((_ xs ...)
       (let ((out (make-asm)))
         (asm out xs ...)
         (asm-value out)))))

  (define (with-asm-labels asm fn)
    (let ((out (make-asm)))
      (set-car! out (hashtable-copy (asm-labels asm)))
         (fn out)
         (asm-value out)))

  (define (with-asm-labels-offset asm offset fn)
    (parameterize ((asm-offset- (lambda (asm) offset)))
      (with-asm-labels asm fn)))

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

  (define-syntax deftest
    (syntax-rules (assert-equal)
      ((_ name instrs ...)
       (begin
	 instrs ...))))

  (define (test-schasm)
    (deftest "imm16" (assert-equal (imm16 20) '(20 0)))
    (deftest "imm16 byte-order"
	  (assert-equal (imm16 1) '(1 0)))
    (deftest "imm64 byte-order"
	  (assert-equal (imm64 1) '(1 0 0 0 0 0 0 0)))))
