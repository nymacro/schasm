;;;; Copyright (c) 2020, Aaron Marks
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
    cmp
    nop
    ret
    label
    add
    sub
    lea

    int
    call
    syscall

    data
    data-string
    subr

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

    instr
    asm-instr-syntax

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

  (define-record-type register
    (nongenerative)
    (fields opcode extended?))

  (define (rex-i-register? register)
    (if (register-extended? register)
        1
        0))

  ;; TODO what is this again???
  (define (rex-prefix w r x b)
    (fxior #b01000000
           (fxarithmetic-shift-left w 3)
           (fxarithmetic-shift-left r 2)
           (fxarithmetic-shift-left x 1)
           b))

  (define %rax (make-register 0 #f))
  (define %rcx (make-register 1 #f))
  (define %rdx (make-register 2 #f))
  (define %rbx (make-register 3 #f))
  (define %rsp (make-register 4 #f))
  (define %rbp (make-register 5 #f))
  (define %rsi (make-register 6 #f))
  (define %rdi (make-register 7 #f))

  (define %r8  (make-register 0 #t))
  (define %r9  (make-register 1 #t))
  (define %r10 (make-register 2 #t))
  (define %r11 (make-register 3 #t))
  (define %r12 (make-register 4 #t))
  (define %r13 (make-register 5 #t))
  (define %r14 (make-register 6 #t))
  (define %r15 (make-register 7 #t))

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

  (define asm-offset$
    (make-parameter
     (lambda (asm) (bytevector-length (asm-value asm)))))

  (define (asm-offset asm)
    ((asm-offset$) asm))

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

  (define (imm8 x)
    (car (encode-integer x 1)))

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
          (rex-prefix 1 0 0 (rex-i-register? register))
	  #xc7
          (+ #xc0 (register-opcode register))
	  (imm32 imm)))

  (define (imm64->reg asm register imm)
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? register))
	  (+ #xb8 (register-opcode register))
	  (imm64 imm)))

  (define (reg64->reg64 asm dst src)
    (emit asm
          (rex-prefix 1 (rex-i-register? src) 0 (rex-i-register? dst))
          #x89
          (+ #xc0
             (fxarithmetic-shift-left (register-opcode src) 3)
             (register-opcode dst))))

  (define (mem->reg64 asm dst src)
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? dst))
          #x8b
          (+ #xc0 (register-opcode dst))
          (imm64 src)))

  (define (label asm name)
    (eq-hashtable-set! (asm-labels asm) 
		       name
		       (asm-offset asm))
    (nop asm))

  (define (nop asm)
    (emit asm #x90))

  (define (ret asm)
    (emit asm #xc3))

  (define (mov asm dst src)
    (cond
     ((and (register? dst) (number? src))
      (imm64->reg asm dst src))
     ((and (register? dst) (register? src))
      (reg64->reg64 asm dst src))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (push asm src)
    (cond
     ((register? src)
      (emit asm (+ #x50 (register-opcode src))))
     ((number? src)
      (let* ([m (imm32 src)]
             [l (imm32 (fxarithmetic-shift-right src 32))])
        (emit asm #x68 m)
        (emit asm #x68 l)))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (pop asm dst)
    (cond
     ((register? dst)
      (emit asm (+ #x58 (register-opcode dst))))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (jmp-relative asm offset)
    ;; offset based on next instruction
    ;; -5 is magic value (1 + 4 bytes)
    (emit asm
          #xe9 ; 32-bit offset
          (imm32 (- offset 5))))

  ;; jump to label
  (define (jmp asm label)
    (jmp-helper asm label jmp-relative))

  (define (jmp-helper asm label fn)
    (let* ((offset (asm-offset asm)))
      (fn asm 0)
      (defer-instr asm offset
        (lambda (asm)
          (let ((label-offset (asm-label-offset asm label))
                (offset (asm-offset asm)))
            (fn asm (- label-offset offset)))))))

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

  (define (test asm dst value)
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? dst))
          #xf7
          (+ #xc0 (fxarithmetic-shift-left (register-opcode dst) 3))
          (imm32 value)))

  (define (cmp asm dst value)
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? dst))
          #x81
          (+ #xf8 (register-opcode dst))
          (imm32 value)))

  (define (add asm dst src)
    (cond
     ((and (register? dst) (number? src))
      (emit asm
            (rex-prefix 1 0 0 (rex-i-register? dst))
            #x81
            (+ #xc0 (register-opcode dst))
            (imm32 src)))
     ((and (register? dst) (register? src))
      (emit asm
            (rex-prefix 1 0 0 (rex-i-register? dst))
            #x01
            (+ #xb9 (fxarithmetic-shift-left (register-opcode dst) 3)
               (register-opcode src))))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (sub asm dst src)
    (cond
     ((and (register? dst) (number? src))
      (emit asm
            (rex-prefix 1 0 0 (rex-i-register? dst))
            #x81
            (+ #xe8 (register-opcode dst))
            (imm32 src)))
     (else
      (raise "unrecognized/unhandled operand[s]"))))

  (define (int asm num)
    (emit asm
          #xcd
          (imm8 num)))

  (define (call-helper asm offset)
    ;; 5-byte instr
    (emit asm
          (rex-prefix 1 0 0 0)
          #xe8
          (imm32 (- offset 5))))

  (define (call asm label)
    (let ((offset (asm-offset asm)))
      (call-helper asm 0)
      (defer-instr asm offset
        (lambda (asm)
          (let ((label-offset (asm-label-offset asm label))
                (offset (asm-offset asm)))
            (call-helper asm (- label-offset offset)))))))

  (define (syscall asm)
    (cond
     ((equal? (machine-type) 'ta6le) (emit asm #x0f #x05))
     (else (int asm #x80))))

  ;; define a subroutine with name
  (define-syntax subr
    (syntax-rules ()
      ((_ asm name instrs ...)
       (asm-syntax asm (label name)
                   instrs ...))))

  ;; define simple data octets
  (define (data asm name . d)
    (let ((end-label-name (gensym)))
      (jmp asm end-label-name)
      (label asm name)
      (let loop ((c d))
        (unless (null? c)
          (emit asm (car c))
          (loop (cdr c))))
      (label asm end-label-name)))

  ;; define string data
  (define (data-string asm name string)
    (apply data `(,asm ,name ,@(bytevector->u8-list (string->utf8 string)))))

  ;; TODO addressing modes. Right now offset is relative to %rip
  (define (lea-helper asm register offset)
    ;; -6 is magical instr relative number
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? register))
          #x8d
          (+ #x05
             (fxarithmetic-shift-left (register-opcode register) 3))
          (imm32 (- offset 6))))

  (define (lea asm dst label)
    (let ((fn lea-helper)
          (offset (asm-offset asm)))
      (fn asm dst 0)
      (defer-instr asm offset
        (lambda (asm)
          (let ((label-offset (asm-label-offset asm label))
                (offset (asm-offset asm)))
            (fn asm dst (- label-offset offset)))))))

  ;; helper function to patch/overwrite a section of bytevector
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

  ;; emit instructions in a new environment, copying labels from an existing environment
  (define (with-asm-labels asm fn)
    (let ((out (make-asm)))
      (set-car! out (hashtable-copy (asm-labels asm)))
         (fn out)
         (asm-value out)))

  ;; emit instructions in an environment with seperate offset
  (define (with-asm-labels-offset asm offset fn)
    (parameterize ((asm-offset$ (lambda (asm) offset)))
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
        (let-values ([(stdin stdout stderr pid) (open-process-ports "llvm-mc -disassemble -show-encoding -triple=x86_64-portbld-freebsd12.0" 'block transcoder)])
          (for-each (lambda (x) (display (format "~a " x) stdin))
                    (bytevector->u8-list instrs))
          (close-output-port stdin)

          (let loop ()
            (let ((d (get-line stdout)))
              (unless (eof-object? d)
                (display d)
                (newline)
                (loop))))))))

  (define-syntax assert-equal
    (syntax-rules ()
      ((_ x y)
       (unless (equal? x y)
	 (display (format "failed assertion: ~a != ~a~%" x y))))))

  (define-syntax deftest
    (syntax-rules ()
      ((_ name instrs ...)
       (begin
         (display (format "~a~%" name))
	 instrs ...))))

  (define-syntax op-mode
    (syntax-rules (rel abs)
      ((_ (rel i)) (cons 'rel i))
      ((_ (abs i)) (cons 'abs i))
      ((_ (mem i)) (cons 'mem i))
      ((_ a) 'a)))
  (define-syntax instr
    (syntax-rules ()
      ((_ op) (list 'op))
      ((_ op a) (list 'op (op-mode a)))
      ((_ op a b) (list 'op (op-mode a) (op-mode b)))))

  (define-syntax asm-instr-syntax
    (syntax-rules ()
      ((_ (x)) (instr x))
      ((_ (x a)) (instr x a))
      ((_ (x a b)) (instr x a b))
      ((_ x xs ...)
       (list
         (asm-instr-syntax x)
         (asm-instr-syntax xs ...)))))

  (define (test-schasm)
    (deftest "imm16" (assert-equal (imm16 20) '(20 0)))
    (deftest "imm16 byte-order"
      (assert-equal (imm16 1) '(1 0)))
    (deftest "imm32 byte-order"
      (assert-equal (imm32 1) '(1 0 0 0)))
    (deftest "imm64 byte-order"
      (assert-equal (imm64 1) '(1 0 0 0 0 0 0 0)))

    (deftest "instr binary rel"
      (assert-equal (instr mov (rel 10) (rel 20)) '(mov (rel . 10) (rel . 20))))
    (deftest "instr binary abs"
      (assert-equal (instr mov (abs 10) (rel 20)) '(mov (abs . 10) (rel . 20))))
    (deftest "instr unary"
      (assert-equal (instr mov 10) '(mov 10)))
    (deftest "instr nullary"
      (assert-equal (instr mov) '(mov)))))
