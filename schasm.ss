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

   ;; testing
   test-schasm)
  (import (chezscheme)
          (stream))

  (define-record-type register
    (nongenerative)
    (fields opcode extended?))

  (define (rex-i-register? register)
    (if (register-extended? register)
        1
        0))

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

  (define-record-type pointer
    (nongenerative)
    (fields address))
  (define (ptr addr)
    (make-pointer addr))

  (define-record-type offset-register
    (nongenerative)
    (fields offset register))
  (define (offset offset register)
    (make-offset-register offset register))

  (define make-asm make-patch-stream)

  (define asm-labels patch-stream-labels)

  (define asm-port patch-stream-port)

  (define asm-read-value! patch-stream-read-value!)

  (define asm-value patch-stream-value)

  (define asm-value! patch-stream-value!)

  (define asm-offset$ patch-stream-offset$)

  (define asm-offset patch-stream-offset)

  (define asm-deferred-instr patch-stream-deferred)

  (define defer-instr patch-defer)

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

  (define (registers-encode offset src dst)
    (let ((xsrc (or src %rax))
          (xdst (or dst %rax)))
      (+ offset
         (fxarithmetic-shift-left (register-opcode xsrc) 3)
         (register-opcode xdst))))

  (define (imm64->reg asm register imm)
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? register))
	  (+ #xb8 (register-opcode register))
	  (imm64 imm)))

  (define (reg64->reg64 asm dst src)
    (emit asm
          (rex-prefix 1 (rex-i-register? src) 0 (rex-i-register? dst))
          #x89
          (registers-encode #xc0 src dst)))

  (define (mem64->reg64 asm dst src)
    (emit asm
          (rex-prefix 1 0 0 (rex-i-register? dst))
          #x8b
          (registers-encode #x04 #f dst)
          #x25
          (imm32 (pointer-address src))))

  (define (memreg64->reg64 asm dst src)
    (emit asm
          (rex-prefix 1 (rex-i-register? src) 0 (rex-i-register? dst))
          #x8b
          (registers-encode #x00 src dst)))

  (define (offmemreg64->reg64 asm dst src offset)
    (emit asm
          (rex-prefix 1 (rex-i-register? src) 0 (rex-i-register? dst))
          #x8b
          (registers-encode #x80 src dst)
          (imm32 offset)))

  (define (label asm name)
    (eq-hashtable-set! (asm-labels asm) 
		       name
		       (asm-offset asm))
    (nop asm))

  (define (nop asm)
    (emit asm #x90))

  (define (ret asm)
    (emit asm #xc3))

  ;; lookup: https://www.felixcloutier.com/x86/mov
  (define (mov asm dst src)
    (cond
     ((and (register? dst) (number? src))
      (imm64->reg asm dst src))
     ((and (register? dst) (register? src))
      (reg64->reg64 asm dst src))
     ((and (register? dst) (pointer? src))
      (cond
       ((register? (pointer-address src))
        (memreg64->reg64 asm dst (pointer-address src)))
       (else
        (mem64->reg64 asm dst src))))
     ((and (register? dst) (offset-register? src))
      (offmemreg64->reg64 asm dst (offset-register-register src) (offset-register-offset src)))
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
    ;; -6 is magical instr relative offset
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

  (define-syntax asm
    (syntax-rules ()
      ((_ out xs ...)
       (begin
         (asm-syntax out xs ...)
         (resolve-all out)))))

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
  (define (disasm bin port)
    (parameterize ((current-output-port port))
      (let ([transcoder (make-transcoder (utf-8-codec) (eol-style lf)
                                         (error-handling-mode replace))])
        (let-values ([(stdin stdout stderr pid) (open-process-ports "llvm-mc -disassemble -show-encoding -triple=x86_64-unknown-unknown" 'block transcoder)])
          (for-each (lambda (x) (display (format "~a " x) stdin))
                    (bytevector->u8-list bin))
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
       (begin
	 (let ((xx x)
	       (yy y))
	   (unless (equal? xx yy)
	     (display (format "failed assertion: ~a != ~a~%" xx yy))))))))

  (define-syntax deftest
    (syntax-rules ()
      ((_ name instrs ...)
       (begin
         (display (format "~a~%" name))
	 instrs ...))))

  (define (display-hex-instr bin)
    (let ((list (bytevector->u8-list bin))
          (print-hex (lambda (byte) (display (format "0x~x," byte)))))
      (for-each print-hex list)
      (display "\n")))

  (define-syntax assert-instr
    (syntax-rules ()
      ((_ instr exp)
       (begin
	 (let ((bin (with-asm instr)))
           (display-hex-instr bin)
	   (let-values ([(out out-string) (open-string-output-port)])
	     (disasm bin out)
	     (assert-equal (out-string) exp)))))))

  (define (test-schasm)
    (deftest "imm16" (assert-equal (imm16 20) '(20 0)))
    (deftest "imm16 byte-order"
      (assert-equal (imm16 1) '(1 0)))
    (deftest "imm32 byte-order"
      (assert-equal (imm32 1) '(1 0 0 0)))
    (deftest "imm64 byte-order"
      (assert-equal (imm64 1) '(1 0 0 0 0 0 0 0)))
    (deftest "mov reg->reg"
      (assert-instr (mov %rax %rbx) "movq %rbx, %rax"))
    (deftest "mov imm->reg"
      (assert-instr (mov %rax 10) "movabsq $10, %rax"))
    (deftest "mov mem->reg"
      (assert-instr (mov %rax (ptr #x11223344)) "movq 31, %rax"))
    (deftest "mov (reg)->reg"
      (assert-instr (mov %rax (ptr %rbx)) "movq (%rbx), %rax"))
    (deftest "mov offmem->reg"
      (assert-instr (mov %rax (offset #x11223344 %rax)) "movq 31(%rax), %rax"))))
