;;;; Copyright (c) 2024, Aaron Marks
(library
    (elf)
  (export
   test-elf
   )
  (import (chezscheme)
          (types)
          (stream))

  (define make-elf make-patch-stream)

  (define (magic out)
    (emit out #x7f 69 76 70))

  ;; only 64-bit
  (define (class out)
    (emit out #x02))

  ;; little endian
  (define (data out)
    (emit out #x01))
  ;; version
  (define (elf-version out)
    (emit out #x01))
  (define (os-abi out)
    (emit out #x09))
  (define (abi-version out)
    (emit out #x00))
  (define (pad out)
    (emit out #x00 #x00 #x00 #x00 #x00 #x00 #x00))

  ;; relocatable
  (define (type out)
    (emit out (imm16 #x01)))
  (define (machine out)
    (emit out (imm16 #x3e)))
  (define (version out)
    (emit out (imm32 #x00)))
  ;; entry point
  (define (entry out)
    (emit out (imm64 0)))
  ;; 64-bits
  ;; (define (program-header-offset out)
  ;;   (emit out (imm64 0)))
  ;; 64-bits
  ;; (define (section-header-offset out)
  ;;   (emit out (imm64 0)))
  (define (flags out)
    (emit out (imm32 0)))
  (define (header-size out)
    (emit out (imm16 0)))
  (define (program-header-size out)
    (emit out (imm16 #x38)))
  (define (program-header-count out)
    (emit out (imm16 0)))
  (define (section-header-size out)
    (emit out (imm16 #x40)))
  (define (section-header-count out)
    (emit out (imm16 0)))
  (define (section-header-name-index out)
    (emit out (imm16 0)))

  ;; PROGRAM HEADER
  (define (program-header-type out type)
    (emit out (imm32 type)))
  (define (program-header-flags out r w x)
    (emit out (imm32 0)))
  (define (program-header-offset out offset)
    (emit out (imm64 offset)))
  (define (program-header-virtual-address out address)
    (emit out (imm64 address)))
  (define (program-header-physical-address out address)
    (emit out (imm64 address)))
  (define (program-header-file-size out)
    (emit out (imm64 0)))
  (define (program-header-mem-size out)
    (emit out (imm64 0)))
  (define (program-header-alignment out)
    (emit out (imm64 0)))

  ;; simple function to do print debugging of position
  (define (debug-position out where)
    (let ((offset (patch-stream-offset out)))
      (display (string-append where ": "))
      (display offset)
      (newline)))

  (define (d-label label)
    (lambda (out _)
      ;; (guard (ex
      ;;         (#t 0))
      (patch-stream-label-offset out label)))

  (define-syntax defer
    (syntax-rules ()
      ((defer-out out fn (args ...) defer-fn)
       ((lambda (out)
          (patch-defer out (patch-stream-offset out)
                       (lambda (out extra-data)
                         ;; (display (format "Running deferred ~a\n" fn))
                         (fn out (defer-fn out extra-data))))
          (fn out args ...)) out))))

  (define (make-elf-header out)
    (elf-syntax out
                (magic)
                (class)
                (data)
                (elf-version)
                (os-abi)
                (abi-version)
                (pad)
                (type)
                (machine)
                (version)
                (entry)

                (defer program-header-offset
                  (0)
                  (d-label 'first-program-header))

                (defer section-header-offset
                  (0)
                  (d-label 'first-section-header))

                (flags)
                (header-size)
                (program-header-size)
                (program-header-count)
                (section-header-size)
                (section-header-count)
                (section-header-name-index)))

  (define (make-program-header out size)
    (elf-syntax out
                (label 'first-program-header)
                (program-header-type 0)
                (program-header-flags 1 0 1)
                (program-header-offset 0)
                (program-header-virtual-address #x00)
                (program-header-physical-address #x00)
                (program-header-file-size)
                (program-header-mem-size)
                (program-header-alignment)))

  ;; SECTION HEADER
  (define (section-header-name out offset)
    (emit out (imm32 0)))
  (define (section-header-type out type)
    (emit out (imm32 type)))
  (define (section-header-flags out flags)
    (emit out (imm64 flags)))
  (define (section-header-virtual-address out address)
    (emit out (imm64 address)))
  (define (section-header-offset out offset)
    (emit out (imm64 offset)))
  (define (section-header-file-size out)
    (emit out (imm64 0)))
  (define (section-header-link out)
    (emit out (imm32 0)))
  (define (section-header-info out)
    (emit out (imm32 0)))
  (define (section-header-address-alignment out)
    (emit out (imm64 8)))
  (define (seciton-header-size out)
    (emit out (imm64 0)))

  (define (make-section-header out size)
    (elf-syntax out
                (label 'first-section-header)
                (section-header-name 0)
                (section-header-type 0)
                (section-header-flags 0)
                (section-header-virtual-address #x00)
                (section-header-offset #x00)
                (section-header-file-size)
                (section-header-link)
                (section-header-info)
                (section-header-address-alignment)
                (section-header-size)))

  ;; syntax sugar to allow writing:
  ;; > (asm out (instr) (instr))
  ;; instead of:
  ;; > (begin (instr out) (instr out))
  (define-syntax elf-syntax
    (syntax-rules ()
      ((_ out (operator operands ...) ...)
       (begin
         (operator out operands ...) ...))))

  (define-syntax with-elf
    (syntax-rules ()
      ((_ xs ...)
       (let ((out (make-elf)))
         (elf-syntax out xs ...)
         (resolve-all out #f)
         (patch-stream-value out)))))

  (define-syntax deftest
    (syntax-rules ()
      ((_ name instrs ...)
       (begin
         (display (format "~a~%" name))
	 instrs ...))))

  (define (dump-to-tmp out)
    (let* ((filename "/tmp/elf.tmp")
           (port (open-file-output-port filename (file-options no-fail))))
      (put-bytevector port out)
      (close-output-port port)
      filename))

  (define (dump-elf out)
    (let* ([filename (dump-to-tmp out)]
           [transcoder (make-transcoder (utf-8-codec) (eol-style lf)
                                        (error-handling-mode replace))])
      (let-values ([(stdin stdout stderr pid) (open-process-ports (string-append "readelf -a " filename) 'block transcoder)])
        (close-output-port stdin)

        (let loop ()
          (let ((d (get-line stdout)))
            (unless (eof-object? d)
              (display d)
              (newline)
              (loop)))))))

  (define-syntax with-elf-test
    (syntax-rules ()
      ((with-elf-test xs ...)
       (dump-elf (with-elf xs ...)))))

  ;; (define-syntax with-program-header
  ;;   (syntax-rules ()
  ;;     ((program-header-section name xs ...)
  ;;      (begin
  ;;        (label name)

  ;; TODO readelf output
  (define (test-elf)
    (deftest "empty elf" (with-elf-test (make-elf-header)))
    (deftest "section elf"
      (with-elf-test
       (make-elf-header)
       (make-program-header 0)
       (make-section-header 0)
       )))
  )
