(library
    (elf)
  (export
   object)

  (define (make-elf)
    (let ([port (let-values ([(op g) (open-bytevector-output-port)])
		  (cons op g))])
      (list port)))

  (define (elf-port elf)
    (car elf))
  
  (define-syntax emit
    (syntax-rules ()
      ((emit asm instr)
       (if (list? instr)
           (for-each (lambda (x) (put-u8 (elf-port asm) x))
                     instr)
           (put-u8 (elf-port asm) instr)))
      ((emit asm instr xinstr ...)
       (begin
	 (emit asm instr)
	 (emit asm xinstr ...)))))

  (define (magic out)
    (emit out #x7f #cE #cL #cF))

  ;; only 64-bit
  (define (class out)
    (emit out #x02))

  ;; little endian
  (define (data out)
    (emit out #x01))
  ;; version
  (define (version out)
    (emit out #x01))
  (define (os-abi out)
    (emit out #x09))
  (define (abi-version out)
    (emit out #x01))
  (define (pad out)
    (emit out #x00 #x00 #x00 #x00 #x00 #x00 #x00))

  ;; relocatable
  (define (type out)
    (emit out #x00 #x00))
  (define (machine out)
    (emit out #x00 #x3e))
  (define (version out)
    (emit out #x00 #x00 #x00 #x00))
  ;; entry point
  (define (entry out)
    (emit out (int64 0)))
  ;; 64-bits
  (define (program-header-offset out)
    (emit out (int64 0)))
  ;; 64-bits
  (define (section-header-offset out)
    (emit out (int64 0)))
  (define (flags out)
    (emit out (int16 0)))
  (define (header-size out)
    (emit out (int16 0)))
  (define (program-header-size out)
    (emit out (int16 #x38)))
  (define (program-header-count out)
    (emit out (int16 0)))
  (define (section-header-size out)
    (emit out (int16 #x40)))
  (define (section-header-count out)
    (emit out (int16 0)))
  (define (section-header-name-index out)
    (emit out (int16 0)))

  ;; PROGRAM HEADER
  (define (program-header-type out type)
    (emit out (int32 type)))
  (define (program-header-flags out r w x)
    (emit out (int32 0)))
  (define (program-header-offset out offset)
    (emit out (int64 offset)))
  (define (program-header-virtual-address out address)
    (emit out (int64 address)))
  (define (program-header-physical-address out address)
    (emit out (int64 address)))
  (define (program-header-file-size out)
    (emit out (int64 0)))
  (define (program-header-mem-size out)
    (emit out (int64 0)))
  (define (program-header-alignment out)
    (emit out (int64 0)))

  ;; SECTION HEADER
  (define (section-header-name out offset)
    (emit out (int32 0)))
  (define (section-header-type out type)
    (emit out (int32 type)))
  (define (section-header-flags out flags)
    (emit out (int64 flags)))
  (define (section-header-virtual-address out address)
    (emit out (int64 address)))
  (define (section-header-offset out)
    (emit out (int64 0)))
  (define (section-header-file-size out)
    (emit out (int64 0)))
  (define (section-header-link out)
    (emit out (int32 0)))
  (define (section-header-info out)
    (emit out (int32 0)))
  (define (section-header-address-alignment out)
    (emit out (int64 8)))
  (define (seciton-header-size out)
    (emit out (int64 0)))

  )
