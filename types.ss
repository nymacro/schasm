;;;; Copyright (c) 2024, Aaron Marks
(library
    (types)
  (export
   imm8
   imm16
   imm32
   imm64
   encode-integer)
  (import (chezscheme))

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
      (append m l))))
