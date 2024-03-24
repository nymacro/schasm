(library
    (stream)
  (export
   make-patch-stream
   patch-stream-labels
   patch-stream-port
   patch-stream-read-value!
   patch-stream-value
   patch-stream-value!
   patch-stream-offset$
   patch-stream-offset
   patch-stream-deferred
   patch-defer
   emit
   resolve-all)
  (import (chezscheme))
  
  ;; (<labels-hashtable> <output-port> <deferred-list>)
  (define (make-patch-stream)
    (let ([port (let-values ([(op g) (open-bytevector-output-port)])
		  (cons op g))])
      (list (make-eq-hashtable)
            port
            (box (list)))))

  (define (patch-stream-labels stream)
    (car stream))

  (define (patch-stream-port stream)
    (caadr stream))

  (define (patch-stream-read-value! stream)
    ((cdr (cadr stream))))
  (define (patch-stream-value stream)
    ;; TODO is there a better way to do this with a bytevector port??
    (let ([v (patch-stream-read-value! stream)])
      (put-bytevector (patch-stream-port stream) v)
      v))
  (define (patch-stream-value! stream value)
    (patch-stream-read-value! stream)
    (put-bytevector (patch-stream-port stream) value)
    value)

  (define patch-stream-offset$
    (make-parameter
     (lambda (stream) (bytevector-length (patch-stream-value stream)))))

  (define (patch-stream-offset stream)
    ((patch-stream-offset$) stream))

  (define (patch-stream-deferred stream)
    (caddr stream))

  (define (patch-defer stream whence defer-fn)
    (let* ((l (patch-stream-deferred stream))
           (v (unbox l)))
      ;; forgive me father, for I have sinned
      (set-box! l (cons (cons defer-fn whence) v))))

  ;; helper function to patch/overwrite a section of bytevector
  (define (patch-stream stream patch offset)
    (let ((mc (patch-stream-read-value! stream)))
      (bytevector-copy! patch 0
                        mc offset
                        (bytevector-length patch))
      (patch-stream-value! stream mc)))

  ;; Replaces placeholder instructions which were unable to be correctly
  ;; encoded at read time. This happens mostly for jump instructions
  ;; which target labels which are not yet defined.
  (define (resolve-deferred asm)
    (let loop ((deferred (unbox (patch-stream-deferred asm))))
      (unless (null? deferred)
        (let* ((pair (car deferred))
               (fn (car pair))
               (off (cdr pair))
               (patch (with-patch-stream-labels-offset asm off (lambda (asm) (fn asm)))))
          (patch-stream asm patch off))
        (loop (cdr deferred)))))

  ;; emit instructions in a new environment, copying labels from an existing environment
  (define (with-patch-stream-labels stream fn)
    (let ((out (make-patch-stream)))
      (set-car! out (hashtable-copy (patch-stream-labels stream)))
      (fn out)
      (patch-stream-value out)))

  ;; emit instructions in an environment with seperate offset
  (define (with-patch-stream-labels-offset stream offset fn)
    (parameterize ((patch-stream-offset$ (lambda (stream) offset)))
      (with-patch-stream-labels stream fn)))

  (define (resolve-all asm)
    (resolve-deferred asm))

  (define-syntax emit
    (syntax-rules ()
      ((emit stream instr)
       (if (list? instr)
           (for-each (lambda (x) (put-u8 (patch-stream-port stream) x))
                     instr)
           (put-u8 (patch-stream-port stream) instr)))
      ((emit stream instr xinstr ...)
       (begin
	 (emit stream instr)
	 (emit stream xinstr ...))))))