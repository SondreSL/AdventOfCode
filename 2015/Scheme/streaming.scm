
(define-syntax scons
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))

(define empty-stream '())
(define scar car)
(define snull? null?)

(define (scdr str)
  (force (cdr str)))
