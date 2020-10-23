

; (load "lister-avansert.scm")

;; Threading macro
(define-syntax ->>
  (syntax-rules ()
    ((_ var) var)
    ((_ var (f1 ...) f2 ...)
     (->> (f1 ... var) f2 ...))
    ((_ var f1 f2 ...)
     (->> (f1 var) f2 ...))))

(define (fmap f . items)
  (if (null? items)
      (lambda items
        (apply map f items))
      (apply map f items)))

(define (foldr f def . lst)
  (define (inner f def items)
    (if (null? items)
        def
        (f (car items) (foldr f def (cdr items)))))
  (if (null? lst)
      (lambda (l)
        (inner f def l))
      (apply inner f def lst)))

(define (foldl f acc . lst)
  (define (inner f acc lst)
    (if (null? lst)
        acc
        (inner f (f acc (car lst)) (cdr lst))))
  (if (null? lst)
      (lambda (l)
        (inner f acc l))
      (apply inner f acc lst)))

;; This procedure lets you accumulate a list using some f.
(define (scanl f init lst)
  (if (null? lst)
      (list init)
      (cons init
            (scanl f (f init (car lst)) (cdr lst)))))

;; Accumulates the list from the right.
(define (scanr f def lst)
  (if (null? lst)
      (list def)
      (let ((rest (scanr f def (cdr lst)))) ; Recursively make the rest of the list
        (cons (f (car lst) (car rest))
              rest))))

(define ((flip f) x y)
  (f y x))

(define (sum lst)
  (apply + lst))

(define (range . args)
  (define (iter from to step)
    (if (> from to)
        '()
        (cons from (iter (+ from step) to step))))
  (case (length args)
    ((1) (iter 0 (car args) 1))
    ((2) (iter (car args) (cadr args) 1))
    ((3) (iter (car args) (cadr args) (caddr args)))))

(define (filter pred . lst)
  (if (null? lst)
      (lambda (l)
        (filter pred l))
      (apply foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst)))

(define (complement f)
  (compose not f))

(define (filter-not pred lst)
  (filter (complement pred) lst))

(define (take n lst)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (null? lst) (zero? n))
      lst
      (drop (dec n) (cdr lst))))

(define (init lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst)
            (init (cdr lst)))))

(define (repeat lst n)
  (if (zero? n)
      '()
      (append lst (repeat lst (- n 1)))))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (last lst)
  (cond ((null? lst) #f) ;; Arguably it should crash here
        ((null? (cdr lst)) (car lst))
        (else (last (cdr lst)))))

(define (id x) x)

(define (compose f g)
  (lambda args (f (apply g args))))

(define (comp f . funcs)
  (foldl compose f funcs))

;; curry, gjør det lettere å sende inn prosedyrer til høyereordens prosedyrer
(define partial
  (lambda (f . givens)
    (lambda args
      (apply f (append givens args)))))

(define (const x)
  (lambda args
    x))

(define (compareBy comp f start . args)
  (cdr (let ((init (cons (f start) start)))
         (foldl (lambda (old y)
                  (let ((new (cons (f y) y)))
                    (if (comp (car old) (car new))
                        new
                        old)))
                init args))))

(define (maximumBy f items)
  (apply (partial compareBy <) f (car items) (cdr items)))

(define (minimumBy f items)
  (apply (partial compareBy >) f (car items) (cdr items)))

(define (maximum items)
  (apply max items))

(define (minimum items)
  (apply min items))

(define (sortBy comp items)
  (if (null? items)
      '()
      (let* ((pivot (car items))
             (left (filter (partial comp pivot) (cdr items)))
             (right (filter (complement (partial comp pivot)) (cdr items))))
        (append (sortBy comp left)
                (list pivot)
                (sortBy comp right)))))

(define sort (partial sortBy >))

(define (find f items)
  (if (null? items)
      #f
      (or (and (f (car items)) items)
          (find f (cdr items)))))

; (define (find-first f items)
;   (call-with-current-continuation
;     (lambda (return)
;       (foldl (lambda (x y)
;                (if (f y)
;                    (return y)
;                    (begin
;                      (display y)
;                      (display "\n")
;                      x)))
;                #f
;                items))))

(define (take-while pred items)
  (if (or (null? items) (not (pred (car items))))
      '()
      (cons (car items)
            (take-while pred (cdr items)))))

(define (drop-while pred items)
  (if (or (null? items)
          (not (pred (car items))))
      items
      (drop-while pred (cdr items))))

(define (all-until pred proc seed)
  (if (not (pred seed))
      '()
      (cons seed
            (all-until pred proc (proc seed)))))

(define (split-on div items)
  (let ((start (take-while (lambda (x) (not (eq? x div))) items))
        (end (drop-while (lambda (x) (not (eq? x div))) items)))
    (cons start
          (if (or (null? end)
                  (null? (cdr end)))
              '()
              (split-on div (cdr end))))))
