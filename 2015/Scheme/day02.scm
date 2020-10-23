(load "input.scm")
(load "utils.scm")

(define file (read-file "../data/02.in"))

(define (digits->num digs)
  (foldl (lambda (x y) (+ (* 10 x) y)) 0 digs))

(define (char->number n)
  (- (char->integer n) 48))

(define digs
  (fmap (compose (fmap (compose digits->num
                                (fmap char->number)))
                 (partial split-on #\x))
        (split-on #\newline file)))

(define (paper items)
  (let* ((l (car items))
         (w (cadr items))
         (h (caddr items))
         (xs (list (* 2 l w) (* 2 w h) (* 2 h l))))
    (+ (sum xs)
       (round (/ (minimum xs) 2)))))

(define (ribbon g)
  (let ((dimension (* 2 (- (apply + g) (apply max g))))
        (bow (apply * g)))
    (+ dimension bow)))


(define a (sum (map paper digs)))
(define b (sum (map ribbon digs)))

(for-each print (list a b))

;; 1598415
;; 3812909

