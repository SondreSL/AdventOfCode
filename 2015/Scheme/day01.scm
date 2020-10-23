
(load "utils.scm")
(load "input.scm")

(define (print x)
  (display x)
  (newline))

(define file (read-file "../data/01.in"))

(define (par ch)
  (cond ((char=? ch #\() 1)
        ((char=? ch #\)) -1)
        (else 'bad-input)))

(define a (sum (map par (init file))))
(define b (length (take-while (partial <= 0) (scanl + 0 (map par (init file))))))

(for-each print (list a b)) ; print the results
