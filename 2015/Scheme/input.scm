
(load "utils.scm")

;; File reading library

(define (read-file filename)
  (let loop ((file (open-input-file filename))
             (new '()))
    (if (eof-object? (peek-char file))
        (reverse new)
        (loop file
              (cons (read-char file) new)))))

(define (read-line port)
  (let ((top (peek-char port)))
    (if (or (eq? #\newline top)
            (eof-object? top))
        (begin
          (read-char port)  ;; Ensure we move past the newline
          '())
        (cons (read-char port)
              (read-line port)))))

;; Parsing

(define (list->number items)
  "Turn a list of digits and spaces
  as chars into a list of numbers"
  (->> items
       (split-on #\space)
       (map (comp string->number list->string))))

;; Output

(define (print x)
  (display x)
  (newline))

;; Testing

(list->number (string->list "1 2 3 12 45 987 654 32 123245"))
