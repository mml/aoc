(import (chezscheme))

(define (blank-line? line)
  (let loop ([cs (string->list line)])
    (cond
      [(null? cs) #t]
      [(char-whitespace? (car cs)) (loop (cdr cs))]
      [else #f])))

(define (line->integer line)
  (let loop ([cs (string->list line)][n #f])
    (cond
      [(null? cs) n]
      [(char-numeric? (car cs))
       (let ([m (- (char->integer (car cs)) (char->integer #\0))])
         (loop (cdr cs) (+ m (* 10 (or n 0)))))]
      [(char-whitespace? (car cs))
       (or n (loop (cdr cs) #f))]
      [else #f])))

(define (safe< a b)
  (let ([d (- b a)])
    (and (<= 1 d)
         (<= d 3))))

(define (safe> a b)
  (safe< b a))

(define (safe-report-increasing? ns)
  (let ([n0 (car ns)] [ns (cdr ns)])
    (cond
      [(null? ns) #t]
      [(safe< n0 (car ns)) (safe-report-increasing? ns)]
      [else #f])))

(define (safe-report-decreasing? ns)
  (let ([n0 (car ns)] [ns (cdr ns)])
    (cond
      [(null? ns) #t]
      [(safe> n0 (car ns)) (safe-report-decreasing? ns)]
      [else #f])))

(define (safe-report? ns)
  (let ([n0 (car ns)] [n1 (cadr ns)] [ns (cdr ns)])
    (cond
      [(safe> n0 n1) (safe-report-decreasing? ns)]
      [(safe< n0 n1) (safe-report-increasing? ns)]
      [else #f])))

(define (get-report line-port)
  (let ([d (get-datum line-port)])
    (if (eof-object? d)
      '()
      (cons d (get-report line-port)))))

(define (call-with-input-string s f)
  (f (open-string-input-port s)))

(define (check-reports port)
  (let loop ([line (get-line port)] [sum 0])
    (cond
      [(eof-object? line) sum]
      [else
        (let ([r (call-with-input-string line get-report)])
          (loop (get-line port) (+ sum (if (safe-report? r) 1 0))))])))

(printf "~a~n"
        (call-with-input-file (car (command-line-arguments)) check-reports))
