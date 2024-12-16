; for constructs inspired by racket
(library (for)
  (export
    for/fold for for*)
  (import (chezscheme))
  (module (for/fold)
    (define-syntax (for/fold x)
      (syntax-case x (break result)
      [(k ([accum-id init] ...)
          (result res1 res2 ...)
          ([id v] ...)
          body ...)
       (with-implicit (k break)
         (with-syntax ([(l ...) (generate-temporaries #'(id ...))]
                       [(ta ...) (generate-temporaries #'(accum-id ...))])
                      #'(call-with-current-continuation
                          (lambda (cont)
                            (let ([accum-id init] ...)
                              (let ([break (lambda () (cont (begin res1 res2 ...)))])
                                (letrec ([loop (lambda (l ...)
                                                 (cond
                                                   [(or (null? l) ...) (break)]
                                                   [else
                                                     (call-with-values
                                                       (lambda ()
                                                         (let ([id (car l)] ...)
                                                           body ...))
                                                       (lambda (ta ...)
                                                         (set! accum-id ta) ...))
                                                     (loop (cdr l) ...)]))])
                                  (loop v ...))))))))]
      [(k ([accum-id init] ...)
          ([id v] ...)
          body ...)
       #'(for/fold ([accum-id init] ...)
                   (result (values accum-id ...))
                   ([id v] ...)
                   body ...)])))

#|
(for/fold ([sum 0]
           [rev-roots '()])
          (result (printf "~a~%~a~%" sum rev-roots))
          ([i '(1 2 3 4)])
  (when (> i 2) (break))
  (values (+ sum i) (cons (sqrt i) rev-roots)))

(printf "---~%")
|#

  (module (for)
    (define-syntax (for x)
      (syntax-case x ()
        [(k ([id v]) body ...)
         (with-implicit (k break)
         #'(call-with-current-continuation
             (lambda (break)
               (let loop ([l v])
                 (cond
                   [(null? l) (void)]
                   [else
                     (let ([id (car l)])
                       body ...)
                     (loop (cdr l))])))))]
        [(k ([id1 v1] [id2 v2] [id3 v3] ...) body ...)
         (with-implicit (k break)
           (with-syntax ([(l3 ...) (generate-temporaries #'(id3 ...))])
             #'(call-with-current-continuation
                 (lambda (break)
                   (let loop ([l1 v1] [l2 v2] [l3 v3] ...)
                     (cond
                       [(or (null? l1) (null? l2) (null? l3) ...) (void)]
                       [else
                         (let ([id1 (car l1)]
                               [id2 (car l2)]
                               [id3 (car l3)] ...)
                           body ...)
                         (loop (cdr l1) (cdr l2) (cdr l3) ...)]))))))])))

  ; --- similar to racket's for* --
  (module ((for* for*/cc))
          (define-syntax (for*/cc x)
            (syntax-case x ()
              [(_ ([id v]) body ...)
               #'(let loop ([l v])
                   (cond
                     [(null? l) (void)]
                     [else
                       (let ([id (car l)])
                         body ...)
                       (loop (cdr l))]))]
              [(_ ([id1 v1] [id2 v2] [id3 v3] ...) body ...)
               #'(for*/cc ([id1 v1])
                          (for*/cc ([id2 v2] [id3 v3] ...)
                                   body ...))]))
          (define-syntax (for* x)
            (syntax-case x (break)
              [(k args ...)
               (with-implicit (k break)
                 #'(call-with-current-continuation
                     (lambda (break)
                       (for*/cc args ...))))])))
)
