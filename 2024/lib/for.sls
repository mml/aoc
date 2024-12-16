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

  (module ((for for/cc))
    (define-syntax (for/cc stx)
      (syntax-case stx ()
        [(_ (bind* ...) body ...)
         (let loop ([bind* #'(bind* ...)]
                    [ids '()] ; to be used for `do` version
                    [lv-bind* '()]
                    [loop-bind* '()]
                    [car-bind* '()]
                    [lists '()])
           (cond
             [(null? bind*)
              (with-syntax ([(l ...) lists])
                #`(let-values #,lv-bind*
                    (let loop #,loop-bind* ; TODO: might be better written as a do loop
                      (cond
                        [(ormap null? (list l ...))
                         (void)]
                        [else
                          (let #,car-bind*
                            (begin body ...)
                            (loop (cdr l) ...))]))))]
             [else
               (let ([b (car bind*)])
                 (syntax-case b ()
                   [[(id1 id2 ...) vs]
                    (with-syntax ([(l1) (generate-temporaries #'(id1))]
                                  [(l2 ...) (generate-temporaries #'(id2 ...))])
                      (loop (cdr bind*)
                            (append #'(id1 id2 ...) ids)
                            (cons #'((l1 l2 ...) vs) lv-bind*)
                            (append #'([l1 l1] [l2 l2] ...) loop-bind*)
                            (append #'([id1 (car l1)] [id2 (car l2)] ...) car-bind*)
                            (append #'(l1 l2 ...) lists)))]
                   [[id v]
                    (with-syntax ([(l) (generate-temporaries #'(id))])
                      (loop (cdr bind*)
                            (cons #'id ids)
                            lv-bind*
                            (cons #'(l v) loop-bind*)
                            (cons #'(id (car l)) car-bind*)
                            (cons #'l lists)))]))]))]))
    ; TODO: add call/cc and break
    (define-syntax (for stx)
      (syntax-case stx ()
        [(_ args ...) #'(for/cc args ...)])))

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
