(library (graph)
  (export make-node node-name node-props node-props-set!
          node-prop-set!
          make-node-adjacency-list node-adjacency-list-neighbors
          node-adjacency-list-neighbors-set!
          node->struct
          make-neighbor neighbor-dst neighbor-weight
          node-add-neighbor!
          make-graph-adjacency-list graph-adjacency-list-nodes graph-adjacency-list-nodes-set!
          graph-add-node!
          make-prioq prioq-add! prioq-pop! prioq-empty? decrease!
          dijkstra)
  (import (chezscheme)
          (for))
(define-record-type node
  (fields name (mutable props))
  (nongenerative)
  (protocol
    (lambda (new)
      (lambda (name)
        (new name '())))))
(define (node-prop-set! n k v)
  (let ([props (node-props n)])
    (cond
      [(null? props) (node-props-set! n (list (cons k v)))]
      [else
        (let loop ([left (car props)] [props (cdr props)])
          (cond
            [(null? props)
             (set-cdr! left (list (cons k v)))]
            [(eq? k (caar props))
             (set-cdr! (car props) v)]
            [else
              (loop (car props) (cdr props))]))])))



(define-record-type node-adjacency-list
  (parent node)
  (fields (mutable neighbors))
  (protocol
    (lambda (pargs->new)
      (lambda (name)
        ((pargs->new name) '()))))
  (nongenerative))
(define-record-type neighbor
  (fields dst weight)
  (nongenerative))
(define (node-add-neighbor! n dst weight)
  (node-adjacency-list-neighbors-set!
    n
    (cons (make-neighbor dst weight) (node-adjacency-list-neighbors n))))
(define-record-type graph-adjacency-list
  (fields (mutable nodes))
  (protocol
    (lambda (new)
      (case-lambda
        [() (new '())]
        [(nodes) (new nodes)])))
  (nongenerative))
(define (graph-add-node! g node-or-name)
  (cond
    [(node? node-or-name)
     (graph-adjacency-list-nodes-set!
       g (cons node-or-name (graph-adjacency-list-nodes g)))]
    [else (graph-add-node! g (make-node-adjacency-list node-or-name))]))
(define (node->struct n)
  `(node ,(node-name n)
     (neighbors ,@(map (lambda (nbr)
                         (list (node-name (neighbor-dst nbr))
                               (neighbor-weight nbr)))
                       (node-adjacency-list-neighbors n)))))


(define (make-visited-set g)
  (make-eq-hashtable))
(define (visited? vs n)
  (eq-hashtable-ref vs n #f))
(define (set-visited! vs n)
  (eq-hashtable-set! vs n #t))

; alist-based prioq is slow, but good enough for this problem.
(define (make-prioq)
  (box '()))
(define (prioq-add! q x prio)
  (let ([l (unbox q)])
    (cond
      [(null? l) (set-box! q (list (cons prio x)))]
      [(< prio (caar l)) (set-box! q (cons (cons prio x) l))]
      [else
        (let loop ([after l] [rest (cdr l)])
          (cond
            [(null? rest)
             (set-cdr! after (list (cons prio x)))]
            [(< prio (caar rest))
             (set-cdr! after (cons (cons prio x) rest))]
            [else
              (loop rest (cdr rest))]))])))
(define (prioq-pop! q)
  (let* ([l (unbox q)]
         [x (car l)])
    (set-box! q (cdr l))
    (values (car x) (cdr x))))
(define (prioq-empty? q)
  (null? (unbox q)))
(define (decrease! q x prio)
  (let ([l0 (unbox q)])
    (cond
      [(null? l0) (assertion-violation 'decrease! "not found" x)]
      [(eq? x (cdar l0))
       (set-car! (car l0) prio)]
      [(< prio (caar l0))
       (let to-top ([left #f] [l l0])
         (cond
           [(null? l) (assertion-violation 'decrease! "not found" x)]
           [(eq? x (cdar l))
            (when left (set-cdr! left (cdr l)))
            (set-cdr! l l0)
            (set-box! q l)]
           [else (to-top l (cdr l))]))]
      [else
        (let find-it ([after #f] [before #f] [left #f] [l l0])
          (cond
            [(null? l) (assertion-violation 'decrease! "not found" x)]
            [(eq? x (cdar l))
             (when (< prio (caar l))
               (set-car! (car l) prio)
               (when after
                 ;(error 'foo "bar")
                 (set-cdr! left (cdr l))
                 (set-cdr! after l)
                 (set-cdr! l before)))]
            [(> prio (caar l))
             (find-it after before l (cdr l))]
            [else
              (find-it (or after left) (or before l) l (cdr l))]))])))
(define (prioq-find pred q)
  (find pred (unbox q)))
(define prioq->alist unbox)

(define (dijkstra g src)
  (let* ([visited (make-visited-set g)]
         [unvisited? (lambda (x)
                       (not (eq-hashtable-ref visited x #f)))]
         [distances (make-prioq)]
         [find-unvisited
           (lambda ()
             (let ([pr (prioq-find (lambda (pr)
                               (unvisited? (cdr pr)))
                             distances)])
               (if (and pr
                        (finite? (car pr)))
                 (values (cdr pr) (car pr))
                 (values #f #f))))])
    (for ([node (graph-adjacency-list-nodes g)])
      (cond
        [(eq? node src) (prioq-add! distances node 0)]
        [else (prioq-add! distances node +inf.0)]))
    (let loop ()
      (let-values ([(node1 dist) (find-unvisited)])
        (when node1
          (for ([nbr (node-adjacency-list-neighbors node1)])
            (let ([node2 (neighbor-dst nbr)])
              (when (unvisited? node2)
                (decrease! distances node2 (+ dist (neighbor-weight nbr))))))
          (set-visited! visited node1)
          (loop))))
    (prioq->alist distances)))
;
)
