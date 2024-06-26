;;; CONFORM -- Type checker, written by Jim Miller.

;;; Functional and unstable

(set-custom-version-limits! 2 1 5 4 4 3 2 9 2 10 7 1 1 2 4 4 9 10 1 9 4 9 7 4 8
  10 5 1 3 7 6 5 3 4 6 2 2 7 2 6 6 10 5 1 8 9 2 7 2 9 5 10 6 10 4 2 1 4 5 2 4 2 7
  5 8 6 3 6 6 4 5 2 10 3 9 4 3 8 7 5 9 4 6 1 4 1 6 7 5 2)


(define (sort-list obj pred) (set-bbv-version-limit! #f) 

  (define (loop l) (set-bbv-version-limit! #f) 
    (if (and (pair? l) (pair? (Scdr l)))
        (split-list l '() '())
        l))

  (define (split-list l one two) (set-bbv-version-limit! #f) 
    (if (pair? l)
        (split-list (Scdr l) two (cons (Scar l) one))
        (merge (loop one) (loop two))))

  (define (merge one two) (set-bbv-version-limit! #f) 
    (cond ((null? one) two)
          ((pred (Scar two) (Scar one))
           (cons (Scar two)
                 (merge (Scdr two) one)))
          (else
           (cons (Scar one)
                 (merge (Scdr one) two)))))

  (loop obj))

;; SET OPERATIONS
; (representation as lists with distinct elements)

(define (adjoin element set) (set-bbv-version-limit! #f) 
  (if (Smemq element set) set (cons element set)))

(define (eliminate element set) (set-bbv-version-limit! #f) 
  (cond ((null? set) set)
        ((eq? element (Scar set)) (Scdr set))
        (else (cons (Scar set) (eliminate element (Scdr set))))))

(define (intersect list1 list2) (set-bbv-version-limit! #f) 
  (let loop ((l list1))
    (cond ((null? l) '())
          ((Smemq (Scar l) list2) (cons (Scar l) (loop (Scdr l))))
          (else (loop (Scdr l))))))

(define (union list1 list2) (set-bbv-version-limit! #f) 
  (if (null? list1)
      list2
      (union (Scdr list1)
             (adjoin (Scar list1) list2))))

;; GRAPH NODES

(define make-internal-node vector)
(define (internal-node-name node) (set-bbv-version-limit! #f)  (Svector-ref node 0))
(define (internal-node-green-edges node) (set-bbv-version-limit! #f)  (Svector-ref node 1))
(define (internal-node-red-edges node) (set-bbv-version-limit! #f)  (Svector-ref node 2))
(define (internal-node-blue-edges node) (set-bbv-version-limit! #f)  (Svector-ref node 3))
(define (set-internal-node-name! node name) (set-bbv-version-limit! #f)  (Svector-set! node 0 name))
(define (set-internal-node-green-edges! node edges) (set-bbv-version-limit! #f)  (Svector-set! node 1 edges))
(define (set-internal-node-red-edges! node edges) (set-bbv-version-limit! #f)  (Svector-set! node 2 edges))
(define (set-internal-node-blue-edges! node edges) (set-bbv-version-limit! #f)  (Svector-set! node 3 edges))

(define (make-node name . blue-edges) (set-bbv-version-limit! #f)    ; User's constructor
  (let ((name (if (symbol? name) (Ssymbol->string name) name))
        (blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (Scar blue-edges))))
    (make-internal-node name '() '() blue-edges)))

(define (copy-node node) (set-bbv-version-limit! #f) 
  (make-internal-node (name node) '() '() (blue-edges node)))

; Selectors

(define name internal-node-name)
(define (make-edge-getter selector) (set-bbv-version-limit! #f) 
  (lambda (node) (set-bbv-version-limit! #f) 
    (if (or (none-node? node) (any-node? node))
        (fatal-error "Can't get edges from the ANY or NONE nodes")
        (selector node))))
(define red-edges (make-edge-getter internal-node-red-edges))
(define green-edges (make-edge-getter internal-node-green-edges))
(define blue-edges (make-edge-getter internal-node-blue-edges))

; Mutators

(define (make-edge-setter mutator!) (set-bbv-version-limit! #f) 
  (lambda (node value) (set-bbv-version-limit! #f) 
    (cond ((any-node? node) (fatal-error "Can't set edges from the ANY node"))
          ((none-node? node) 'OK)
          (else (mutator! node value)))))
(define set-red-edges! (make-edge-setter set-internal-node-red-edges!))
(define set-green-edges! (make-edge-setter set-internal-node-green-edges!))
(define set-blue-edges! (make-edge-setter set-internal-node-blue-edges!))

;; BLUE EDGES

(define make-blue-edge vector)
(define (blue-edge-operation edge) (set-bbv-version-limit! #f)  (Svector-ref edge 0))
(define (blue-edge-arg-node edge) (set-bbv-version-limit! #f)  (Svector-ref edge 1))
(define (blue-edge-res-node edge) (set-bbv-version-limit! #f)  (Svector-ref edge 2))
(define (set-blue-edge-operation! edge value) (set-bbv-version-limit! #f)  (Svector-set! edge 0 value))
(define (set-blue-edge-arg-node! edge value) (set-bbv-version-limit! #f)  (Svector-set! edge 1 value))
(define (set-blue-edge-res-node! edge value) (set-bbv-version-limit! #f)  (Svector-set! edge 2 value))

; Selectors
(define operation blue-edge-operation)
(define arg-node blue-edge-arg-node)
(define res-node blue-edge-res-node)

; Mutators
(define set-arg-node! set-blue-edge-arg-node!)
(define set-res-node! set-blue-edge-res-node!)

; Higher level operations on blue edges

(define (lookup-op op node) (set-bbv-version-limit! #f) 
  (let loop ((edges (blue-edges node)))
    (cond ((null? edges) '())
          ((eq? op (operation (Scar edges))) (Scar edges))
          (else (loop (Scdr edges))))))

(define (has-op? op node) (set-bbv-version-limit! #f) 
  (not (null? (lookup-op op node))))

;; GRAPHS

(define make-internal-graph vector)
(define (internal-graph-nodes graph) (set-bbv-version-limit! #f)  (Svector-ref graph 0))
(define (internal-graph-already-met graph) (set-bbv-version-limit! #f)  (Svector-ref graph 1))
(define (internal-graph-already-joined graph) (set-bbv-version-limit! #f)  (Svector-ref graph 2))
(define (set-internal-graph-nodes! graph nodes) (set-bbv-version-limit! #f)  (Svector-set! graph 0 nodes))

; Constructor

(define (make-graph . nodes) (set-bbv-version-limit! #f) 
  (make-internal-graph nodes (make-empty-table) (make-empty-table)))

; Selectors

(define graph-nodes internal-graph-nodes)
(define already-met internal-graph-already-met)
(define already-joined internal-graph-already-joined)

; Higher level functions on graphs

(define (add-graph-nodes! graph nodes) (set-bbv-version-limit! #f) 
  (set-internal-graph-nodes! graph (cons nodes (graph-nodes graph))))

(define (copy-graph g) (set-bbv-version-limit! #f) 
  (define (copy-list l) (set-bbv-version-limit! #f)  (Svector->list (Slist->vector l)))
  (make-internal-graph
   (copy-list (graph-nodes g))
   (already-met g)
   (already-joined g)))

(define (clean-graph g) (set-bbv-version-limit! #f) 
  (define (clean-node node) (set-bbv-version-limit! #f) 
    (if (not (or (any-node? node) (none-node? node)))
        (begin
          (set-green-edges! node '())
          (set-red-edges! node '()))))
  (for-each clean-node (graph-nodes g))
  g)

(define (canonicalize-graph graph classes) (set-bbv-version-limit! #f) 
  (define (fix node) (set-bbv-version-limit! #f) 
    (define (fix-set object selector mutator) (set-bbv-version-limit! #f) 
      (mutator object 
               (map (lambda (node) (set-bbv-version-limit! #f) 
                      (find-canonical-representative node classes))
                    (selector object))))
    (if (not (or (none-node? node) (any-node? node)))
        (begin
          (fix-set node green-edges set-green-edges!)
          (fix-set node red-edges set-red-edges!)
          (for-each 
           (lambda (blue-edge) (set-bbv-version-limit! #f) 
             (set-arg-node! blue-edge
                            (find-canonical-representative (arg-node blue-edge) classes))
             (set-res-node! blue-edge
                            (find-canonical-representative (res-node blue-edge) classes)))
           (blue-edges node))))
    node)
  (define (fix-table table) (set-bbv-version-limit! #f) 
    (define (canonical? node) (set-bbv-version-limit! #f)  (eq? node (find-canonical-representative node classes)))
    (define (filter-and-fix predicate-fn update-fn list) (set-bbv-version-limit! #f) 
      (let loop ((list list))
        (cond ((null? list) '())
              ((predicate-fn (Scar list))
               (cons (update-fn (Scar list)) (loop (Scdr list))))
              (else (loop (Scdr list))))))
    (define (fix-line line) (set-bbv-version-limit! #f) 
      (filter-and-fix
       (lambda (entry) (set-bbv-version-limit! #f)  (canonical? (Scar entry)))
       (lambda (entry) (set-bbv-version-limit! #f)  (cons (Scar entry)
                             (find-canonical-representative (Scdr entry) classes)))
       line))
    (if (null? table)
        '()
        (cons (Scar table)
              (filter-and-fix
               (lambda (entry) (set-bbv-version-limit! #f)  (canonical? (Scar entry)))
               (lambda (entry) (set-bbv-version-limit! #f)  (cons (Scar entry) (fix-line (Scdr entry))))
               (Scdr table)))))
  (make-internal-graph
   (Smap2 (lambda (class) (set-bbv-version-limit! #f)  (fix (Scar class))) classes)
   (fix-table (already-met graph))
   (fix-table (already-joined graph))))

;; USEFUL NODES

(define none-node (make-node 'none #t))
(define (none-node? node) (set-bbv-version-limit! #f)  (eq? node none-node))

(define any-node (make-node 'any '()))
(define (any-node? node) (set-bbv-version-limit! #f)  (eq? node any-node))

;; COLORED EDGE TESTS

(define (green-edge? from-node to-node) (set-bbv-version-limit! #f) 
  (cond ((any-node? from-node) #f)
        ((none-node? from-node) #t)
        ((Smemq to-node (green-edges from-node)) #t)
        (else #f)))

(define (red-edge? from-node to-node) (set-bbv-version-limit! #f) 
  (cond ((any-node? from-node) #f)
        ((none-node? from-node) #t)
        ((Smemq to-node (red-edges from-node)) #t)
        (else #f)))

;; SIGNATURE

; Return signature (i.e. <arg, res>) given an operation and a node

(define sig
  (let ((none-comma-any (cons none-node any-node)))
    (lambda (op node) (set-bbv-version-limit! #f)                    ; Returns (arg, res)
      (let ((the-edge (lookup-op op node)))
        (if (not (null? the-edge))
            (cons (arg-node the-edge) (res-node the-edge))
            none-comma-any)))))

; Selectors from signature

(define (arg pair) (set-bbv-version-limit! #f)  (Scar pair))
(define (res pair) (set-bbv-version-limit! #f)  (Scdr pair))

;; CONFORMITY

(define (conforms? t1 t2) (set-bbv-version-limit! #f) 
  (define nodes-with-red-edges-out '())
  (define (add-red-edge! from-node to-node) (set-bbv-version-limit! #f) 
    (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
    (set! nodes-with-red-edges-out
          (adjoin from-node nodes-with-red-edges-out)))
  (define (greenify-red-edges! from-node) (set-bbv-version-limit! #f) 
    (set-green-edges! from-node
                      (Sappend (red-edges from-node) (green-edges from-node)))
    (set-red-edges! from-node '()))
  (define (delete-red-edges! from-node) (set-bbv-version-limit! #f) 
    (set-red-edges! from-node '()))
  (define (does-conform t1 t2) (set-bbv-version-limit! #f) 
    (cond ((or (none-node? t1) (any-node? t2)) #t)
          ((or (any-node? t1) (none-node? t2)) #f)
          ((green-edge? t1 t2) #t)
          ((red-edge? t1 t2) #t)
          (else
           (add-red-edge! t1 t2)
           (let loop ((blues (blue-edges t2)))
             (if (null? blues)
                 #t
                 (let* ((current-edge (Scar blues))
                        (phi (operation current-edge)))
                   (and (has-op? phi t1)
                        (does-conform
                         (res (sig phi t1))
                         (res (sig phi t2)))
                        (does-conform
                         (arg (sig phi t2))
                         (arg (sig phi t1)))
                        (loop (Scdr blues)))))))))
  (let ((result (does-conform t1 t2)))
    (for-each (if result greenify-red-edges! delete-red-edges!)
              nodes-with-red-edges-out)
    result))

(define (equivalent? a b) (set-bbv-version-limit! #f) 
  (and (conforms? a b) (conforms? b a)))

;; EQUIVALENCE CLASSIFICATION
; Given a list of nodes, return a list of equivalence classes

(define (classify nodes) (set-bbv-version-limit! #f) 
  (let node-loop ((classes '())
                  (nodes nodes))
    (if (null? nodes)
        (Smap2 (lambda (class) (set-bbv-version-limit! #f) 
               (sort-list class
                          (lambda (node1 node2) (set-bbv-version-limit! #f) 
                            (SFX< (Sstring-length (name node1))
                               (Sstring-length (name node2))))))
             classes)
        (let ((this-node (Scar nodes)))
          (define (add-node classes) (set-bbv-version-limit! #f) 
            (cond ((null? classes) (list (list this-node)))
                  ((equivalent? this-node (caar classes))
                   (cons (cons this-node (Scar classes))
                         (Scdr classes)))
                  (else (cons (Scar classes)
                              (add-node (Scdr classes))))))
          (node-loop (add-node classes)
                     (Scdr nodes))))))

; Given a node N and a classified set of nodes,
; find the canonical member corresponding to N

(define (find-canonical-representative element classification) (set-bbv-version-limit! #f) 
  (let loop ((classes classification))
    (cond ((null? classes) (fatal-error "Can't classify" element)) 
          ((Smemq element (Scar classes)) (Scar (Scar classes)))
          (else (loop (Scdr classes))))))

; Reduce a graph by taking only one member of each equivalence 
; class and canonicalizing all outbound pointers

(define (reduce graph) (set-bbv-version-limit! #f) 
  (let ((classes (classify (graph-nodes graph))))
    (canonicalize-graph graph classes)))

;; TWO DIMENSIONAL TABLES

(define (make-empty-table) (set-bbv-version-limit! #f)  (list 'TABLE))
(define (lookup table x y) (set-bbv-version-limit! #f) 
  (let ((one (Sassq x (Scdr table))))
    (if one
        (let ((two (Sassq y (Scdr one))))
          (if two (Scdr two) #f))
        #f)))
(define (insert! table x y value) (set-bbv-version-limit! #f) 
  (define (make-singleton-table x y) (set-bbv-version-limit! #f) 
    (list (cons x y)))
  (let ((one (Sassq x (Scdr table))))
    (if one
        (Sset-cdr! one (cons (cons y value) (Scdr one)))
        (Sset-cdr! table (cons (cons x (make-singleton-table y value))
                              (Scdr table))))))

;; MEET/JOIN 
; These update the graph when computing the node for node1*node2

(define (blue-edge-operate arg-fn res-fn graph op sig1 sig2) (set-bbv-version-limit! #f) 
  (make-blue-edge op
                  (arg-fn graph (arg sig1) (arg sig2))
                  (res-fn graph (res sig1) (res sig2))))

(define (meet graph node1 node2) (set-bbv-version-limit! #f) 
  (cond ((eq? node1 node2) node1)
        ((or (any-node? node1) (any-node? node2)) any-node) ; canonicalize
        ((none-node? node1) node2)
        ((none-node? node2) node1)
        ((lookup (already-met graph) node1 node2)) ; return it if found
        ((conforms? node1 node2) node2)
        ((conforms? node2 node1) node1)
        (else
         (let ((result
                (make-node (Sstring-append "(" (name node1) " ^ " (name node2) ")"))))
           (add-graph-nodes! graph result)
           (insert! (already-met graph) node1 node2 result)
           (set-blue-edges! result
             (map
              (lambda (op) (set-bbv-version-limit! #f) 
                (blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
              (intersect (map operation (blue-edges node1))
                         (map operation (blue-edges node2)))))
           result))))

(define (join graph node1 node2) (set-bbv-version-limit! #f) 
  (cond ((eq? node1 node2) node1)
        ((any-node? node1) node2)
        ((any-node? node2) node1)
        ((or (none-node? node1) (none-node? node2)) none-node) ; canonicalize
        ((lookup (already-joined graph) node1 node2)) ; return it if found
        ((conforms? node1 node2) node1)
        ((conforms? node2 node1) node2)
        (else
         (let ((result
                (make-node (Sstring-append "(" (name node1) " v " (name node2) ")"))))
           (add-graph-nodes! graph result)
           (insert! (already-joined graph) node1 node2 result)
           (set-blue-edges! result
             (map
              (lambda (op) (set-bbv-version-limit! #f) 
                (blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
              (union (map operation (blue-edges node1))
                     (map operation (blue-edges node2)))))
           result))))

;; MAKE A LATTICE FROM A GRAPH

(define (make-lattice g) (set-bbv-version-limit! #f) 
  (define (step g) (set-bbv-version-limit! #f) 
    (let* ((copy (copy-graph g))
           (nodes (graph-nodes copy)))
      (for-each (lambda (first) (set-bbv-version-limit! #f) 
                  (for-each (lambda (second) (set-bbv-version-limit! #f) 
                              (meet copy first second) (join copy first second))
                            nodes))
                nodes)
      copy))
  (define (loop g count) (set-bbv-version-limit! #f) 
    (let ((lattice (step g)))
      (let* ((new-g (reduce lattice))
             (new-count (length (graph-nodes new-g))))
        (if (GEN= new-count count)
            new-g
            (loop new-g new-count)))))
  (let ((graph
         (apply make-graph
                (adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
    (loop graph (length (graph-nodes graph)))))

;; DEBUG and TEST

(define a '())
(define b '())
(define c '())
(define d '())

(define (setup) (set-bbv-version-limit! #f) 
  (set! a (make-node 'a))
  (set! b (make-node 'b))
  (set-blue-edges! a (list (make-blue-edge 'phi any-node b)))
  (set-blue-edges! b (list (make-blue-edge 'phi any-node a)
                           (make-blue-edge 'theta any-node b)))
  (set! c (make-node "c"))
  (set! d (make-node "d"))
  (set-blue-edges! c (list (make-blue-edge 'theta any-node b)))
  (set-blue-edges! d (list (make-blue-edge 'phi any-node c)
                           (make-blue-edge 'theta any-node d)))
  '(made a b c d))

(define-keys (run !key (n (unknown 1000 1)))
  (let loop ((n n) (r #f))
    (if (FX= n 0)
        r
      (begin
        (setup)
        (loop (FX- n 1)
              (map name
                (graph-nodes (make-lattice (make-graph a b c d any-node none-node)))))))))

(define (check result) (set-bbv-version-limit! #f) 
  (equal? (map (lambda (s) (set-bbv-version-limit! #f) 
                     (Slist->string (map char-downcase (Sstring->list s))))
                   result)
              '("(((b v d) ^ a) v c)"
                "(c ^ d)"
                "(b v (a ^ d))"
                "((a v d) ^ b)"
                "(b v d)"
                "(b ^ (a v c))"
                "(a v (c ^ d))"
                "((b v d) ^ a)"
                "(c v (a v d))"
                "(a v c)"
                "(d v (b ^ (a v c)))"
                "(d ^ (a v c))"
                "((a ^ d) v c)"
                "((a ^ b) v d)"
                "(((a v d) ^ b) v (a ^ d))"
                "(b ^ d)"
                "(b v (a v d))"
                "(a ^ c)"
                "(b ^ (c v d))"
                "(a ^ b)"
                "(a v b)"
                "((a ^ d) ^ b)"
                "(a ^ d)"
                "(a v d)"
                "d"
                "(c v d)"
                "a"
                "b"
                "c"
                "any"
                "none")))
