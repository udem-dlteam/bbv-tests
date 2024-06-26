;;; MAZE -- Constructs a maze on a hexagonal grid, written by Olin Shivers.

;------------------------------------------------------------------------------
; Was file "rand.scm".

; Minimal Standard Random Number Generator
; Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
; better constants, as proposed by Park.
; By Ozan Yigit

;;; Rehacked by Olin 4/1995.

(define (random-state n) (set-bbv-version-limit! #f) 
  (cons n #f))

(define (fx32 a) (set-bbv-version-limit! #f) 
   (SFXbit-and (SFX- (SFXbit-lsh 1 31) 1) a))

(define (rand state) (set-bbv-version-limit! #f) 
  (let ((seed (Scar state))
        (A 2813) ; 48271
        (M 8388607) ; 2147483647
        (Q 2787) ; 44488
        (R 2699)) ; 3399
    (let* ((hi (fx32 (SFXquotient seed Q)))
           (lo (fx32 (SFXmodulo seed Q)))
           (test (fx32 (SFX- (fx32 (SFX* A lo)) (fx32 (SFX* R hi)))))
           (val (if (SFX> test 0) test (SFX+ test M))))
      (Sset-car! state val)
      val)))

(define (random-int n state) (set-bbv-version-limit! #f) 
   (fx32 (SFXmodulo (rand state) n)))

; poker test
; seed 1
; cards 0-9 inclusive (random 10)
; five cards per hand
; 10000 hands
;
; Poker Hand     Example    Probability  Calculated
; 5 of a kind    (aaaaa)      0.0001      0
; 4 of a kind    (aaaab)      0.0045      0.0053
; Full house     (aaabb)      0.009       0.0093
; 3 of a kind    (aaabc)      0.072       0.0682
; two pairs      (aabbc)      0.108       0.1104
; Pair           (aabcd)      0.504       0.501
; Bust           (abcde)      0.3024      0.3058

; (define (random n) (set-bbv-version-limit! #f) 
;   (let* ((M 2147483647)
;        (slop (SFXmodulo M n)))
;     (let loop ((r (rand)))
;       (if (SFX> r slop)
;         (SFXmodulo r n)  
;         (loop (rand))))))
; 
; (define (rngtest) (set-bbv-version-limit! #f) 
;   (display "implementation ")
;   (srand 1)
;   (let loop ((n 0))
;     (if (SFX< n 10000)
;         (begin
;          (rand)
;          (loop (1+ n)))))
;   (if (SFX= *seed* 399268537)
;       (display "looks correct.")
;       (begin
;        (display "failed.")
;        (newline)
;        (display "   current seed ") (display *seed*)
;        (newline)
;        (display "   correct seed 399268537")))
;   (newline))

;------------------------------------------------------------------------------
; Was file "uf.scm".

;;; Tarjan's amortised union-find data structure.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This data structure implements disjoint sets of elements.
;;; Four operations are supported. The implementation is extremely
;;; fast -- any sequence of N operations can be performed in time
;;; so close to linear it's laughable how close it is. See your
;;; intro data structures book for more. The operations are:
;;;
;;; - (base-set nelts) -> set
;;;   Returns a new set, of size NELTS.
;;;
;;; - (set-size s) -> integer
;;;   Returns the number of elements in set S.
;;;
;;; - (union! set1 set2)
;;;   Unions the two sets -- SET1 and SET2 are now considered the same set
;;;   by SET-EQUAL?.
;;;
;;; - (set-equal? set1 set2)
;;;   Returns true <==> the two sets are the same.

;;; Representation: a set is a cons cell. Every set has a "representative"
;;; cons cell, reached by chasing cdr links until we find the cons with
;;; cdr = (). Set equality is determined by comparing representatives using
;;; EQ?. A representative's car contains the number of elements in the set.

;;; The speed of the algorithm comes because when we chase links to find 
;;; representatives, we collapse links by changing all the cells in the path
;;; we followed to point directly to the representative, so that next time
;;; we walk the cdr-chain, we'll go directly to the representative in one hop.


(define (base-set nelts) (set-bbv-version-limit! #f)  (cons nelts '()))

;;; Sets are chained together through cdr links. Last guy in the chain
;;; is the root of the set.

(define (get-set-root s) (set-bbv-version-limit! #f) 
  (let lp ((r s))                       ; Find the last pair
    (let ((next (Scdr r)))               ; in the list. That's
      (cond ((pair? next) (lp next))    ; the root r.

            (else
             (if (not (eq? r s))        ; Now zip down the list again,
                 (let lp ((x s))        ; changing everyone's cdr to r.
                   (let ((next (Scdr x)))        
                     (cond ((not (eq? r next))
                            (Sset-cdr! x r)
                            (lp next))))))
             r)))))                     ; Then return r.

(define (set-equal? s1 s2) (set-bbv-version-limit! #f)  (eq? (get-set-root s1) (get-set-root s2)))

(define (set-size s) (set-bbv-version-limit! #f)  (Scar (get-set-root s)))

(define (union! s1 s2) (set-bbv-version-limit! #f) 
  (let* ((r1 (get-set-root s1))
         (r2 (get-set-root s2))
         (n1 (set-size r1))
         (n2 (set-size r2))
         (n  (SFX+ n1 n2)))

    (cond ((SFX> n1 n2)
           (Sset-cdr! r2 r1)
           (Sset-car! r1 n))
          (else
           (Sset-cdr! r1 r2)
           (Sset-car! r2 n)))))

;------------------------------------------------------------------------------
; Was file "maze.scm".

;;; Building mazes with union/find disjoint sets.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This is the algorithmic core of the maze constructor.
;;; External dependencies:
;;; - RANDOM-INT
;;; - Union/find code
;;; - bitwise logical functions

; (define-record wall
;   owner         ; Cell that owns this wall.
;   neighbor      ; The other cell bordering this wall.
;   bit)          ; Integer -- a bit identifying this wall in OWNER's cell.

; (define-record cell
;   reachable     ; Union/find set -- all reachable cells.
;   id            ; Identifying info (e.g., the coords of the cell).
;   (walls -1)    ; A bitset telling which walls are still standing.
;   (parent #f)   ; For DFS spanning tree construction.
;   (mark #f))    ; For marking the solution path.

(define-macro (make-wall owner neighbor bit)
  `(vector 'wall ,owner ,neighbor ,bit))

(define-macro (wall:owner o)          `(Svector-ref ,o 1))
(define-macro (set-wall:owner o v)    `(Svector-set! ,o 1 ,v))
(define-macro (wall:neighbor o)       `(Svector-ref ,o 2))
(define-macro (set-wall:neighbor o v) `(Svector-set! ,o 2 ,v))
(define-macro (wall:bit o)            `(Svector-ref ,o 3))
(define-macro (set-wall:bit o v)      `(Svector-set! ,o 3 ,v))

(define-macro (make-cell reachable id)
  `(vector 'cell ,reachable ,id -1 #f #f))

(define-macro (cell:reachable o)       `(Svector-ref ,o 1))
(define-macro (set-cell:reachable o v) `(Svector-set! ,o 1 ,v))
(define-macro (cell:id o)              `(Svector-ref ,o 2))
(define-macro (set-cell:id o v)        `(Svector-set! ,o 2 ,v))
(define-macro (cell:walls o)           `(Svector-ref ,o 3))
(define-macro (set-cell:walls o v)     `(Svector-set! ,o 3 ,v))
(define-macro (cell:parent o)          `(Svector-ref ,o 4))
(define-macro (set-cell:parent o v)    `(Svector-set! ,o 4 ,v))
(define-macro (cell:mark o)            `(Svector-ref ,o 5))
(define-macro (set-cell:mark o v)      `(Svector-set! ,o 5 ,v))

;;; Iterates in reverse order.

(define (vector-for-each proc v) (set-bbv-version-limit! #f) 
  (let lp ((i (SFX- (Svector-length v) 1)))
    (cond ((SFX>= i 0)
           (proc (Svector-ref v i))
           (lp (SFX- i 1))))))

(define (vector-for-each-exit-if proc v) (set-bbv-version-limit! #f) 
  (let lp ((i (SFX- (Svector-length v) 1)))
    (cond ((SFX>= i 0)
           (or (proc (Svector-ref v i))
               (lp (SFX- i 1)))))))


;;; Randomly permute a vector.

(define (permute-vec! v random-state) (set-bbv-version-limit! #f) 
  (let lp ((i (SFX- (Svector-length v) 1)))
    (cond ((SFX> i 1)
           (let ((elt-i (Svector-ref v i))
                 (j (random-int i random-state)))       ; j in [0,i)
             (Svector-set! v i (Svector-ref v j))
             (Svector-set! v j elt-i))
           (lp (SFX- i 1)))))
  v)


;;; This is the core of the algorithm.

(define (dig-maze walls ncells) (set-bbv-version-limit! #f) 
  (vector-for-each-exit-if
   (lambda (wall) (set-bbv-version-limit! #f)                         ; For each wall,
     (let* ((c1   (wall:owner wall))     ; find the cells on
            (set1 (cell:reachable c1))

            (c2   (wall:neighbor wall)) ; each side of the wall
            (set2 (cell:reachable c2)))

       ;; If there is no path from c1 to c2, knock down the
       ;; wall and union the two sets of reachable cells.
       ;; If the new set of reachable cells is the whole set
       ;; of cells, quit.
       (and (not (set-equal? set1 set2))
            (let ((walls (cell:walls c1))
                  (wall-mask (SFXbit-not (wall:bit wall))))
              (union! set1 set2)
              (set-cell:walls c1 (SFXbit-and walls wall-mask))
              (SFX= (set-size set1) ncells)))))
   walls))


;;; Some simple DFS routines useful for determining path length 
;;; through the maze.

;;; Build a DFS tree from ROOT. 
;;; (DO-CHILDREN proc maze node) applies PROC to each of NODE's children.
;;; We assume there are no loops in the maze; if this is incorrect, the
;;; algorithm will diverge.

(define (dfs-maze maze root do-children) (set-bbv-version-limit! #f) 
  (let search ((node root) (parent #f))
    (set-cell:parent node parent)
    (do-children (lambda (child) (set-bbv-version-limit! #f) 
                   (if (not (eq? child parent))
                       (search child node)))
                 maze node)))

;;; Move the root to NEW-ROOT.

(define (reroot-maze new-root) (set-bbv-version-limit! #f) 
  (let lp ((node new-root) (new-parent #f))
    (let ((old-parent (cell:parent node)))
      (set-cell:parent node new-parent)
      (if old-parent (lp old-parent node)))))

;;; How far from CELL to the root?

(define (path-length cell) (set-bbv-version-limit! #f) 
  (do ((len 0 (SFX+ len 1))
       (node (cell:parent cell) (cell:parent node)))
      ((not node) len)))

;;; Mark the nodes from NODE back to root. Used to mark the winning path.

(define (mark-path node) (set-bbv-version-limit! #f) 
  (let lp ((node node))
    (set-cell:mark node #t)
    (cond ((cell:parent node) => lp))))

;------------------------------------------------------------------------------
; Was file "harr.scm".

;;; Hex arrays
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - define-record

;;;        ___       ___       ___
;;;       /   \     /   \     /   \
;;;   ___/  A  \___/  A  \___/  A  \___
;;;  /   \     /   \     /   \     /   \
;;; /  A  \___/  A  \___/  A  \___/  A  \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/

;;; Hex arrays are indexed by the (x,y) coord of the center of the hexagonal
;;; element. Hexes are three wide and two high; e.g., to get from the center
;;; of an elt to its {NW, N, NE} neighbors, add {(-3,1), (0,2), (3,1)}
;;; respectively.
;;;
;;; Hex arrays are represented with a matrix, essentially made by shoving the
;;; odd columns down a half-cell so things line up. The mapping is as follows:
;;;     Center coord      row/column
;;;     ------------      ----------
;;;     (x,  y)        -> (y/2, x/3)
;;;     (3c, 2r + c&1) <- (r,   c)


; (define-record harr
;   nrows
;   ncols
;   elts)

(define-macro (make-harr nrows ncols elts)
  `(vector 'harr ,nrows ,ncols ,elts))

(define-macro (harr:nrows o)       `(Svector-ref ,o 1))
(define-macro (set-harr:nrows o v) `(Svector-set! ,o 1 ,v))
(define-macro (harr:ncols o)       `(Svector-ref ,o 2))
(define-macro (set-harr:ncols o v) `(Svector-set! ,o 2 ,v))
(define-macro (harr:elts o)        `(Svector-ref ,o 3))
(define-macro (set-harr:elts o v)  `(Svector-set! ,o 3 ,v))

(define-macro (harr r c)
  `(let ((r ,r) (c ,c))
    (make-harr r c (Smake-vector1 (SFX* r c)))))

(define (href ha x y) (set-bbv-version-limit! #f) 
  (let ((r (SFXquotient y 2))
        (c (SFXquotient x 3)))
    (Svector-ref (harr:elts ha)
                (SFX+ (SFX* (harr:ncols ha) r) c))))

(define (hset! ha x y val) (set-bbv-version-limit! #f) 
  (let ((r (SFXquotient y 2))
        (c (SFXquotient x 3)))
    (Svector-set! (harr:elts ha)
                 (SFX+ (SFX* (harr:ncols ha) r) c)
                 val)))

(define (href/rc ha r c) (set-bbv-version-limit! #f) 
    (Svector-ref (harr:elts ha)
                (SFX+ (SFX* (harr:ncols ha) r) c)))

;;; Create a nrows x ncols hex array. The elt centered on coord (x, y)
;;; is the value returned by (PROC x y).

(define (harr-tabulate nrows ncols proc) (set-bbv-version-limit! #f) 
  (let ((v (Smake-vector1 (SFX* nrows ncols))))

    (do ((r (SFX- nrows 1) (SFX- r 1)))
        ((SFX< r 0))
      (do ((c 0 (SFX+ c 1))
           (i (SFX* r ncols) (SFX+ i 1)))
          ((SFX= c ncols))
        (Svector-set! v i (proc (SFX* 3 c) (SFX+ (SFX* 2 r) (SFXbit-and c 1))))))

    (make-harr nrows ncols v)))


(define (harr-for-each proc harr) (set-bbv-version-limit! #f) 
  (vector-for-each proc (harr:elts harr)))

;------------------------------------------------------------------------------
; Was file "hex.scm".

;;; Hexagonal hackery for maze generation.
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - cell and wall records
;;; - Functional Postscript for HEXES->PATH
;;; - logical functions for bit hacking
;;; - hex array code.

;;; To have the maze span (0,0) to (1,1):
;;; (scale (/ (SFX+ 1 (SFX* 3 ncols))) (/ (SFX+ 1 (SFX* 2 nrows)))
;;;        (translate (point 2 1) maze))

;;; Every elt of the hex array manages his SW, S, and SE wall.
;;; Terminology: - An even column is one whose column index is even. That
;;;                means the first, third, ... columns (indices 0, 2, ...).
;;;              - An odd column is one whose column index is odd. That
;;;                means the second, fourth... columns (indices 1, 3, ...).
;;;              The even/odd flip-flop is confusing; be careful to keep it
;;;              straight. The *even* columns are the low ones. The *odd*
;;;              columns are the high ones.
;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/
;;;  0 1 2 3

(define south-west 1)
(define south      2)
(define south-east 4)

(define (gen-maze-array r c) (set-bbv-version-limit! #f) 
  (harr-tabulate r c (lambda (x y) (set-bbv-version-limit! #f)  (make-cell (base-set 1) (cons x y)))))

;;; This could be made more efficient.
(define (make-wall-vec harr) (set-bbv-version-limit! #f) 
  (let* ((nrows (harr:nrows harr))
         (ncols (harr:ncols harr))
         (xmax (SFX* 3 (SFX- ncols 1)))

         ;; Accumulate walls.
         (walls '())
         (add-wall (lambda (o n b) (set-bbv-version-limit! #f)  ; owner neighbor bit
                     (set! walls (cons (make-wall o n b) walls)))))
        
    ;; Do everything but the bottom row.
    (do ((x (SFX* (SFX- ncols 1) 3) (SFX- x 3)))
        ((SFX< x 0))
      (do ((y (SFX+ (SFX* (SFX- nrows 1) 2) (SFXbit-and x 1))
              (SFX- y 2)))
          ((SFX<= y 1))    ; Don't do bottom row.
          (let ((hex (href harr x y)))
            (if (not (SFXzero? x))
                (add-wall hex (href harr (SFX- x 3) (SFX- y 1)) south-west))
            (add-wall hex (href harr x (SFX- y 2)) south)
            (if (SFX< x xmax)
                (add-wall hex (href harr (SFX+ x 3) (SFX- y 1)) south-east)))))

    ;; Do the SE and SW walls of the odd columns on the bottom row.
    ;; If the rightmost bottom hex lies in an odd column, however,
    ;; don't add it's SE wall -- it's a corner hex, and has no SE neighbor.
    (if (SFX> ncols 1)
        (let ((rmoc-x (SFX+ 3 (SFX* 6 (SFXquotient (SFX- ncols 2) 2)))))
          ;; Do rightmost odd col.
          (let ((rmoc-hex (href harr rmoc-x 1)))
            (if (SFX< rmoc-x xmax) ; Not  a corner -- do E wall.
                (add-wall rmoc-hex (href harr xmax 0) south-east))
            (add-wall rmoc-hex (href harr (SFX- rmoc-x 3) 0) south-west))

          (do ((x (SFX- rmoc-x 6) ; Do the rest of the bottom row's odd cols.
                  (SFX- x 6)))
              ((SFX< x 3)) ; 3 is X coord of leftmost odd column.
            (add-wall (href harr x 1) (href harr (SFX- x 3) 0) south-west)
            (add-wall (href harr x 1) (href harr (SFX+ x 3) 0) south-east))))

    (Slist->vector walls)))


;;; Find the cell ctop from the top row, and the cell cbot from the bottom
;;; row such that cbot is furthest from ctop. 
;;; Return [ctop-x, ctop-y, cbot-x, cbot-y].

(define (pick-entrances harr) (set-bbv-version-limit! #f) 
  (dfs-maze harr (href/rc harr 0 0) for-each-hex-child)
  (let ((nrows (harr:nrows harr))
        (ncols (harr:ncols harr)))
    (let tp-lp ((max-len -1)
                (entrance #f)
                (exit #f)
                (tcol (SFX- ncols 1)))
      (if (SFX< tcol 0) (vector entrance exit)
          (let ((top-cell (href/rc harr (SFX- nrows 1) tcol)))
            (reroot-maze top-cell)
            (let ((result
                    (let bt-lp ((max-len max-len)
                                (entrance entrance)
                                (exit exit)
                                (bcol (SFX- ncols 1)))
;                     (format #t "~a ~a ~a ~a~%" max-len entrance exit bcol)
                      (if (SFX< bcol 0) (vector max-len entrance exit)
                          (let ((this-len (path-length (href/rc harr 0 bcol))))
                            (if (SFX> this-len max-len)
                                (bt-lp this-len tcol bcol (SFX- bcol 1))
                                (bt-lp max-len  entrance exit (SFX- bcol 1))))))))
              (let ((max-len (Svector-ref result 0))
                    (entrance (Svector-ref result 1))
                    (exit (Svector-ref result 2)))
                (tp-lp max-len entrance exit (SFX- tcol 1)))))))))
                


;;; Apply PROC to each node reachable from CELL.
(define (for-each-hex-child proc harr cell) (set-bbv-version-limit! #f) 
  (let* ((walls (cell:walls cell))
         (id (cell:id cell))
         (x (Scar id))
         (y (Scdr id))
         (nr (harr:nrows harr))
         (nc (harr:ncols harr))
         (maxy (SFX* 2 (SFX- nr 1)))
         (maxx (SFX* 3 (SFX- nc 1))))
    (if (not (bit-test walls south-west)) (proc (href harr (SFX- x 3) (SFX- y 1))))
    (if (not (bit-test walls south))      (proc (href harr x       (SFX- y 2))))
    (if (not (bit-test walls south-east)) (proc (href harr (SFX+ x 3) (SFX- y 1))))

    ;; NW neighbor, if there is one (we may be in col 1, or top row/odd col)
    (if (and (SFX> x 0)    ; Not in first column.
             (or (SFX<= y maxy)            ; Not on top row or
                 (SFXzero? (SFXmodulo x 6)))) ; not in an odd column.
        (let ((nw (href harr (SFX- x 3) (SFX+ y 1))))
          (if (not (bit-test (cell:walls nw) south-east)) (proc nw))))

    ;; N neighbor, if there is one (we may be on top row).
    (if (SFX< y maxy)              ; Not on top row
        (let ((n (href harr x (SFX+ y 2))))
          (if (not (bit-test (cell:walls n) south)) (proc n))))

    ;; NE neighbor, if there is one (we may be in last col, or top row/odd col)
    (if (and (SFX< x maxx) ; Not in last column.
             (or (SFX<= y maxy)            ; Not on top row or
                 (SFXzero? (SFXmodulo x 6)))) ; not in an odd column.
        (let ((ne (href harr (SFX+ x 3) (SFX+ y 1))))
          (if (not (bit-test (cell:walls ne) south-west)) (proc ne))))))



;;; The top-level
(define (make-maze nrows ncols) (set-bbv-version-limit! #f) 
  (let* ((cells (gen-maze-array nrows ncols))
         (walls (permute-vec! (make-wall-vec cells) (random-state 20))))
    (dig-maze walls (SFX* nrows ncols))
    (let ((result (pick-entrances cells)))
      (let ((entrance (Svector-ref result 0))
            (exit (Svector-ref result 1)))
        (let* ((exit-cell (href/rc cells 0 exit))
               (walls (cell:walls exit-cell)))
          (reroot-maze (href/rc cells (SFX- nrows 1) entrance))
          (mark-path exit-cell)
          (set-cell:walls exit-cell (SFXbit-and walls (SFXbit-not south)))
          (vector cells entrance exit))))))


(define (pmaze nrows ncols) (set-bbv-version-limit! #f) 
  (let ((result (make-maze nrows ncols)))
    (let ((cells (Svector-ref result 0))
          (entrance (Svector-ref result 1))
          (exit (Svector-ref result 2)))
      (print-hexmaze cells entrance))))

;------------------------------------------------------------------------------
; Was file "hexprint.scm".

;;; Print out a hex array with characters.
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - hex array code
;;; - hex cell code

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ 

;;; Top part of top row looks like this:
;;;    _   _  _   _
;;;  _/ \_/ \/ \_/ \
;;; /        

(set-custom-version-limits! 2 1 5 4 4 3 2 9 2 10 7 1 1 2 4 4 9 10 1 9 4 9 7 4 8
  10 5 1 3 7 6 5 3 4 6 2 2 7 2 6 6)

(define output #f) ; the list of all characters written out, in reverse order.

(define (write-ch c) (set-bbv-version-limit! #f) 
  (set! output (cons c output)))

(define (print-hexmaze harr entrance) (set-bbv-version-limit! #f) 
  (let* ((nrows  (harr:nrows harr))
         (ncols  (harr:ncols harr))
         (ncols2 (SFX* 2 (SFXquotient ncols 2))))

    ;; Print out the flat tops for the top row's odd cols.
    (do ((c 1 (SFX+ c 2)))
        ((SFX>= c ncols))
;     (display "   ")
      (write-ch #\space)
      (write-ch #\space)
      (write-ch #\space)
      (write-ch (if (SFX= c entrance) #\space #\_)))
;   (newline)
    (write-ch #\newline)

    ;; Print out the slanted tops for the top row's odd cols
    ;; and the flat tops for the top row's even cols.
    (write-ch #\space)
    (do ((c 0 (SFX+ c 2)))
        ((SFX>= c ncols2))
;     (format #t "~a/~a\\"
;             (if (SFX= c entrance) #\space #\_)
;             (dot/space harr (SFX- nrows 1) (SFX+ c 1)))
      (write-ch (if (SFX= c entrance) #\space #\_))
      (write-ch #\/)
      (write-ch (dot/space harr (SFX- nrows 1) (SFX+ c 1)))
      (write-ch #\\))
    (if (SFXodd? ncols)
        (write-ch (if (SFX= entrance (SFX- ncols 1)) #\space #\_)))
;   (newline)
    (write-ch #\newline)

    (do ((r (SFX- nrows 1) (SFX- r 1)))
        ((SFX< r 0))

      ;; Do the bottoms for row r's odd cols.
      (write-ch #\/)
      (do ((c 1 (SFX+ c 2)))
          ((SFX>= c ncols2))
        ;; The dot/space for the even col just behind c.
        (write-ch (dot/space harr r (SFX- c 1)))
        (display-hexbottom (cell:walls (href/rc harr r c))))    

      (cond ((SFXodd? ncols)
             (write-ch (dot/space harr r (SFX- ncols 1)))
             (write-ch #\\)))
;     (newline)
      (write-ch #\newline)

      ;; Do the bottoms for row r's even cols.
      (do ((c 0 (SFX+ c 2)))
          ((SFX>= c ncols2))
        (display-hexbottom (cell:walls (href/rc harr r c)))
        ;; The dot/space is for the odd col just after c, on row below.
        (write-ch (dot/space harr (SFX- r 1) (SFX+ c 1))))
      
      (cond ((SFXodd? ncols)
             (display-hexbottom (cell:walls (href/rc harr r (SFX- ncols 1)))))
            ((not (SFXzero? r)) (write-ch #\\)))
;     (newline)
      (write-ch #\newline))))

(define (bit-test j bit) (set-bbv-version-limit! #f) 
  (not (SFXzero? (SFXbit-and j bit))))

;;; Return a . if harr[r,c] is marked, otherwise a space.
;;; We use the dot to mark the solution path.
(define (dot/space harr r c) (set-bbv-version-limit! #f) 
  (if (and (SFX>= r 0) (cell:mark (href/rc harr r c))) #\. #\space))

;;; Print a \_/ hex bottom.
(define (display-hexbottom hexwalls) (set-bbv-version-limit! #f) 
  (write-ch (if (bit-test hexwalls south-west) #\\ #\space))
  (write-ch (if (bit-test hexwalls south     ) #\_ #\space))
  (write-ch (if (bit-test hexwalls south-east) #\/ #\space)))

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/

;------------------------------------------------------------------------------

(define (run1 nrows ncols) (set-bbv-version-limit! #f) 
  (set! output '())
  (pmaze nrows ncols)
  (Sreverse output))

(define output-expected "   _       _
 _/.\\_/.\\_/ \\_
/ \\  . _/ \\ / \\
\\_ .\\_/   / \\ /
/. _/ \\ /  _/ \\
\\_ .\\ / \\_   _/
/ \\_ .\\_/  _/ \\
\\_    /  _/  _/
/ \\_/.\\ / \\ / \\
\\_   _ . _/. _/
/ \\_  \\_ . _ .\\
\\_  \\_/ \\_/   /
/ \\ /   /.\\_/.\\
\\_  \\_/    .\\ /
/  _/ \\_/.\\_ .\\
\\_  \\ /. _/ \\_/
/  _/ \\_ .\\ / \\
\\    _   _ . _/
/ \\_/ \\_/.\\ / \\
\\_  \\_ . _ . _/
/ \\_/ \\ / \\_/ \\
\\ /  _/. _/  _/
/ \\  . _/.\\_  \\
\\_  \\  .\\  . _/
/ \\ / \\_ .\\   \\
\\_  \\_/  _/.\\_/
/  _  \\ /   / \\
\\_  \\ / \\_/.\\ /
/ \\_/ \\_/.  / \\
\\_  \\_/. _/  _/
/  _/.  /     \\
\\_ . _/ \\_/ \\_/
/  _ .\\_/ \\_/ \\
\\_/. _/  _/. _/
/  _ . _/. _ .\\
\\_/   /   /   /
/ \\_/.\\_/.\\_/.\\
\\   \\_ . _/. _/
/ \\_   _  \\   \\
\\ /   / \\ /.\\_/
/ \\_/ \\_  \\_ .\\
\\_/ \\_/ \\_/ \\ /
")

(define-keys (run !key (n (unknown 50000 1)) (nrows (unknown 20)) (ncols (unknown 7)))
  (let loop ((n n) (result #f))
    (if (SFX> n 0)
        (loop (SFX- n 1) (run1 nrows ncols))
        result)))

(define (check result) (set-bbv-version-limit! #f) 
  (string=? (list->string result) output-expected))
