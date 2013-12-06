; Some utility functions that you may find useful.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Problem 18

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
      (cond 
    ((null? list1) list2)
    ((null? list2) list1)
    (else 
     (if 
      (comp (car list1) (car list2))
      (cons (car list1) (merge comp (cdr list1) list2))
      (cons (car list2)  (merge comp (cdr list2) list1))
      ))))

(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Sort a list of lists of numbers to be in decreasing lexicographic
;; order. Relies on a correct implementation of merge.
(define (sort-lists lsts)
  (if (or (null? lsts) (null? (cdr lsts)))
      lsts
      (let ((sublsts (split lsts)))
        (merge greater-list
               (sort-lists (car sublsts))
               (sort-lists (cdr sublsts))))))

(define (greater-list x y)
  (cond ((null? y) #t)
        ((null? x) #f)
        ((> (car x) (car y)) #t)
        ((> (car y) (car x)) #f)
        (else (greater-list (cdr x) (cdr y)))))

(define (split x)
  (cond ((or (null? x) (null? (cdr x))) (cons x nil))
        (else (let ((sublsts (split (cdr (cdr x)))))
                (cons (cons (car x) (car sublsts))
                      (cons (car (cdr x)) (cdr sublsts)))))))

(merge greater-list '((3 2 1) (1 1) (0)) '((4 0) (3 2 0) (3 2) (1)))
; expect ((4 0) (3 2 1) (3 2 0) (3 2) (1 1) (1) (0))


; Problem 19

;; A list of all ways to partition TOTAL, where  each partition must
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.
(define (list-partitions total max-pieces max-value)
 (define result (iter-max total max-pieces max-value '()))
 result
 (define result (filter (lambda (x) (and (not (null? (car x))) (or (< (len x 0) max-pieces) (= (len x 0) max-pieces)))) result))
 result
)

(define (iter-max total max-pieces max-value L)
  (define L (list-helper-depth total max-pieces max-value L))
  (if (< max-value (/ total 2) )
      L
      (iter-max total max-pieces (- max-value 1) L))
)
(define (list-helper-depth total max-pieces max-value L)
    (define p (list-helper total max-pieces max-value))

    (define new-p (cons p L))
   ;; (if (or (< (len new-p 0) max-pieces) (= (len new-p 0) max-pieces))
        (define L new-p)
    ;;)
    
    (define new-total  (list-helper (car (cdr p)) max-pieces (- (car (cdr p)) 1 )))
    (define to-pass (car new-total))
    

    (define  new-e (cons (car p) new-total))
    ;;(if (or (< (len new-e 0) max-pieces) (= (len new-e 0) max-pieces))
        (define L (cons new-e L))
    ;;)
    (define L (add-more to-pass max-pieces max-value L))
    L
)
(define (add-more to-pass max-pieces max-value L)
     
     (define new-total (list-helper to-pass max-pieces (- to-pass 1)))
     (define first (car (car L)))
     (define rest (cdr (cdr (car L))))
     (define e (append (list first) new-total rest))
      
     ; (if (or (< (len e 0) max-pieces) (= (len e 0) max-pieces))
       (define L (cons e L ))
     ;)
     
     (define to-pass (car (cdr (car L))))
      (if (= 1 to-pass)
           L
          (add-more to-pass max-pieces (- to-pass 1) L))
  )
(define (list-helper total max-pieces max-value)
  ;(display total)(display max-value)
    (cond ((= 1 total) (list total))
        ((> 1 total)  nil)
        ((= 0 max-value) nil)
        ((= 1 max-pieces) (list total))
        (else 
              (append (list max-value) (list-helper (- total max-value)   max-pieces  (- total max-value))))
        ))

(define (len L count)
  (if (null? L)
    count
    (len (cdr L) (+ count 1))
 )
)
; Problem 19 tests rely on correct Problem 18.
(sort-lists (list-partitions 5 2 4))
; expect ((4 1) (3 2))
(sort-lists (list-partitions 7 3 5))
; expect ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))


; Problem 20

;; The Tree abstract data type has an entry and a list of children.
(define (make-tree entry children)
  (cons entry children))
(define (entry tree)
  (car tree))
(define (children tree)
  (cdr tree))

;; An example tree:
;;                5
;;       +--------+--------+
;;       |        |        |
;;       6        7        2
;;    +--+--+     |     +--+--+
;;    |     |     |     |     |
;;    9     8     1     6     4
;;                      |
;;                      |
;;                      3
(define tree
  (make-tree 5 (list
                (make-tree 6 (list
                              (make-tree 9 nil)
                              (make-tree 8 nil)))
                (make-tree 7 (list
                              (make-tree 1 nil)))
                (make-tree 2 (list
                              (make-tree 6 (list
                                            (make-tree 3 nil)))
                              (make-tree 4 nil))))))

;; Takes a TREE of numbers and outputs a list of sums from following each
;; possible path from root to leaf.

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define (leaf? node)
  (null? (children node)))

(define (all-leaf? children)
  (define bool-list (map (lambda (x) (leaf? x)) children))
  (define bool (filter (lambda (x) (= #f x)) bool-list))

  (null? bool)
)


(define (tree-sums tree)
(if (leaf? tree) 
      tree
      (map (lambda (x) (+ x (entry tree))) (flatten  (map (lambda (x) (helper  x)) (children tree)))))
)

(define (helper tree)

(cond ((leaf? tree) (list (entry tree)))
         ((all-leaf? (children tree)) (map (lambda (x) (+ (entry tree) x)) (flatten (children tree))))
         (else  (map (lambda (x) (+ (entry tree) x)) 
         (map (lambda (x) (accumulate + 0 x)) (map (lambda (x) (flatten x)) (children tree)))))
  ))





  




  ;;(define x (entry tree)) 
  ;;(define y (entry (entry (children tree))))
  ;;(define z (entry (children (entry (children tree)))))
  


  

  ;;(define z1 (entry (children (children (entry (children tree))))))
  

  ;;(define y1 (entry (entry (children (children tree)))))
  ;;(define z2  (entry (children (entry (children (children tree))))))

   
   
  ;;(define y2 (entry (entry (children (children (children tree))))))
  ;;(define z3  (entry (entry (children (entry (children (children (children tree))))))))
  ;;(define w (entry (children (entry (children (entry (children (children (children tree)))))))))



  ;;(define z4   (entry (children (children (entry (children (children (children tree))))))))


  ;;(define L (list (cons x (cons y z)) (cons x (cons y z1)) (cons x (cons y1 z2)) (cons x (cons y2 (cons z3 w)) )
   ;;(cons x (cons y2 z4))))

  ;;(list (accumulate + 0 (car L)) (accumulate + 0 (car (cdr L)))
    ;; (accumulate + 0 (car (cdr (cdr L)))) (accumulate + 0 (car (cdr (cdr (cdr L)))))
     ;;(accumulate + 0 (car (cdr (cdr (cdr (cdr L)))))))
  ;;)

 
;(entry (entry (entry (children tree)))))
 ;(accumulate + 0 x ))
  ;(accumulate + (entr—y tree) (children tree)))
  



  ;(if (null? (cdr tree)) 
   ; (cons (car tree) nil)
    ;(cons (+ (car tree) (car (tree-sums (car (cdr tree))))) 
     ; nil)
  ;))
(define (children-list tree)
  (if (null? (children tree))
      (entry tree)
      (children-list (children tree))
))
 

(tree-sums tree)
; expect (20 19 13 16 11)


; Problem 21 (optional)

; Draw the hax image using turtle graphics.
(define (hax d k)
  ; *** YOUR CODE HERE ***
  nil)