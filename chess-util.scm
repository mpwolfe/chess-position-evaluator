;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.



(define *wpieces* (make-vector 16))
(define *bpieces* (make-vector 16))
(define *board* (make-vector 121))

;;(define *htsqs* (make-hash-table 'equal?))
;;(define *htsqs* (make-hashtable))
(define *htsqs* (make-table))
(define *htalgebra* (make-table))
(define *ht* (make-table))
(define *enpassant* (list #f))


;; ex: (readlist '(1 2 3 4 5) 3) Index starts at 1
; caller passing in element of list to retrieve
; ex: i want the 1st element of the *wpieces* vector at index=1 element 1
; in reality index=1 would be the 'car of the list
(define readlist
  (lambda (ls index)
    (let ((rec (lambda (break)      ;; this will be the continuation
                 (let f ((ls ls)
                         (i 1))
                   (cond ((null? ls)
                          #t)
                         ((= i index)
                          (break (car ls)))    ;; break out of loop
                         (else
                          (f (cdr ls) (+ 1 i)))) ))))  ;; matches receiver
      (call/cc rec)) ))


;; change value in a list
;; list, index (note: starting at 0) and new value
(define setlist!
  (lambda (ls index value)
    (println "setlist! beginning list -> " ls " index " index " value " value)
    (let ((rec (lambda (break)      ;; this will be the continuation
                 (let f ((ls ls)
                         (i 1))
                   ;(println "setlist! list -> " ls " index " i)
                   (cond ((null? ls)
                          #t)
                         ((= i index)
                         ; (println "setlist!, before CHANGE (cdr ls) " (cdr ls) " new value " value)
                          (set-car! (cdr ls) value)   ;; change value of element of list
                         ; (println "setlist!, after CHANGE (cdr ls) " (cdr ls) " new value " value " ls " ls)
                          (break #t))    ;; break out of loop
                         (else
                          (f (cdr ls) (+ 1 i)))) ))))  ;; matches receiver
      (call/cc rec)) ))


;; use this procedure when index = 0
(define setlist1!
  (lambda (ls index value)
    (println "**setlist1! beginning list -> " ls " index " index " value " value)
    (let ((rec (lambda (break)      ;; this will be the continuation
                 (let f ((ls ls)
                         (i 0))
                   ;(println "**setlist1! list -> " ls " index " i)
                   (cond ((null? ls)
                          #t)
                         ((= index 0)
                          (set! ls (cons 'x ls))  ;; dummy code, just for index=0
                          (set-car! (cdr ls) value)   ;; change value of element of list
                         ; (println "**setlist1!, CHANGE (cdr ls) " (cdr ls) " new value " value " ls " ls)
                          (break #t))   
                         ((= i index)
                          (println "**setlist1!, before CHANGE (cdr ls) " (cdr ls) " new value " value)
                          (set-car! (cdr ls) value)   ;; change value of element of list
                         ; (println "**setlist1!, after CHANGE (cdr ls) " (cdr ls) " new value " value " ls " ls)
                          (break #t))    ;; break out of loop
                         (else
                          (f (cdr ls) (+ 1 i)))) ))))  ;; matches receiver
      (call/cc rec)) ))

;;(setlist (vector-ref *wpieces* 0) 2 'R) ;; vector index, list index (starting at 0) change value
;(setlist (vector-ref *wpieces* 12) 0 'e2)
;(display-wpieces1)

;; set new value/piece from oldsq(from) newsq(tosq) ex: value=7 piece=R
(define (setboard! from tosq)
  (println "setboard! from -> " from " to -> " to)
  
  (setlist!(vector-ref *board* tosq)  2 (readlist(vector-ref *board* from) 3)) ; new 'value
  (setlist!(vector-ref *board* from) 2 0)                                      ; old value set to '0
  (setlist!(vector-ref *board* tosq) 3 (readlist(vector-ref *board* from) 4))  ; new 'piece tosq 'piece        
  (setlist!(vector-ref *board* from) 3 '*))                                 ; old piece tosq set to '*

; use this procedure to see if piece in the 'move list
; ex: spring-in-list "K" "movels* 0 1
(define string-in-list?
  (lambda (str ls start end)
    (println "string-in-list? str " str " ls " ls " start " start " end " end)
    (let loop ((ls ls))
      (cond ((null? ls)
             #f)
            ((string=? (substring(list->string(car ls)) start end) str)
             (println "found match (car ls) " (car ls) " " (substring(list->string(car ls)) start end) " with: " str)
             #t)
            (else
;             (println "(car ls) " (car ls) " list-> " ls)
             (loop (cdr ls)))))))

;;(string-in-list? "K" *movels* 0 1)

;; pieces-state *wpieces* 7 6 0 - check state of vector, 0 has not moved
(define piece-state
  (lambda (vec index field value)
    (cond ((= (list-ref(vector-ref vec index) field) value)
                          (break #t))
                         (else
                          #f))))

;;(piece-state *wpieces* 7 6 0)

(define index-of-algebra-piece
  (lambda (vec algebra cnt piece)
    (println "index-of-algebra-piece, working on.. " algebra " piece " piece)
    (let loop ((i 0))
      ;(println "index-of-algebra-piece curr-index -> " i)
      (cond ((> i cnt)
             (list #f))
            ((and (equal? (list-ref(vector-ref vec i)0) algebra) (= (list-ref(vector-ref vec i) 5) piece))
             (list #t i))
            (else
             (loop (+ i 1)))))))

;; similiar to above which looks at algebra a1 this one instead will look at: 'a for ex: Rae1
(define index-of-algebra-piece-1
  (lambda (vec algebra cnt piece)
    ;(println "index-of-algebra-piece-1, working on.. " algebra " piece " piece " cnt " cnt)
    (let loop ((i 0))
     ; (println "index-of-algebra-piece curr-index -> " i)
      (cond ((> i cnt)
             (list #f))
            ((and (equal? (substring(symbol->string(list-ref(vector-ref vec i)0)) 0 1) (symbol->string algebra)) ; first letter
                  (= (list-ref(vector-ref vec i) 5) piece))
             (println "index-of-algebra-piece-1 Match!! " (symbol->string algebra))
             (list #t i))
            (else
              (println "algebra-> " (substring(symbol->string(list-ref(vector-ref vec i)0)) 0 1))
             (loop (+ i 1)))))))

(define index-of-piece
  (lambda (vec cnt piece)
    (println "index-of-piece, working on.. piece " piece)
    (let ((ls '()))
     (let loop ((i 0))
       (println "index-of-algebra-piece curr-index -> " i)
       (cond ((> i cnt)
              ls)
             ((= (list-ref(vector-ref vec i) 5) piece)
              (set! ls (cons i ls))
              (println ls)
              (loop (+ i 1)))
             (else
              (loop (+ i 1)))))) ))

;;(index-of-piece *wpieces* 15 6)


;(readlist (vector-ref *bpieces* 4) 1)

;(index-of-algebra-piece *wpieces* 'e3 15 1)
;(list? (cdr(index-of-algebra-piece *wpieces* 'c3 15 2)))
;(list-ref(vector-ref *wpieces* 0)0) ; algebra
;(list-ref(vector-ref *wpieces* 0)5) ; intpiece

; procdure to get index of piece vectors
; if found return #t,index
; else return #f,index
; vectors: *wpieces*/*bpieces* ex: *wpieces* 4 24 (for sq 24)
(define get-vec-index
  (lambda (vec field value)
    (let ((rec (lambda (break)  
                 (let f(( i 0))
                   (println "index -> " i)
                   (cond ((> i 15)
                          (break -1))
                         ((= (list-ref(vector-ref vec i) field) value)
                          (break i))
                         (else
                          (f (+ i 1))))) )))
      (call/cc rec)) ))

;(let (( r1(get-vec-index *wpieces* 4 34)))
;  (println " is r1 a number? " (number? r1))
;  (println "return from get-vec-index -> " r1))

;; return int square of string sq of chessboard - via hashtable
(define get-square
  (lambda (algebra)
    (table-ref *htsqs* algebra)))

;;(get-square "f3")

;; return algebra of board sq via hashtable
(define get-algebra
  (lambda (sq)
    (table-ref *htalgebra* sq)))

;;(get-algebra '63)

;; check if piece capturing same color piece as tosq
(define same-occupy?
  (lambda (turn tosq)
    (println "same-occupy? turn -> " turn " tosq " tosq)
    (cond ((= turn 1)
           (if (char-upper-case? (string-ref (symbol->string(readlist(vector-ref *board* tosq) 4)) 0))
               ;#t
               (list #t tosq)
               #f))
          ((= turn 0)
           (if (char-lower-case? (string-ref (symbol->string(readlist(vector-ref *board* tosq) 4)) 0))
               (list #t tosq)
               #f)))))

(same-occupy? 0 83)
;(same-occupy? 1 54)
;(same-occupy? 0 23)


; Note: Gambit-C has no 'string-upcase 
(define (lower-to-upper s1)
  (println "lower-to-upper s1 -> " s1)
  
  (cond ((equal? (substring s1 0 1) "r")
         "R")
        ((equal? (substring s1 0 1) "n")
         "N")
        ((equal? (substring s1 0 1) "b")
         "B")
        ((equal? (substring s1 0 1) "q")
         "Q")
        ((equal? (substring s1 0 1) "k")
         "K")
        (else
         (println "lower-to-upper, illegal substring on 0,1 !")
         #f)))
        
  

;; piece cannot capture an oppenents piece if making non-capture move
;; use this procedure on a non-capture move
(define opponent-occupy?
  (lambda (turn tosq)
    (cond ((= turn 1) ; Whites' move, check if black on tosq
           (if (char-lower-case? (string-ref (symbol->string(readlist(vector-ref *board* tosq) 4)) 0))
               #t
               #f))
          ((= turn 0) ; Black's move check if white on tosq
           (if (char-upper-case? (string-ref (symbol->string(readlist(vector-ref *board* tosq) 4)) 0))
               #t
               #f)))))
           
;;(opponent-occupy? 0 34)

;; checks to determine if intervining sqs. from -> tosq are occupied
;; NOTE!! if opponent-occupy list is length = 1 and tosq member of that list - Exception!
;;        then return #f#f as piece move will check if tosq is occuppied
;;        if it is just a standard piece move then sqs. cannot be occupied
;;        by oppoenents pieces. if capture move then tosq needs an opponents piece on tosq.
;;        returning #f#f meanns no same-occupy piece or opponents-occupy piece on sqs
(define check-sqs-occupied?
  (lambda (turn ls tosq)
      (let f ((ls ls) (oppls '()) (same-occupy #f) (opponent-occupy #f) (samels '()) )
        (println "check-sqs-occupied? list ls -> " ls " tosq " tosq " oppls " oppls " samels " samels)
        
        (cond (( equal? #t (car ls))  ;; #t marks the end of the list...
               (println "** check-sqs-occupied? return with -> "
                        (list same-occupy opponent-occupy oppls samels)
                        " oppls -> " oppls)
               (println " ** check-sqs-occupied? tosq member of opponent-occupy list? -> "
                        (member tosq oppls)
                        " length of oppls -> " (length oppls))

               (if (and (member tosq oppls) (= (length oppls) 1)) ;; just one occupied by 'tosq??
                   (list same-occupy #f)
                   (list same-occupy opponent-occupy oppls samels)))
              (else
               (cond ((same-occupy? turn (car ls))
                      (println "check-sqs-occupied: proc: same-occupy? = #t sq -> " (car ls))
                      (f (cdr ls) oppls #t opponent-occupy (append(list(car ls))samels) ) )
                     
                     ((opponent-occupy? turn (car ls))
                      (println "check-sqs-occupied: proc: opponent-occupy? = #t sq -> " (car ls))
                      (f (cdr ls) (append(list(car ls))oppls) same-occupy #t samels ))
                     
                     (else
                      (f (cdr ls) oppls same-occupy opponent-occupy samels) ))) ))))

;;(check-sqs-occupied? 1 (list  82 72 62 52 #t) 82)

; return #t if board piece value > 0
(define sq-nonzero-value?
  (lambda (tosq)
    (if (> (readlist(vector-ref *board* tosq) 3) 0)
        #t
        #f)))

;(sq-nonzero-value? 85) 


;; check if any false values (#f) in parsing pieces
;; return #t if move looks legal ie no false values
;; otherwie return #f as move contains #f values, illegal
(define check-move-legal?
  (lambda (ls)
    (call/cc (lambda (break)
               (let f ((ls ls))
                 ;(println "check-move-legal? the current list -> " ls)
                 (cond ((null? ls)
                        (list #t))
                       ((equal? #f (car ls))
                        (break (list #f (car ls))))
                       (else
                        (f (cdr ls)))) )))))

; return #t if piece can capture opponents piece
(define piece-capture?
  (lambda (turn tosq)
     (println "piece-capture? turn -> " turn " to -> " tosq)
    (cond ((= turn 1)
           (cond ((and (sq-nonzero-value? tosq) (opponent-occupy? turn tosq))
                  #t)
                 (else
                  #f)))
    ((= turn 0)
     (cond ((and (sq-nonzero-value? tosq) (opponent-occupy? turn tosq))
            #t)
          (else
           #f)))
     (else
      #f))))

;(piece-capture? 1 55) 

 ;; check if piece putting opponents king in check at tosq
;; #t if king in check otherwise #f
(define king-checked?
  (lambda (turn tosq)
    (cond ((= turn 1)
           (if (char=? (string-ref(symbol->string(readlist(vector-ref *board* tosq) 4))0) #\k)
               #t
               #f))
          ((= turn 0)
           (if (char=? (string-ref(symbol->string(readlist(vector-ref *board* tosq) 4))0) #\K)
               #t
               #f)))))

;(king-checked? 1 25)


;; get piece that we want to move, piece with value > 0
;; return either #f or #t, index, piece, intsq
; get a piece that equals input 'piece and state of piece < 9 (9=captured)
(define getpiece
  (lambda (turn piece)
    (println "getpiece: turn -> " turn " piece -> " piece)
    (let ((ret (list #f)))
      (lambda (index)  ;; index will be free variable
        (println "getpiece: current index -> " index)
        (cond ((= turn 1)
               (let f (( i index))
                 (cond (( < i 16)
                       ;; (println "White loop i -> " i " sq -> " (readlist(vector-ref *wpieces* i)1))
                        (cond  ((and (= piece (readlist (vector-ref *wpieces* i) 6)) ;; piece(i) value > 0?
                                     (< (readlist (vector-ref *wpieces* i) 7) 9))    ; state
                                ;(println "getpiece: White, found value > 0, index -> " i " piece -> "
                                ;         (readlist(vector-ref *wpieces* i) 3)
                                ;         " sq -> " (readlist(vector-ref *wpieces* i)5))
                                (set! ret (list #t i piece (readlist(vector-ref *wpieces* i) 5)))   
                                (f(+ i 16)))     ;; end the loop!
                                (else
                                 (f(+ i 1)))) )) ;; loop
                 (car(list ret)) )) ;; return #f - piece not found
                                              
            ((= turn 0)
             (println "getpiece: Black index -> " index)
             (let f (( i index))
               (cond (( < i 16)
                      ;;(println "Black loop i -> " i " sq -> " (readlist(vector-ref *bpieces* i)1))
                      (cond ((and (= piece (readlist (vector-ref *bpieces* i) 6)) ;intpiece
                                  (< (readlist (vector-ref *bpieces* i) 7) 9))    ; state
                             ;(println "getpiece: Black, found value > 0 i -> " i " piece -> "
                             ;         (readlist(vector-ref *bpieces* i) 3)
                             ;         " sq -> " (readlist(vector-ref *bpieces* i)5))
                             
                             (set! ret (list #t i piece (readlist(vector-ref *bpieces* i) 5)))
                             (f(+ i 16)))
                            (else
                             (f(+ i 1)))) ))
               (car(list ret)) )) )))))  ;return #f - piece not found

;;(define func (getpiece 1 4))
;;(func 0)

(define (trypiece turn piece)           
  (let ((f (getpiece turn piece)))
    f))

;(let (( f (trypiece 1 4)))
;(f 1))


;; a piece procedure ex: 'pawn calls 'do-trypiece. proc is specific procedure from ex: 'pawn
;; ls = the list of piece indices to try ex: Rook is 0,7
;; f returns either #f or #t,index,piece,sq
;; if successful ret: #t,index,piece,from,tosq else #f
; use this procedure for 'Pawns!!
(define (do-trypiece-pawn turn piece tosq ls proc)
  (println "*do-trypiece-pawn: turn: " turn " piece " piece " tosq " tosq " proc " proc)
  (call/cc (lambda (break)
             (let ((f (trypiece turn piece))) ;; return procedure 'getpiece
               (println "** do-trypiece-pawn: return from trypiece f -> " f)
               (let loop ((ls ls))
                 (cond ((null? ls)
                        (list #f))
                       (else
                        ;(println "**do-trypiece-pawn: working on -> " (car ls))
                        (let (( r1 (f (car ls))))   ;; call 'getpiece with a piece index
                          (println "***do-trypiece-pawn: ret -> " r1) 
                          (if (proc turn (list-ref r1 3) tosq)  ;; procedure for each piece ex: pawn-move?
                              (break (append r1 (list tosq))))
                          (loop (cdr ls)))) ))))) )

;; for Rook, bishop and Queen - need to check for sqs 'from -> 'tosq, 'check-sqs-occupied
;; in general r3 returning from check-sqs-occupied #f#f shows that the piece wanting to
;; move to 'tosq is not blocked by same color or opponent color piece
;; check-sqs-occuppied will return #f#f meaining no sqs. occupied - passed test
;; so return #t
(define (do-trypiece turn piece tosq ls proc)
  (println "@@@@@@ *** do-trypiece: turn: " turn " piece " piece " tosq " tosq " ls " ls " proc " proc)
  (call/cc (lambda (break)
             (let ((f (trypiece turn piece)) ;; return procedure 'getpiece
                   (squares '()))
               (println "** do-trypiece: return from trypiece f -> " f)
               (let loop ((ls ls))
                 (cond ((null? ls)
                        (println "** do-trypiece: end of list returning (list #f) squares-> " squares)
                        (append (list #f) squares))
                        ;(list #f))
                       (else
                        (println "**do-trypiece: working on -> " (car ls))
                        (let (( r1 (f (car ls))))   ;; call 'getpiece with a piece index !!
                          (println "MPW do-trypiece: r1 -> " r1)
                          (let ((r2 (proc turn (list-ref r1 3) tosq)))
                            ;(println "MPW do-trypiece: return from proc r2 -> " r2)
                            (cond ((car r2) ; possible? to move but other pieces might be in the way
                                   (let ((r3 (check-sqs-occupied? turn (cdr(reverse r2))tosq))) ;#f#f passed test!!
                                     (println "MPW do-trypiece: return *Squares* check-sqs-occupied? r3 -> " r3)
                                     (set! squares r3)
                                     ;(println "MPW do-trypiece: car r3 " (car r3) " cdr r3 " (cdr r3))
                                     (cond ((and (equal? #f (car r3)) (equal? #f (cadr r3))) ; OK! not occupied..
                                            (println "MPW do-trypiece: call break! r1 -> " r1 " (list tosq) " (list tosq))
                                            (break (append r1 (list tosq )))
                                            (println "MPW should not be here do-trypiece !!!!"))) )))))
                        (loop (cdr ls)) )))))))

; arg= piece vector and index. ex: *wpieces 1
(define cache-piece-vec-data
  (lambda()
    (lambda (vec index)
      (println "cache-piece-vec-data  index-> " index)
      (let ((r1 (readlist(vector-ref vec index) 1))  ;algebra
            (r2 (readlist(vector-ref vec index) 5))  ;sq
            (r3 (readlist(vector-ref vec index) 7))) ;state
        (println "cache-piece-vec-data.. saving.. piece at index: " index " algebra " r1 " sq " r2 " state " r3)
        (list index r1 r2 r3) ))))

(define cache-board-vec-data
  (lambda()
    (lambda ( from to)
      (println "cache-board-vec-data  from: " from " to " to)
      (let ((r1 (readlist(vector-ref *board* from) 3))  ; value
            (r2 (readlist(vector-ref *board* from) 4)))  ; piece
        (println "cache-board-vec-data..saving..  from " from " to " to " value " r1 " piece " r2)
        (list from to r1 r2) ))))

;(define x (cache-piece-vec-data))
;(define y (cache-piece-vec-data))
;(x *wpieces* 1)
;(y *bpieces* 1)

;(define x1 (cache-board-vec-data))
;(x1 62 74)

; return white or black piece vector depending on 'turn
(define choose-piece-vector
  (lambda()
    (lambda (turn)
      (if (= turn 1)
          *wpieces*
          *bpieces*))))

;(define v (choose-piece-vector))
;(v 1)
;(v 0)

;; if turn = 1 then return black vector
;; else return white vector
(define choose-opposite-vector
  (lambda()
    (lambda (turn)
      (if (= turn 1)
          *bpieces*
          *wpieces*))))

;(define v1 (choose-opposite-vector))
;(v1 1)


  