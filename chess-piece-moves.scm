
(define move-piece
  (lambda (turn index algebra from tosq move)

    (println "move-piece -> turn " turn " index " index " algebra " algebra
             " from " from " to " tosq " move " move)

    (setlist!(vector-ref *board* tosq) 2 (readlist(vector-ref *board* from) 3)) ; new 'value
    (setlist!(vector-ref *board* from) 2 0)                                     ; old value set to '0
    (setlist!(vector-ref *board* tosq) 3 (readlist(vector-ref *board* from) 4)) ; new 'piece tosq 'piece        
    (setlist!(vector-ref *board* from) 3 '*)                                    ; old piece tosq set to '*

    (let ((v1 (choose-piece-vector))) ; v1 will be either *wpieces* or *pieces* vector depending on turn
           (setlist!(vector-ref (v1 turn) index) 4 tosq)      ; sq
           (setlist1!(vector-ref (v1 turn) index) 0 algebra)  ; algebra Zero index
           (setlist!(vector-ref (v1 turn) index) 6 1)         ; state = 1, moved
           (println "White ***************")
           (display-wpieces1)
           (println "Black ***************")
           (display-bpieces1))

    (set! *movels* (cons(string->list move) *movels*))
    (pp-movels (reverse *movels*))
    ;(println "\n moves-> [" (reverse *movels*) "]") 
    
    (db) ; board piece
    ;(db-values) ; board values
    #t))

;; pawn queens move....
(define move-queening-piece
  (lambda (turn index algebra from tosq move)

    (println "move-queening-piece -> turn " turn " index " index " algebra " algebra
             " from " from " to " tosq " move " move)

    ;;(setlist!(vector-ref *board* tosq) 2 (readlist(vector-ref *board* from) 3)) ; new 'value
    (setlist!(vector-ref *board* from) 2 0)                                     ; old value set to '0
    (setlist!(vector-ref *board* tosq) 3 (readlist(vector-ref *board* from) 4)) ; new 'piece tosq 'piece        
    (setlist!(vector-ref *board* from) 3 '*)                                    ; old piece tosq set to '*
    (setlist!(vector-ref *board* tosq) 2 9)                                     ; new value to 9

    (let ((v1 (choose-piece-vector))) ; v1 will be either *wpieces* or *pieces* vector depending on turn
           (setlist!(vector-ref (v1 turn) index) 4 tosq)      ; sq
           (setlist1!(vector-ref (v1 turn) index) 0 algebra)  ; algebra Zero index
           
           (setlist!(vector-ref (v1 turn) index) 1 9)         ; now pawn has queen value
           (setlist!(vector-ref (v1 turn) index) 5 5)         ; now pawn is a queen

           ;; change piece to either 'Q or 'q and board to either 'Q or 'q
           ;; depends if White or Black turn...
           (cond ((= (readlist(vector-ref (v1 turn) index) 4) 1)
                  (setlist!(vector-ref (v1 turn) index) 2 'Q)
                  (setlist!(vector-ref *board* tosq) 3 'Q))
                 (else
                  (setlist!(vector-ref (v1 turn) index) 2 'q)
                  (setlist!(vector-ref *board* tosq) 3 'q)))
           
           (setlist!(vector-ref (v1 turn) index) 6 1)         ; state = 1, moved
           (println "White ***************")
           (display-wpieces1)
           (println "Black ***************")
           (display-bpieces1))

    (set! *movels* (cons(string->list move) *movels*))
    (pp-movels (reverse *movels*))
    ;(println "\n moves-> [" (reverse *movels*) "]") 
    
    (db) ; board piece
    ;(db-values) ; board values
    #t))

;set state=9 -> captured
; choose-piece-vector will return white or black piece vector
; depending on turn
; choose-opposite-vector will return white if turn = 0(black)
;                             return black if turn = 1(white) 
(define move-captured-piece
  (lambda (turn index algebra from tosq move)
    (println "move-captured-piece -> turn " turn " index " index " algebra " algebra
             " from " from " to " tosq " move " move)
    
    (setlist!(vector-ref *board* tosq) 2 (readlist(vector-ref *board* from) 3)) ; new 'value
    (setlist!(vector-ref *board* from) 2 0)                  ; old value set to 0
    (setlist!(vector-ref *board* tosq) 3 (readlist(vector-ref *board* from) 4)) ; new 'piece to tosq        
    (setlist!(vector-ref *board* from) 3 '*)                ; old piece tosq set to '*

    (let ((v1 (choose-piece-vector))
          (v2 (choose-opposite-vector)))
      (setlist!(vector-ref (v1 turn) index) 4 tosq)       ; sq (= turn) - new
      (setlist1!(vector-ref (v1 turn) index) 0 algebra)   ; algebra Zero index (= turn) - new
      (setlist! (vector-ref (v1 turn) index) 6 1)         ; state = 1, moved (= turn) - new
      
      (let ((r1 (get-vec-index (v2 turn) 4 tosq)))        ; index of sq/tosq from vector (opposite of turn)
      ;;  (setlist!  (vector-ref (v1 turn) index) 6 1)      ; state = 1, moved (= turn) 
        (setlist1! (vector-ref (v2 turn) r1) 0 0)         ; algebra, (opposite of turn) - old
        (setlist!  (vector-ref (v2 turn) r1) 6 9)         ; state=captured(9) (opposite of turn) - old
        (setlist!  (vector-ref (v2 turn) r1) 4 0)))       ; sq, (opposite of turn)  - old
    
    
    (set! *movels* (cons(string->list move) *movels*))
    ;(println "\n moves-> [" (reverse *movels*) "]"
    (pp-movels (reverse *movels*))
    
    (println "*White Pieces*")
    (display-wpieces1)
    (println "*Black Pieces*")
    (display-bpieces1)
    (db) ; board piece
    (db-values) ; board values
    #t))

;; New procedure for enpassant capture
;; ptosq = is opponents pawn square +10 or -10, sq. below the pawn
;; tosq = is the pawn sq. which in a non-enpassant move would be the correct 'tosq
(define move-captured-piece-enpassant
  (lambda (turn index algebra from tosq ptosq move)
    (println "move-captured-piece-enpassant -> turn " turn " index " index " algebra " algebra
             " from " from " to " tosq  " ptosq " ptosq " move " move)

    (println "1")
    (setlist!(vector-ref *board* ptosq) 2 (readlist(vector-ref *board* from) 3)) ; new 'value
    (println "2")
    (setlist!(vector-ref *board* from) 2 0)       ; old value set to 0
    (println "3")
    
    (setlist!(vector-ref *board* ptosq) 3 (readlist(vector-ref *board* from) 4)) ; new 'piece to tosq
    (print "4")
    (setlist!(vector-ref *board* from) 3 '*)      ; old piece tosq set to '*
    (print "5")
    (setlist!(vector-ref *board* tosq) 3 '*)     ; pawn set to '*
    (print "6")
    (setlist!(vector-ref *board* tosq) 2 0)      ; pawn value set to 0
    (print "7")
    

    (let ((v1 (choose-piece-vector))
          (v2 (choose-opposite-vector)))
      (setlist!(vector-ref (v1 turn) index) 4 ptosq)       ; sq (= turn) new sq.
      (setlist1!(vector-ref (v1 turn) index) 0 algebra)   ; algebra Zero index (= turn) new algebra
      (setlist! (vector-ref (v1 turn) index) 6 1)         ; state = 1, moved (= turn) new state
      
      (let ((r1 (get-vec-index (v2 turn) 4 tosq)))       ; index of sq/ptosq from vector (opposite of turn)
        (setlist1! (vector-ref (v2 turn) r1) 0 0)         ; algebra, (opposite of turn)  , old
        (setlist!  (vector-ref (v2 turn) r1) 6 9)         ; state=captured(9) (opposite of turn) ,old
        (setlist!  (vector-ref (v2 turn) r1) 4 0)))       ; sq, (opposite of turn) ,old
    
    
    (set! *movels* (cons(string->list move) *movels*))
    ;(println "\n moves-> [" (reverse *movels*) "]")
    (pp-movels (reverse *movels*))
    
    (println "*White Pieces*")
    (display-wpieces1)
    (println "*Black Pieces*")
    (display-bpieces1)
    (db) ; board piece
    (db-values) ; board values
    #t))

;; New procedure for enpassant capture
;; ptosq = is opponents pawn sq that has become enpassant
;; by current turns pawn
(define move-captured-piece-enpassant-1
  (lambda (turn index algebra from tosq ptosq move)
    (println "move-captured-piece-enpassant -> turn " turn " index " index " algebra " algebra
             " from " from " to " tosq  " ptosq " ptosq " move " move)
    
    
    (set! *movels* (cons(string->list move) *movels*))
    ;(println "\n moves-> [" (reverse *movels*) "]")
    (pp-movels (reverse *movels*))
    
    (println "*White Pieces*")
    (display-wpieces1)
    (println "*Black Pieces*")
    (display-bpieces1)
    (db) ; board piece
    (db-values) ; board values
    #t))


;;(move-capture-piece 0 26)

; (move-castle turn 7 4 'h1 'e1 28 25 26 27 move))
(define move-castle
  (lambda (turn i1 i2 a1 a2 f1 f2 t1 t2  move)

    (println "move-castle  turn: " turn " i1 -> " i1 " i2 -> " i2 " a1 -> " a1 " a2 -> " a2
             " f1 -> " f1 " f2 -> " f2 " t1 -> " t1 " t2 -> " t2)
    
    (setlist!(vector-ref *board* t1) 2 (readlist(vector-ref *board* f1) 3)) ; new 'value - rook
    (setlist!(vector-ref *board* f1) 2 0)                  ; old value set to '0 - rook
    (setlist!(vector-ref *board* t1) 3 (readlist(vector-ref *board* f1) 4)) ; new 'piece tosq 'piece -rook     
    (setlist!(vector-ref *board* f1) 3 '*) ; old piece tosq set to '* - rook

    (setlist!(vector-ref *board* t2) 2 (readlist(vector-ref *board* f2) 3)) ; new 'value - king
    (setlist!(vector-ref *board* f2) 2 0)                  ; old value set to '0 - rook
    (setlist!(vector-ref *board* t2) 3 (readlist(vector-ref *board* f2) 4)) ; new 'piece tosq 'piece -king     
    (setlist!(vector-ref *board* f2) 3 '*) ; old piece tosq set to '* - king

    (let ((v1 (choose-piece-vector)))
           (setlist!(vector-ref  (v1 turn) i1) 4 t1)   ; sq
           (setlist1!(vector-ref (v1 turn) i1) 0 a1)   ; algebra Zero index
           (setlist!(vector-ref  (v1 turn) i1) 6 1)    ; state = 1, moved

           (setlist!(vector-ref  (v1 turn) i2) 4 t2)   ; sq
           (setlist1!(vector-ref (v1 turn) i2) 0 a2)   ; algebra Zero index
           (setlist!(vector-ref  (v1 turn) i2) 6 1))   ; state = 1, moved

    (println "White")
    (display-wpieces1)
    (println "Black")
    (display-bpieces1)
      
    (set! *movels* (cons(string->list move) *movels*))
    ;(println "\n moves-> [" (reverse *movels*) "]")
    (pp-movels (reverse *movels*))
    
    (db) ; board piece
    (db-values) ; board values
    #t))


;ex: (piececc "Ne3" , O=castle
(define piececc
  (lambda (move)
    (let ((ls (list #\R #\N #\B #\Q #\K #\O)))
      (let ((receiver (lambda (break)
                        (let f ((ls ls))
                          (cond ((null? ls)
                                 (list #f))
                                ((char=? (string-ref move 0) (car ls))
                                 (break (list #t (car ls) (string-length move))))
                                (else
                                 (f (cdr ls)))) )))) ;; matches receiver
        (call/cc receiver)) )))

;; Note: need to check the to part ex: Rxv5 is invalid as there is no 'v sq.
;; make sure the 'to part is valid.... via pawncc, because the 'to square might be invalid.
(define parse-piece
  (lambda (move)
    (println "parse-piece move-> " move)
    (println "parse-piece move length -> " (string-length move))
    
    (let ((ls (list #\x #\+))
          (p (piececc move)))
      (println "parse-piece return from 'piececc p-> " p)
      (let ((recc (lambda (break)
                    (cond ((not (car p))  ; did not find a piece
                           ;;(println "parse-piece, Error, did not find a piece")
                           (break (list #f)))
                              
                          ;((and (= (caddr p) 3))   ; ex: Nf3
                          ; (println "ok, parse-piece: move -> " move " to -> " (substring move 1 3) " " p)
                          ; (break (list #t (string-length move) (cadr p) (string->symbol (substring move 1 3))
                          ;              (car(pawncc(substring move 1 2))) )))

                          ((and (= (caddr p) 3) (equal? (cadr p) #\O)) ; Castle move...
                           (println "ok, parse-piece: move -> Castle King side move!! " move " to -> " (substring move 1 3) " " p)
                           (break (list #t (string-length move) (cadr p) (string->symbol (substring move 1 3)) #t)))
                          
                           ((= (caddr p) 3)   ; ex: Nf3
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 1 3) " " p)
                           (break (list #t (string-length move) (cadr p) (string->symbol (substring move 1 3))
                                        (car(pawncc(substring move 1 2))) )))

                          ((and (= (caddr p) 4) (char=? (string-ref move 1) (car ls))) ; ex: Nxf3
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 2 4) " " p)
                           (break (list #t (string-length move) (cadr p) #\x (string->symbol (substring move 2 4))
                                        (car(pawncc(substring move 2 3))) )))
                          
                          ((and (= (caddr p) 4) (char=? (string-ref move 3) (cadr ls)))  ; ex: Ne2+
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 1 3) " " p)
                           (break (list #t (string-length move) (cadr p) #\+ (string->symbol (substring move 1 3))
                                        (car(pawncc(substring move 1 2))) )))

                          ;; ex: Rae1 NEW !! ok...
                          ((= (caddr p) 4)  ; ex: Rae1
                           (println "ok, ex: Rae1  parse-piece: move -> " move " to -> " (substring move 2 4) " " p)
                           (break (list #t (string-length move) (cadr p) #\* (string->symbol (substring move 1 2))
                                        (string->symbol (substring move 2 4))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 2 3))) )))

                          
                          ((and (= (caddr p) 5) (char=? (string-ref move 1) (car ls))
                                (char=? (string-ref move 4) (cadr ls))) ;; Nxe2+
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 2 4) " " p)
                           (break (list #t (string-length move) (cadr p) #\x #\+ #\*
                                        (string->symbol (substring move 2 4))
                                        (car(pawncc(substring move 2 3))) )))                         
                          
                           ;;;;;;;;;;; caddr p = length of move
                          ((and (= (caddr p) 5) (equal? (cadr p) #\O)) ; Castle Queen side, move...
                           (println "ok, parse-piece: move -> Castle Queen move!! " move " to -> " (substring move 1 3) " " p)
                           (break (list #t 3 (cadr p) (string->symbol (substring move 1 3)) #t)))

                          ;; ex: Rae1+  NEW!! ok..
                          ((and (= (caddr p) 5) (char=? (string-ref move 4) (cadr ls))) 
                           (println "ok, ex: Rae1+  parse-piece: move -> " move " to -> " (substring move 2 4)" " p)
                           (break (list #t (string-length move) (cadr p) #\*  #\+ (string->symbol(substring move 1 2))
                                        (string->symbol(substring move 2 4))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 2 4))) )))

                          ;; ex: Raxe1 NEW !!
                          ((and (= (caddr p) 5) (char=? (string-ref move 2) (car ls))) 
                           (println "ok, ex: Raxe1 testing... parse-piece: move -> " move " to -> " (substring move 2 4)" " p)
                           (break (list #t (string-length move) (cadr p) #\x  #\* (string->symbol(substring move 1 2))
                                        (string->symbol(substring move 3 5))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 3 5))) )))

                          
                          ((and (= (caddr p) 5))                                               ; ex: Ne3f3
                           (println "ok, parse-piece: (len=5) move -> " move " to -> " (substring move 3 5)" " p)
                           (break (list #t (string-length move) (cadr p) #\* #\*(string->symbol(substring move 1 3))
                                        (string->symbol(substring move 3 5))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 3 4))) )))


                           ;; ex: Raxe1+ NEW
                          ((and (= (caddr p) 6) (char=? (string-ref move 2) (car ls)) (char=? (string-ref move 5) (cadr ls)))
                           (println "ok, parse-piece: Raxe1+ testing... move -> " move " to -> " (substring move 4 5) " " p)
                           (break (list #t(string-length move)(cadr p) #\x #\+ (string->symbol(substring move 1 2))
                                        (string->symbol(substring move 3 5))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 3 4))) )))

                          
                          ((and (= (caddr p) 6) (char=? (string-ref move 5) (cadr ls)))         ; ex: Ne3f3+
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 3 4) " " p)
                           (break (list #t(string-length move)(cadr p) #\* #\+ (string->symbol(substring move 1 3))
                                        (string->symbol(substring move 3 5))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 3 4))) )))
                          
                          ((and (= (caddr p) 6) (char=? (string-ref move 3) (car ls)))         ; ex: Ne3xf3
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 4 5) " " p)
                           (break (list #t(string-length move)(cadr p) #\x #\* (string->symbol(substring move 1 3))
                                        (string->symbol(substring move 4 6))
                                        (car(pawncc(substring move 1 2))) (car(pawncc(substring move 4 5))) )))

                          
                           ((and (= (caddr p) 7) (char=? (string-ref move 3) (car ls))
                                 (char=? (string-ref move 6) (cadr ls))) ; ex: Ne3xf3+ 
                           (println "ok, parse-piece: move -> " move " to -> " (substring move 3 4) " " p)
                           (break (list #t (string-length move) (cadr p) #\x #\+ (string->symbol(substring move 1 3))
                                        (string->symbol(substring move 4 6)) (car(pawncc(substring move 1 2)))
                                        (car(pawncc(substring move 4 6))) )))
                          ;;;;;;;;;
                          
                          (else
                           ;;(println "parse-piece, Error, found a piece but piece string is invalid")
                           (break (list #f)))) )))
        (call/cc recc)) )))

;(parse-piece "Raxe1")
;(parse-piece "Raxe1+")
;(parse-piece "Rae1")
;(parse-piece "Rae1+")
;(parse-piece "Rxf3")
;(parse-piece "Re3+")
;(parse-piece "Ra1a3")
;(parse-piece "Nf3d3")
;(parse-piece "Nf3xd3")
;(parse-piece "Rxe3")
;(parse-piece "Re3f3")


; proc: is piece procedure (ex: knight) with: move,turn,tosq
;; Need to add new moves: ex: Rae1 Rae1+
(define process-piece
  (lambda (turn move)
    (println "process-piece: turn -> " turn " move -> " move " " (list? move)(symbol? move)(string? move))
    (let ((piece (parse-piece move)))
      (println "*** process-piece *** returned from parse-piece, **piece list** -> " piece)
      ;; (println "*** process-piece: does move look legal? -> " (check-move-legal? piece))
      ;; find proc in hashtable, proc 1st char of move ie: R,N,B,Q,K
      ;; (cond ((car piece)
        (cond ((car (check-move-legal? piece))     
            (let ((proc (table-ref *ht* (string->symbol(substring move 0 1)) )))
              (println " process-piece: returned from check-move-legal? , found procedure -> " proc)
              ;; len=3 'to #t
              (cond  (( and (= 3 (list-ref piece 1)) (list-ref piece 4) (char=? #\O (list-ref piece 2))) ; O-O CASTLE !!
                     (println "standard major piece Castle move -> " piece)
                     (cond ((proc move turn "99" 1 piece)
                            (println "process-piece: OK,Castle len=3 move,returned #t from proc: " proc)
                            (list #t #\c))    
                           (else
                            (list #f #\c))))

                    (( and (= 3 (list-ref piece 1)) (list-ref piece 4))   ;; NEW!!
                     (println "standard major piece (Xf3) move -> " piece)
                     (let ((r1 (proc move turn (get-square(list-ref piece 3)) 1 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK, move, (Xf3) len=3 type=1 proc: " proc " r1 " r1)
                             (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                   (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;            (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              ;(list #f)
                              (list #f)))))

                    ;; len=4 'x to #t  Note: need to add case: Rae1
                    (( and  (= 4 (list-ref piece 1)) (list-ref piece 5))
                     (println "*** process-piece len=4 list-ref piece 5 " (list-ref piece 5))
                     (cond ((char=? (list-ref piece 3) '#\x)
                           (println "len 4  major piece (Xxf3) capture  -> " piece)
                           (let ((r1 (proc move turn (get-square(list-ref piece 4))2 piece)))
                             (cond ((car r1)
                                    (println "process-piece: OK, move, (Xf3) len=3 type=1 proc: " proc " r1 " r1)
                                    (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                          (list-ref r1 4) (list-ref r1 5) move))
                                    ;(move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    ;            (list-ref r1 4) (list-ref r1 5) move)
                                    ;#t)
                                   (else
                                    r1 )))) ; Testing
                                    ;;(list #f) ))))

                           ((char=? (list-ref piece 3) '#\+)
                            (println "len 4  major piece (Nf3+) check -> " piece)
                            (let ((r1 (proc move turn (get-square(list-ref piece 4))3 piece)))
                              (cond ((car r1)
                                     (println "process-piece: OK,(Xf3+) len=4,returned #t from proc: " proc " r1 " r1)
                                     (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                           (list-ref r1 4) (list-ref r1 5) move))
                                     ;(move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                     ;            (list-ref r1 4) (list-ref r1 5) move)
                                     ;#t)
                                    (else
                                     (list #f)))))
                           
                           ((char=? (list-ref piece 3) '#\*)  ;; Note: 9 is a new type!! - (list-ref piece 5 -> tosq) based on algebra 'e1
                            (println "len 4  major piece (Rae1) -> " piece)
                            (let ((r1 (proc move turn (get-square(list-ref piece 5))9 piece)))
                              (cond ((car r1)
                                     (println "process-piece: OK,(Xf3+) len=4,returned #t from proc: " proc " r1 " r1)
                                     (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                           (list-ref r1 4) (list-ref r1 5) move))
                                     ;(move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                     ;            (list-ref r1 4) (list-ref r1 5) move)
                                     ;#t)
                                    (else
                                     (list #f))))) ))
                            
                    (( and (= 5 (list-ref piece 1)) (list-ref piece 7) (list-ref piece 8)  ;; NEW!! Raxe1
                           (char=? (list-ref piece 3) '#\x))
                     (println "len 5  major piece testing.... (Raxe1)  -> " piece)
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))11 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Raxe1) len=5,returned #t from proc: " proc " r1 " r1)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;                   (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                    
                    (( and (= 5 (list-ref piece 1)) (list-ref piece 7)
                           (char=? (list-ref piece 3) '#\x))
                     (println "len 5  major piece (Xxf3+)  -> " piece)
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))4 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Xxf3+) len=5,returned #t from proc: " proc " r1 " r1)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                             ; (move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                             ;                    (list-ref r1 4) (list-ref r1 5) move)
                             ; #t)
                             (else
                              (list #f)))))

                    ; ex: Rae1+ New!!
                    (( and (= 5 (list-ref piece 1)) (list-ref piece 7) (list-ref piece 8) (char=? (list-ref piece 4) '#\+))
                     (println "len 5  major piece (Rae1+) NEW!  -> " piece)   ;; (list-ref piece 6 -> tosq) based on algebra 'e1
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))10 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Rae1+) len=5,returned #t from proc: " proc " r1 " r1)
                              (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;                   (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                    
                    ;;; some special cases: Ne3f3, Ne3f3+ Ne3xf3 Ne3xf3+, len=5 'from #t to #t
                    (( and (= 5 (list-ref piece 1)) (list-ref piece 7) (list-ref piece 8))
                     (println "len 5  major piece (Xe3f3)  -> " piece)
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))5 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Xe3f3) len=5,returned #t from proc: " proc " r1 " r1)
                              (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;                   (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))

                    ;; New !!! Raxe1+
                    (( and (= 6 (list-ref piece 1)) (char=? (list-ref piece 3) '#\x) (char=? (list-ref piece 4) '#\+)
                           (list-ref piece 7) (list-ref piece 8))
                     (println "len 6  major piece testing ... (Raxe1+)  -> " piece) 
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))12 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Raxe1+) len=6,returned #t from proc: " proc)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))    
                              ;(move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;                     (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                    
                    (( and (= 6 (list-ref piece 1)) (char=? (list-ref piece 3) '#\x)
                           (list-ref piece 7) (list-ref piece 8))
                     (println "len 6  major piece (Xe3xf3)  -> " piece) 
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))6 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Xe3xf3) len=6,returned #t from proc: " proc)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;                     (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                        
                    (( and (= 6 (list-ref piece 1)) (char=? (list-ref piece 4) '#\+) (list-ref piece 7)
                           (list-ref piece 8)) ;len 6 'from #t 'to #t  MPW
                     (println "len 6  major piece (Xe3f3+)  -> " piece)
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))7 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Xe3f3+) len=6,returned #t from proc: " proc)
                              (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;            (list-ref r1 4) (list-ref r1 5) move)      
                              ;#t)
                             (else
                              (list #f)))))

                    ;; len=7 'from #t 'to #t
                    (( and (= 7 (list-ref piece 1)) (char=? (list-ref piece 3) '#\x) (char=? (list-ref piece 4) '#\+)
                           (list-ref piece 7) (list-ref piece 8))
                     (println "len 7  major piece (Xe3xf3+)  -> " piece)
                     (let ((r1 (proc move turn (get-square(list-ref piece 6))8 piece)))
                       (cond ((car r1)
                              (println "process-piece: OK,(Xe3xf3+) len=7,returned #t from proc: " proc)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                              ;                     (list-ref r1 4) (list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                    (else
                     (println "process-piece *illegal (inner) piece move ! " move)
                     (list #f))) ))
            (else
             (println "process-piece: **illegal (outer) piece move !!")
             (list #f))))))

;(let (( r1 (process-piece 1 "Qd3")))
;  (println "r1 -> " r1 " length r1 " (length r1))
;  (apply (list-ref r1 1) (list (list-ref r1 2) (list-ref r1 3) (list-ref r1 4)(list-ref r1 5)
;            (list-ref r1 6) (list-ref r1 7))))


;(let* ((r1 (process-piece 1 "Bc1")))
;  (cmd-apply-move r1))

;(process-piece 1 "Qd8d6")
;(process-piece 1 "Qd1d2")
;(process-piece 1 "Nb1c3")
;(process-piece 1 "Rae1")
;(process-piece 1 "Rad3+")
;(process-piece 1 "Ra1e1+")
;(process-piece 1 "Qexf7")
;(process-piece 0 "Kexf7")
;(process-piece 1 "Qgxe6")
;(process-piece  1 "Raxe1")
;(process-piece 1  "Raxe1+")
;(process-piece 1 "Qgxg6+")

(define parse-castle-move
  (lambda (move)
    (cond ((string=? move "O-O")
           (list #t "q-castle"))
          ((string=? move "O-O-O")
           (list #t "k-castle"))
          ((string=? move "O-O+")
           (list #t "q-castle+"))
          ((string=? move "O-O-O+")
           (list #t "k-castle+"))
          (else
           #f))))

; used for: Rook, Knight, Bishop, Queen, King
; ls = list of indices of piece in piece vector ex: Knight [1 6]
; proc = procedure to execute of specific piece ex: Knight-move?
; piece = piece list, Note: 'move really not used, just showing move to make...
(define Piece
    (lambda (move turn tosq type piece intpiece ls proc)
      (println "** Piece proc: ** move: " move " turn: " turn " type: " type " piece: " piece
               " intpiece: " intpiece " ls: " ls " proc: " proc)

       (cond ((= type 1)
             (println "**Piece standard non-capture type=1 Xf3")         
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (cond ((and (not (opponent-occupy? turn tosq)) (car r1))
                      (println "**Piece: Ok, piece can move (r1) " r1)
                      (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4))(list-ref r1 3) (list-ref r1 4)))
                     (else
                      (list #f)))))
            
            ((= type 2)
             (println "**Piece capture move type=2 Xxf3")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (println "Piece: returned from do-trypiece, r1-> " r1)
               (cond ((and (opponent-occupy? turn tosq) (car r1))
                      (println "**Piece: Ok, type=2 can capture piece (r1) " r1)
                      (println "**Piece: WOULD capture like this: turn: " turn
                               " index " (list-ref r1 1)
                               " algebra-to "  (get-algebra(list-ref r1 4))
                               " from "        (list-ref r1 3)
                               " tosq "  (list-ref r1 4))
                      (list #t  turn (list-ref r1 1) (get-algebra(list-ref r1 4))(list-ref r1 3) (list-ref r1 4)))
                     (else
                      r1 ))))   ;; Testing...
                      ;(list #f)))))

            ; **1st determine if Knight can move to tosq then
            ;   check if new tosq can check king at his sq.
            ; Note: (vector-ref (v1 turn)4)4) is opponents King's sq (list-ref r1 1) Knight's index
            ; Note: need to temporarily change Knight's intsq to new proposed tosq ex: change 62 to 74
            ; do-trypiece: turn 2 (piece) tosq (list of knight index) procedure
            ; (setlist!(vector-ref (v1a turn) (list-ref r1 1) 4 tosq)) Temp tosq !!
            ; (setlist!(vector-ref (v1a turn) (list-ref r1 1) 4 (list-ref r1 3))) back to previous value !!
            ;(v2 (knight-move? turn tosq (list-ref(vector-ref (v1 turn) 4)4)))) ;2rd Knight-move? to King sq.
            
            ((= type 3) 
             (println "**Piece standard King+ move type=3 Xf3+")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (cond ((and (not (opponent-occupy? turn tosq)) (car r1)
                           (piece-check-move? turn (list-ref r1 1)(list-ref r1 3) tosq intpiece proc))
                      (println "Piece: OK, type=3 piece can check king!! (r1) " r1)  ; make the intended move!
                      (list #t turn (list-ref r1 1)(get-algebra(list-ref r1 4))(list-ref r1 3) (list-ref r1 4)))
                     (else
                     (list  #f)))))

            ; similliar to type=3 except piece takes opponents piece and checks King
            ((= type 4)
             (println "**Piece capture and King+ move type=4 Xxf3+")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (cond ((and (opponent-occupy? turn tosq) (car r1)
                           (piece-check-move? turn (list-ref r1 3) (list-ref r1 4) tosq intpiece proc))
                     (println "**Piece: Ok, type=4 can capture piece and king check (r1) " r1)
                     (list #t turn (list-ref r1 1)(get-algebra(list-ref r1 4))(list-ref r1 3) (list-ref r1 4)))
                     (else
                      (list #f)))))
            
            ;((= type 5)
            ; (println "**Piece specific piece/move (Nc3b5) (from-algebra) " (list-ref piece 5) " type " type)
            ; (cond ((piece-specific-non-capture-move turn (list-ref piece 5)  intpiece tosq type move proc)
             ;       (println "** Piece: OK, type=5 can make specific non-capture move")
              ;      #t)
               ;    (else
                ;    #f)))

            ((= type 5)
             (println "Piece:  specific non-capture move type=5 Xc3b5")   
             (let (( r1 (piece-specific-non-capture-move turn (list-ref piece 5) intpiece tosq proc)))
               (println "Piece: type=5 r1: " r1)
               (cond ((car r1)
                      (println "Piece: OK type=5 looks good, specific non-capture move r1: " r1)
                      (list #t (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)(list-ref r1 4)(list-ref r1 5)))
                     (else
                      (list #f)))))
            
            ((= type 6)   ;; will use 'piece-specific-capture-move 
             (println "Piece:  specific capture-move type=6 Xc3xb5")
             (let ((r1 (piece-specific-capture-move turn (list-ref piece 5) intpiece tosq proc)))
               (println "Piece type=6 r1: " r1)
               (cond ((car r1)
                      (print "*** Piece: OK, type=6 looks good,  specific capture move")
                      (list #t (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)(list-ref r1 4)(list-ref r1 5)))
                     (else
                      (list #f)))))

                         ;; (list-ref piece 5) is specific algebra of 'piece ex: 'a1
                         ;; looks like somewhat of a Hack... by calling do-trypiece then piece-specific-not-capture-move!!
            ((= type 7)  ;; pass: turn index from tosq piece proc and turn algebra piece tosq type move proc
             (println "**Piece specific non-capture-move+ type=7 Xe3f3+")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc))) ;; do any succeed?? via (car r1)
               (cond ((and (not (opponent-occupy? turn tosq)) (car r1) ;; no piece on tosq
                           (piece-check-move? turn (list-ref r1 1) (list-ref r1 3) tosq intpiece proc))
                      (println " type=7 piece-check-move? has passed test... now try to make the move")
                      (let (( r2 (piece-specific-non-capture-move turn (list-ref piece 5) intpiece tosq proc)))
                        (cond ((car r2)
                               (println "OK type=7 OK looks good (Xe3f3+) r2: " r2)
                               (list #t (list-ref r2 1) (list-ref r2 2) (list-ref r2 3)(list-ref r2 4)(list-ref r2 5)))
                              (else
                               (list #f)))))
                     (else
                      (list #f)))))
                        
            ((= type 8)    ;; ..................................................
             (println "**Piece specific capture-move+ type=8 Xe3xf3+")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (cond ((and (opponent-occupy? turn tosq) (car r1)
                           (piece-check-move? turn (list-ref r1 1) (list-ref r1 3) tosq intpiece proc))
                      (println " type=8 piece-check-move? has passed test... now try to make the move")
                      (let ((r2 (piece-specific-capture-move turn (list-ref piece 5) intpiece tosq proc)))
                        (cond ((car r2)
                               (println "OK, type=8 looks good (Xe3xf3+) r2: " r2)
                               (list #t (list-ref r2 1) (list-ref r2 2) (list-ref r2 3)(list-ref r2 4)(list-ref r2 5)))
                              (else
                               (list #f)))))
                     (else
                      (list #f)))))
             ((= type 9)
             (println "Piece:  specific non-capture move type=9 Rae1")
             (let (( r1 (piece-specific-non-capture-move-1 turn (list-ref piece 4) intpiece tosq proc)))
               (println "Piece: type=9 r1: " r1)
               (cond ((car r1)
                      (println "Piece: OK type=9 looks good, specific (Rae1) non-capture move r1: " r1)
                      (list #t (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)(list-ref r1 4)(list-ref r1 5)))
                     (else
                      (list #f)))))

             ;; combining piece-check-move with piece-specific-not-capture-move-1 'a instead of 'a1
            ((= type 10)  ;; pass: turn index from tosq piece proc and turn algebra piece tosq type move proc
             (println "**Piece specific non-capture-move+ type=7 Rae1+")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (cond ((and (not (opponent-occupy? turn tosq)) (car r1)
                           (piece-check-move? turn (list-ref r1 1) (list-ref r1 3) tosq intpiece proc))
                      (println " type=10 piece-check-move? has passed test... now try to make the move")
                      (let (( r2 (piece-specific-non-capture-move-1 turn (list-ref piece 5) intpiece tosq proc)))
                        (cond ((car r2)
                               (println "OK type=10 OK looks good (Rae1+) r2: " r2)
                               (list #t (list-ref r2 1) (list-ref r2 2) (list-ref r2 3)(list-ref r2 4)(list-ref r2 5)))
                              (else
                               (list #f)))))
                     (else
                      (list #f)))))
            
            ((= type 11)  ;; Raxe1
             (println "working on this now... type 11")
             (let (( r1 (piece-specific-capture-move-1 turn (list-ref piece 5) intpiece tosq proc)))
               (println "Piece: type=11 r1: " r1)
               (cond ((car r1)
                      (println "Piece: OK type=11 looks good, specific (Raxe1) capture move r1: " r1)
                      (list #t (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)(list-ref r1 4)(list-ref r1 5)))
                     (else
                      (list #f)))))

            ((= type 12)  ;; Raxe1+
             (println "working on this now ... type 12")
             (let ((r1 (do-trypiece turn intpiece tosq ls proc)))
               (cond ((and (opponent-occupy? turn tosq) (car r1)
                           (piece-check-move? turn (list-ref r1 1) (list-ref r1 3) tosq intpiece proc))
                      (println " type=12 piece-check-move? has passed test... now try to make the move")
                      (let (( r2 (piece-specific-capture-move-1-+ turn (list-ref piece 5) intpiece tosq proc)))
                        (cond ((car r2)
                               (println "OK type=12 OK looks good (Raxe1+) r2: " r2)
                               (list #t (list-ref r2 1) (list-ref r2 2) (list-ref r2 3)(list-ref r2 4)(list-ref r2 5)))
                              (else
                               (list #f)))))
                     (else
                      (list #f)))))
            
            (else
              (println "this type -> " type " not implemented yet, piece -> " piece)
              (list #f))) ))

;; Note: the actual piece moves are now done in piece procedure ex: rook,knight.....
;;       instead of Piece procedure....

;; piece = 4, indices {0,7}
;; Note: (list-ref r1 1) is the move procedure ie move-piece, move-captured-piece...
(define rook
  (lambda ()
    (lambda (move turn tosq type piece)
       (println "**** Rook:  move -> " move " type: " type " turn: " turn
               " tosq: " tosq  " piece(3) " (list-ref piece 3) " piece: " piece)
       
       (let ((r1 (Piece move turn tosq type piece 4 (list 0 7) rook-move?)))
         (println "Rook r1-> " r1)
         (cond ((car r1)
                (println "Ok Rook making the move-> " move)
                r1)
               (else
                (println "Illegal move,  Rook move-> " move)
                (list #f)))))))
      
; Piece: move turn tosq type piece intpiece ls proc)
; do-trypiece; turn, piece, tosq, list of indices, procedure
; Note: if capture move 1st try 'knight-move? if returns #t
;          then try 'piece-capture to determine if Knight
;          can capture opponents piece

; piece = 2, indices {1,6}
(define knight
  (lambda ()
    (lambda (move turn tosq type piece)
      (println "**** knight:  move -> " move " type: " type " turn: " turn
               " tosq: " tosq  " piece(list): " piece)
      
; determine if move can be made and return move data to 'process-piece to make the move..
      (let ((r1 (Piece move turn tosq type piece 2 (list 1 6) knight-move?)))
        (println "** Returned, Knight r1-> " r1)
        (cond ((car r1)
                (println "Ok knight making the move-> " move)
                r1)
              (else
               (println "Illegal move,  Knight move-> " move)
               (list #f)))))))
                             
;;(do-trypiece 1 2 tosq (list 1 6) knight-move?) )))
;(setlist!(vector-ref *wpieces* index) 4 tosq)    ; sq
;(setlist1!(vector-ref *wpieces* index) 0 algebra)  ; algebra Zero index
;(setlist!(vector-ref *wpieces* index) 6 1)        ; state = 1, moved

; piece = 3, indices {2,5}
(define bishop
  (lambda ()
    (lambda (move turn tosq type piece)
      (println "**** Bishop:  move -> " move " type: " type " turn: " turn
               " tosq: " tosq  " piece(3) " (list-ref piece 3) " piece: " piece)

      (let ((r1 (Piece move turn tosq type piece 3 (list 2 5) bishop-move?)))
        (println "** Returned, Bishop r1-> " r1)
        (cond ((car r1)
               (println "Ok Bishop making the move-> " move)
               r1)
              (else
               (println "Illegal move,  Bishop move-> " move)
               (list #f)))))))
      
; piece = 5, index 3 OLD
(define queen-old
  (lambda ()
    (lambda (move turn tosq type piece)
      (println "**** Queen:  move -> " move " type: " type " turn: " turn
               " tosq: " tosq  " piece(3) " (list-ref piece 3) " piece: " piece)
      
      (let ((r1 (Piece move turn tosq type piece 5 (list 3) queen-move?)))
        (println "Queen r1-> " r1)
        (cond ((car r1)
               (println "Ok Queen making the move-> " move)
               r1)
              (else
               (println "Illegal move,  Queen move-> " move)
               (list #f)))))))

; piece = 5, index 3, check if pawn have queened... NEW
(define queen
  (lambda ()
    (lambda (move turn tosq type piece)
      (println "**** NEW Queen:  move -> " move " type: " type " turn: " turn
               " tosq: " tosq  " piece(3) " (list-ref piece 3) " piece: " piece)
      
      (let* (( ls (find-piece-indices turn 8 15 3 5))
            (r1 (Piece move turn tosq type piece 5 ls queen-move?)))
        (println "** Returned, Queen r1-> " r1 " list -> " ls)
        (cond ((car r1)
               (println "Ok Queen making the move-> " move)
               r1)
              (else
               (println "Illegal move,  Queen move-> " move)
               r1 )))))) ;; Testing
               ;;(list #f)))))))

; used mainly for queen as pawn can queen adding another queen
; default-index for queen would be 3, piece for queen 5
; start = start index end = end index, for pawns 8 thru 15
(define (find-piece-indices turn start end  default-index piece) 
  (let ((ls (list default-index))
            (v1 (choose-piece-vector)))
        (do ((i start (+ i 1)))
            ((> i end) ls)
          (cond ((= (readlist(vector-ref(v1 turn) i) 6) piece)
                 (println "found another piece -> " piece " at index -> " i)
                 (set! ls (cons i ls)))))))

;(find-piece-indices 1 8 15 3 5)
  

; piece = 6, index 4
(define king
  (lambda ()
    (lambda (move turn tosq type piece)
      (println "king move -> " move " piece: " piece)

      (let ((r1 (Piece move turn tosq type piece 6 (list 4) king-move?)))
        (println "** Returned, King r1-> " r1)
        (cond ((car r1)
               (println "Ok King making the move-> " move)
               r1)
              (else
               (println "Illegal move,  king move-> " move)
               (list #f)))))))

;(lambda (turn i1 i2 a1 a2 f1 f2 t1 t2  move)
; to castle king/queenside - check if sqs. between rook and king empty
; check if rook and king have not moved prior to castle move
(define castle
  (lambda ()
    (lambda (move turn tosq type piece)
      (println "castle procedure  move " move " turn " turn)
      ; ex: string-in-list "K" "movels* 0 1
       (cond ((= turn 1)  ; white              
              (cond ((string=? "O-O" move)
                     
                     (println "white, found kingside castle")
                     (println "white, Board(26) value -> " (list-ref(vector-ref *board* 26) 2))
                     (println "white, Board(27) value -> " (list-ref(vector-ref *board* 27) 2))
                     (println "white  rook state -> " (list-ref(vector-ref *wpieces* 7) 6))
                     (println "white  king state -> " (list-ref(vector-ref *wpieces* 4) 6))
                     (cond ((and (= (list-ref(vector-ref *board* 26) 2) 0) ; empty sq. value = 0
                                 (= (list-ref(vector-ref *board* 27) 2) 0)
                                 (= (list-ref(vector-ref *wpieces* 7) 6) 0) ;rook,king not moved = 0
                                 (= (list-ref(vector-ref *wpieces* 4) 6) 0))
                            (println "White, looks like it can Kingside castle!")
                            (move-castle turn 7 4 'f1 'g1 28 25 26 27 move)
                            #t)
                           (else
                            (println "White, can not! kingside castle!")
                            #f) ))
                    
                    ((string=? "O-O-O" move)
                      
                     (println "white, found queenside castle")
                     (println "white, Board(24) value -> " (list-ref(vector-ref *board* 24) 2))
                     (println "white, Board(23) value -> " (list-ref(vector-ref *board* 23) 2))
                     (println "white, Board(22) value -> " (list-ref(vector-ref *board* 22) 2))
                     (println "white  rook state -> " (list-ref(vector-ref *wpieces* 0) 6))
                     (println "white  king state -> " (list-ref(vector-ref *wpieces* 4) 6))
                     (cond ((and (= (list-ref(vector-ref *board* 24) 2) 0) ; empty sq. value = 0
                                 (= (list-ref(vector-ref *board* 23) 2) 0)
                                 (= (list-ref(vector-ref *board* 22) 2) 0)
                                 (= (list-ref(vector-ref *wpieces* 0) 6) 0) ; rook,king not move = 0
                                 (= (list-ref(vector-ref *wpieces* 4) 6) 0))
                            (println "White, looks like it can Queenside castle!")
                            (move-castle turn 0 4 'd1 'c1 21 25 24 23 move)
                            #t)
                           (else
                            (println "White, can not! Queenside castle!")
                            #f) ))))
             
             (( = turn 0) ; black
              (cond ((string=? "O-O" move)
                     
                     (println "black, found kingside castle")
                     (println "black, Board(96) value -> " (list-ref(vector-ref *board* 96) 2))
                     (println "black, Board(97) value -> " (list-ref(vector-ref *board* 97) 2))
                     (println "black  rook state -> " (list-ref(vector-ref *bpieces* 7) 6))
                     (println "black  king state -> " (list-ref(vector-ref *bpieces* 4) 6))
                     (cond ((and (= (list-ref(vector-ref *board* 96) 2) 0)  ; empty sq. value=0
                                 (= (list-ref(vector-ref *board* 97) 2) 0)
                                 (= (list-ref(vector-ref *bpieces* 7) 6) 0)
                                 (= (list-ref(vector-ref *bpieces* 4) 6) 0))
                            (println "black, looks like it can Kingside castle!")
                            (move-castle turn 7 4 'f8 'g8 98 95 96 97 move)
                            #t)
                           (else
                            (println "Black, can not! kingside castle!")
                            #f) ))
              
                    ((string=? "O-O-O" move)
                      
                     (println "black, found queenside castle")
                     (println "black, Board(94) value -> " (list-ref(vector-ref *board* 96) 2))
                     (println "black, Board(93) value -> " (list-ref(vector-ref *board* 97) 2))
                     (println "black, Board(92) value -> " (list-ref(vector-ref *board* 97) 2))
                     (println "black  rook state -> " (list-ref(vector-ref *bpieces* 0) 6))
                     (println "black  king state -> " (list-ref(vector-ref *bpieces* 4) 6))
                     (cond ((and (= (list-ref(vector-ref *board* 94) 2) 0)  ; empty sq. value=0
                                 (= (list-ref(vector-ref *board* 93) 2) 0)
                                 (= (list-ref(vector-ref *board* 92) 2) 0)
                                 (= (list-ref(vector-ref *bpieces* 0) 6) 0) ; rook,king not moved = 0
                                 (= (list-ref(vector-ref *bpieces* 4) 6) 0))
                            (println "Black, looks like it can Queenside castle!")
                            (move-castle turn 0 4 'd8 'c8 91 95 94 93 move)
                            #t)
                           (else
                            (println "Black, can not! Queenside castle!")
                            #f) ))) )))))

; determine if piece can check the King
; (r1 1) = index (r1 3) from
; will temporary setlist! to see if piece can check King then
; will reset piece to orginal sq and return #t or #f if move can be made
; (list-ref(vector-ref (v1 turn) 4)4) is opponents king intsq.
(define piece-check-move?
  (lambda (turn index from tosq piece proc)
    (println "piece-check-move?: turn " turn " index " index " from " from
             " tosq " tosq " piece " piece " proc " proc)
    
    (let ((v1 (choose-opposite-vector))   ; pointer to opponents vector (king's sq)
          (v1a (choose-piece-vector)))    ; pointer to piece vector, based on turn
      (setlist!(vector-ref (v1a turn) index) 4 tosq) ; change to temp value, then try to move to temp sq.
      (let ((v2 (do-trypiece turn piece (list-ref(vector-ref (v1 turn) 4)4) (list index) proc)))
        (setlist!(vector-ref (v1a turn) index) 4 from) ; restore previous value
        (println "Piece, (king+) move returned from 2rd do-trypiece v2 -> " v2)
        (cond ((car v2)  ; True??
               (println "piece-check-move?: returning true!!")
               #t)
              (else
               (println "piece-check-move?: returning false!!")
               #f)) )) ))

;;  (cond ((and ( not (opponent-occupy? turn tosq)) (car r2))
;;  (cond ((and ( opponent-occupy? turn tosq) (car r2))


(define piece-specific-non-capture-move
  (lambda (turn algebra piece tosq proc)
    (println "piece-specific-non-capture-move turn-> " turn " algebra-> " algebra " piece "
             piece " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((and (not (opponent-occupy? turn tosq)) (car r2))
                        (println "(PSNCM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                        (list #t turn (list-ref r2 1)
                              (get-algebra(list-ref r2 4))
                              (list-ref r2 3) (list-ref r2 4)) )
                       ;; #t)
                       (else
                        (println "piece-specific-non-capture-move? returning #f!!")
                        (list #f))) ))
              (else
               (println "piece-specific-non-capture-move? returning #f!!")
               (list #f))) ))))

; make specific piece move ex: Nc3xb5
; piece(5) = algebra
; Note: 'type just used for debugging..
(define piece-specific-capture-move
  (lambda (turn algebra piece tosq proc)
    (println "piece-specific-capture-move turn-> " turn " algebra-> " algebra " piece "
             piece " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((and (opponent-occupy? turn tosq) (car r2))
                        (println "(PSCM) Piece-> " piece " move looking good!! (r2)-> " r2)
                        (list #t turn (list-ref r2 1)
                              (get-algebra(list-ref r2 4))
                              (list-ref r2 3) (list-ref r2 4)) )
                       (else
                        (println "piece-specific-capture-move? returning #f!!")
                        (list #f))) ))
              (else
               (println "piece-specific-capture-move? returning #f!!")
               (list #f))) ))))



; make specific piece move ex: Rae1 NEW!!
; needs: turn algebra piece tosq proc
; donot need type,move
(define piece-specific-non-capture-move-1 
  (lambda (turn algebra piece tosq proc)
    (println "piece-specific-non-capture-move-1 turn-> " turn " algebra-> " algebra " piece "
             piece " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece-1 (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((and (not (opponent-occupy? turn tosq)) (car r2))
                        (println "(PSNCM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                        (list #t turn (list-ref r2 1)
                              (get-algebra(list-ref r2 4))
                              (list-ref r2 3) (list-ref r2 4)) )

                       ;; #t)
                       (else
                        (println "piece-specific-non-capture-move-1? returning #f!!")
                        (list #f))) ))
              (else
               (println "piece-specific-non-capture-move-1? returning #f!!")
               (list #f))) ))))

; make specific piece move ex: Raxe1  NEW!!
; needs: turn algebra piece tosq proc
; donot need type,move
(define piece-specific-capture-move-1
  (lambda (turn algebra piece tosq proc)
    (println "piece-specific-capture-move-1 turn-> " turn " algebra-> " algebra " piece "
             piece " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece-1 (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((and (opponent-occupy? turn tosq) (car r2))
                        (println "(PSCM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                        (list #t turn (list-ref r2 1)
                              (get-algebra(list-ref r2 4))
                              (list-ref r2 3) (list-ref r2 4)) )
                       ;; #t)
                       (else
                        (println "piece-specific-capture-move-1? returning #f!!")
                        (list #f))) ))
              (else
               (println "piece-specific-capture-move-1? returning #f!!")
               (list #f))) ))))


; make specific piece move ex: Raxe1+ NEW!!
; needs: turn algebra piece tosq proc
; donot need type,move
(define piece-specific-capture-move-1-+
  (lambda (turn algebra piece tosq proc)
    (println "piece-specific-capture-move-1-+ turn-> " turn " algebra-> " algebra " piece "
             piece " tosql " tosq " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece-1 (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((and (opponent-occupy? turn tosq) (car r2))
                        (println "(PSCM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                        (list #t turn (list-ref r2 1)
                              (get-algebra(list-ref r2 4))
                              (list-ref r2 3) (list-ref r2 4)) )
                       ;; #t)
                       (else
                        (println "piece-specific-capture-move-1-+? returning #f!!")
                        (list #f))) ))
              (else
               (println "piece-specific-capture-move-1-+? returning #f!!")
               (list #f))) ))))

;; *************************************************************************************
;; Currently not used - just for TESTing concept!!!!
; make specific piece move ex: Nc3b5
; needs: turn algebra piece tosq proc
; donot need type,move
(define piece-specific-non-and-capture-move
  (lambda (turn algebra piece tosq proc type)
    (println "piece-specific-non--and-capture-move turn-> " turn " algebra-> " algebra " piece "
             piece " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((= type 1)
                        (cond ((and (not (opponent-occupy? turn tosq)) (car r2))
                               (println "(PSNACM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                               (list #t turn (list-ref r2 1)
                                     (get-algebra(list-ref r2 4))
                                     (list-ref r2 3) (list-ref r2 4)) )
                              (else
                               (println "piece-specific-non-and-capture-move? returning #f!!")
                               (list #f))))
                        ((= type 2)
                         (cond ((and (opponent-occupy? turn tosq) (car r2))
                               (println "(PSNACM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                               (list #t turn (list-ref r2 1)
                                     (get-algebra(list-ref r2 4))
                                     (list-ref r2 3) (list-ref r2 4)) )
                              (else
                               (println "piece-specific-non-and-capture-move? returning #f!!")
                               (list #f))))
                         (else
                          (println "piece-specific-non-and-capture-move, type " type " not implemented!!")
                          (list #f))) ))
                     
              (else
               (println "piece-specific-non-and-capture-move? returning #f!!")
               (list #f)) )))))

;; Not Current ussing TESTing concept!!
(define piece-specific-non-and-capture-move-1 
  (lambda (turn algebra piece tosq proc type)
    (println "piece-specific-non-and-capture-move-1 turn-> " turn " algebra-> " algebra " piece "
             piece " proc-> " proc)
    (let ((v1 (choose-piece-vector)))   ; getting correct vector based on Turn..
      (let ((r1 (index-of-algebra-piece-1 (v1 turn) algebra 15 piece)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece turn piece tosq (cdr r1) proc)))
                 (cond ((= type 1)
                        (cond ((and (not (opponent-occupy? turn tosq)) (car r2))
                               (println "(PSNACM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                               (list #t turn (list-ref r2 1)
                                     (get-algebra(list-ref r2 4))
                                     (list-ref r2 3) (list-ref r2 4)) )
                              (else
                               (println "piece-specific-non-and-capture-move-1? returning #f!!")
                               (list #f))))
                       ((= type 2)
                        (cond ((and (opponent-occupy? turn tosq) (car r2))
                               (println "(PSNACM) Piece-> " piece "  move looking good!! (r2)-> " r2)
                               (list #t turn (list-ref r2 1)
                                     (get-algebra(list-ref r2 4))
                                     (list-ref r2 3) (list-ref r2 4)) )
                              (else
                               (println "piece-specific-non-and-capture-move-1? returning #f!!")
                               (list #f))))
                       (else
                        (println "piece-specific-non-and-capture-move-1, type " type " not implemented!!")
                        (list #f)))))                        
                        
              (else
               (println "piece-specific-non-and-capture-move-1? returning #f!!")
               (list #f))) ))))

;; ***************************************************************************

           
(define R (rook))
(define N (knight))
(define B (bishop))
(define Q (queen))
(define K (king))
(define C (castle))

(table-set! *ht* 'R R)
(table-set! *ht* 'N N)
(table-set! *ht* 'B B)
(table-set! *ht* 'Q Q)
(table-set! *ht* 'K K)
(table-set! *ht* 'O C)


;(define Ck (castle-king))
;(define Cq (castle-queen))

;(symbol? (list-ref(vector-ref *wpieces* 7) 0))

;;(setlist (vector-ref *wpieces* 0) 2 'R) ;; vector index, list index (starting at 0) change value
;(setlist (vector-ref *wpieces* 12) 0 'e2)
;(display-wpieces1)


(define rook-move?
  (lambda (turn from tosq)
    (println "rook-move? turn -> " turn " from: " from " to: " tosq)
    (cond ((not(same-occupy? turn tosq))
           (let ((receiver(lambda(break)
                            (let f ((cnt 0) ( i from) (j from) (i1 '()) (j1 '()))
                             ; (println "i-> " i " j-> " j " i1 " i1 " j1 " j1 " cnt" cnt)
                              (cond ((or (> i 98) (< i 21) (> cnt 7))
                                     (println "Rook-No-Match j -> " j " i " i)
                                     (break (list #f))))
                              (cond ((= i tosq)
                                     (println "Rook-Match i=tosq i-> " i " tosq-> " tosq " i1 " (append(list i)i1))
                                     (break (cons #t(append(list i)i1))))
                                    ((= j tosq)
                                     (println "Rook-Match j=tosq j-> " j " tosq-> " tosq " j1 " (append(list j)j1))
                                     (break (cons #t(append(list j)j1))))                                  
                                    ((< from tosq)
                                     (cond ((= (readlist(vector-ref *board* i)2) 99)
                                            (f(+ cnt 1)(- i 1)(+ j 10) (append(list i)i1)(append(list j)j1) ))
                                           (else
                                            (f(+ cnt 1)(+ i 1)(+ j 10) (append(list i)i1)(append(list j)j1) )) )) 
                                    ((< tosq from)
                                     (cond ((= (readlist(vector-ref *board* i)2) 99)
                                            (f(+ cnt 1)(+ i 1)(- j 10) (append(list i)i1)(append(list j)j1)))
                                           (else
                                            (f(+ cnt 1)(- i 1)(- j 10) (append(list i)i1)(append(list j)j1))))) )))))
             (call/cc receiver)))
          (else
           (list #f))) ))

;(rook-move? 1 57 61) ; fail
;(rook-move? 1 57 51)
;(rook-move? 1 47 97)
;(rook-move? 1 54 53)
;(rook-move? 1 48 41)
;(rook-move? 1 93 23)
;(rook-move? 1 34 33) ; fail
;(rook-move? 1 53 42) ; fail

;; +10 -10 +1 -1
; need to check if sqs 'from -> 'tosq unoccupied

;(let (( r1 (check-sqs-occupied? 1 (cdr(reverse(rook-move? 1 21 41))))))
;  (println "car of r1 -> " (car r1) " cdr of r1 " (cdr r1)))
;(check-sqs-occupied? 1 (cdr(reverse(rook-move? 1 33 93))))
;(check-sqs-occupied? 1 (cdr(reverse(rook-move? 1 23 93))))

(define knight-move?
  (lambda (turn from tosq)
    (println "knight-move? turn -> " turn " from: " from " to: " tosq)
    (cond ((not(same-occupy? turn tosq))
           (let ((receiver(lambda(break)
                            (let f ((cnt 0)( i from)(j from)(k from)(l from)
                                    (i1 '()) (j1 '()) (k1 '()) (l1 '()) )
                              ;(println "cnt-> " cnt " i-> " i " j-> " j " k-> " k " l-> " l)
                              (cond ((or (> i 98) (< i 21)(> cnt 1))
                                     (println "Knight-No-Match i-> " i " j-> " j " k-> " k " l-> " l)
                                     (break (list #f))))
                              (cond ((= i tosq)
                                     (println "Knight-Match i=tosq i-> " i " tosq-> " tosq " i1 " (append(list i)i1))
                                     (break (cons #t(append(list i)i1))))
                                    ((= j tosq)
                                     (println "Knight-Match j=tosq j-> " j " tosq-> " tosq " j1 " (append(list j)j1))
                                     (break (cons #t(append(list j)j1))))
                                    ((= k tosq)
                                      (println "Knight-Match k=tosq k-> " k " tosq-> " tosq " k1 " (append(list k)k1))
                                     (break (cons #t(append(list k)k1))))
                                    ((= l tosq)
                                      (println "Knight-Match k=tosq l-> " l " tosq-> " tosq " l1 " (append(list l)l1))
                                     (break (cons #t(append(list l)l1))))
                                    ((< from tosq)
                                     (f(+ cnt 1)(+ i 8)(+ j 12)(+ k 19)(+ l 21)
                                       (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))
                                    ((< tosq from)
                                     (f(+ cnt 1)(- i 8)(- j 12)(- k 19)(- l 21)
                                       (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))) ))))
             (call/cc receiver)))
          (else
           (list #f))) ))

;(knight-move? 1 22 43)

;; check if Knight move would put king in check+
(define knight-move+?
  (lambda (turn from tosq)
    (println "knight-move+? turn -> " turn " from: " from " to: " tosq)))

;(knight-move+? 1 62 95)

(define bishop-move?
  (lambda (turn from tosq)
    (println "bishop-move? turn -> " turn " from: " from " to: " tosq)
    (cond ((not(same-occupy? turn tosq))
           (let ((receiver(lambda(break)
                             (let f ((cnt 0) (i from) (j from) (i1 '()) (j1 '()))
                              ;(println "i-> " i " j-> " j " i1 " i1 " j1 " j1 " cnt: " cnt)
                              (cond ((or (> i 98) (< i 21) (> cnt 7))
                                     (println "Bishop-No-Match j -> " j " i " i)
                                     (break (list #f))))
                              (cond ((= i tosq)
                                     (println "Bishop-Match i=tosq i-> " i " tosq-> " tosq " i1 " (append(list i)i1))
                                     (break (cons #t(append(list i)i1))))
                                    ((= j tosq)
                                     (println "Bishop-Match j=tosq j-> " j " tosq-> " tosq " j1 " (append(list j)j1))
                                     (break (cons #t(append(list j)j1))))
                                    ((< from tosq)
                                     (cond ((= (readlist(vector-ref *board* i)2) 99) ; check if off the board
                                            (f(+ cnt 1)(- i 9) (+ j 11) (append(list i)i1)(append(list j)j1) ))
                                           ((= (readlist(vector-ref *board* j)2) 99)
                                            (f(+ cnt 1)(+ i 9) (- j 11) (append(list i)i1)(append(list j)j1) ))
                                           (else
                                            (f(+ cnt 1)(+ i 9) (+ j 11) (append(list i)i1)(append(list j)j1) )) ))
                                    ((< tosq from)
                                     (cond (( = (readlist(vector-ref *board* i)2) 99) ; check if off the board
                                            (f(+ cnt 1)(+ i 9) (- j 11) (append(list i)i1)(append(list j)j1) ))
                                           (( = (readlist(vector-ref *board* j)2) 99)
                                            (f(+ cnt 1)(- i 9) (+ j 11) (append(list i)i1)(append(list j)j1) ))
                                           (else
                                            (f(+ cnt 1)(- i 9) (- j 11) (append(list i)i1)(append(list j)j1))))) )))))
             (call/cc receiver)))
          (else
           (list #f))) ))

;(car(bishop-move? 1 91 54)) ;fail
;(bishop-move? 1 41 95); fail
;(bishop-move? 1 41 96)
;(bishop-move? 1 41 94)
;(bishop-move? 1 96 41)
;(bishop-move? 1 65 46)

;(cdr(reverse(bishop-move? 0 76 21)))
;(bishop-move? 1 54 76) ;; j
;(bishop-move? 1 46 28)


;; queen move is combination of Rook/Bishop move
(define queen-move?
  (lambda (turn from tosq)
    (println "** queen-move? turn -> " turn " from: " from " to: " tosq)
    (cond ((not(same-occupy? turn tosq))
           (let ((receiver(lambda(break)
                            (let f ((cnt 0) ( i from)(j from)(k from)(l from)
                                    (i1 '()) (j1 '()) (k1 '()) (l1 '()) )
                              
                              ;(println "cnt-> " cnt " i-> " i " j-> " j " k-> " k " l-> " l)
                              
                              (cond ((or (> i 98) (< i 21) (> cnt 7))
                                     (println "Quenn-No-Match i-> " i " j-> " j " k-> " k " l-> " l " cnt-> " cnt)
                                     (break (list #f))))
                              (cond ((= i tosq)
                                     (println "Queen-Match i=tosq i-> " i " tosq-> " tosq " i1 " (append(list i)i1))
                                     (break (cons #t(append(list i)i1))))
                                    ((= j tosq)
                                     (println "Queen-Match j=tosq j-> " j " tosq-> " tosq " j1 " (append(list j)j1))
                                     (break (cons #t(append(list j)j1))))
                                    ((= k tosq)
                                      (println "Queen-Match k=tosq k-> " k " tosq-> " tosq " k1 " (append(list k)k1))
                                     (break (cons #t(append(list k)k1))))
                                    ((= l tosq)
                                      (println "Queen-Match k=tosq l-> " l " tosq-> " tosq " l1 " (append(list l)l1))
                                     (break (cons #t(append(list l)l1))))
                                    ((< from tosq)
                                     (println "** queen-move? < from tosq,  from: " from " tosq: " tosq)
                                     (cond ((= (readlist(vector-ref *board* j)2) 99) ; check if off the board
                                            (f(+ cnt 1)(+ i 1)(- j 9)(+ k 10)(+ l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))
                                           ((= (readlist(vector-ref *board* i)2) 99)
                                               (f(+ cnt 1)(- i 1)(+ j 9)(+ k 10)(- l 11)
                                                 (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))
                                           ((= (readlist(vector-ref *board* l)2) 99)
                                            (f(+ cnt 1)(+ i 1)(+ j 9)(+ k 10)(- l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))
                                           (else
                                            (f(+ cnt 1)(+ i 1)(+ j 9)(+ k 10)(+ l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) )) ))
                                    ((< tosq from)
                                     (println "** queen-move? < tosq from, tosq: " tosq " from: " from) 
                                     (cond ((= (readlist(vector-ref *board* j)2) 99) ; check if off the board
                                            (f(+ cnt 1)(- i 1)(+ j 9)(- k 10)(- l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1)))
                                           ((= (readlist(vector-ref *board* i)2) 99)
                                            (f(+ cnt 1)(+ i 1)(- j 9)(- k 10)(- l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1)))
                                           ((= (readlist(vector-ref *board* l)2) 99)
                                            (f(+ cnt 1)(- i 1)(- j 9)(- k 10)(+ l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1)))
                                           (else
                                            (f(+ cnt 1)(- i 1)(- j 9)(- k 10)(- l 11)
                                              (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1))))) )))))
             (call/cc receiver)))
          (else
           (list #f))) ))

;(queen-move? 1 44 72)
;(queen-move? 1 41 95) ; fail
;(queen-move? 1 41 96)
;(queen-move? 1 41 94)
;(queen-move? 1 96 41)
;(queen-move? 1 64 46)
;(queen-move? 1 57 61) ; fail
;(queen-move? 1 57 51)

(define king-move?
  (lambda (turn from tosq)
    (println "king-move? turn -> " turn " from: " from " to: " tosq)
    (cond ((not(same-occupy? turn tosq))
           (let ((receiver(lambda(break)
                            (let f ((cnt 0)( i from)(j from)(k from)(l from)
                                    (i1 '()) (j1 '()) (k1 '()) (l1 '()) )
                              
                              ;(println "cnt-> " cnt " i-> " i " j-> " j " k-> " k " l-> " l)
                              
                              (cond ((or (> i 98) (< i 21) (> cnt 1))
                                     (println "King-No-Match i-> " i " j-> " j " k-> " k " l-> " l)
                                     (break (list #f))))
                              (cond ((= i tosq)
                                     (println "King-Match i=tosq i-> " i " tosq-> " tosq " i1 " (append(list i)i1))
                                     (break (cons #t(append(list i)i1))))
                                    ((= j tosq)
                                     (println "King-Match j=tosq j-> " j " tosq-> " tosq " j1 " (append(list j)j1))
                                     (break (cons #t(append(list j)j1))))
                                    ((= k tosq)
                                      (println "King-Match k=tosq k-> " k " tosq-> " tosq " k1 " (append(list k)k1))
                                     (break (cons #t(append(list k)k1))))
                                    ((= l tosq)
                                      (println "King-Match k=tosq l-> " l " tosq-> " tosq " l1 " (append(list l)l1))
                                     (break (cons #t(append(list l)l1))))
                                    ((< from tosq)
                                     (f(+ cnt 1)(+ i 1)(+ j 9)(+ k 10)(+ l 11)
                                       (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))
                                    ((< tosq from)
                                     (f(+ cnt 1)(- i 1)(- j 9)(- k 10)(- l 11)
                                       (append(list i)i1) (append(list j)j1) (append(list k)k1) (append(list l)l1) ))) ))))
             (call/cc receiver)))
          (else
           (list #f))) ))



;(king-move? 1 25 34)
;(king-move? 1 24 34) ; fail
;(king-move? 1 53 54)
;(king-move? 1 23 34)



