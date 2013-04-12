;; ex: (pawncc "b2"))
(define pawncc
  (lambda (move)
    (let ((ls (list  #\a #\b #\c #\d #\e #\f #\g #\h)))
      (let ((receiver (lambda (break)
                        (let f ((ls ls))
                          (cond ((null? ls)
                                 (list #f))
                                ((char=? (string-ref move 0) (car ls))
                                 (break (list #t (car ls) (string-length move))))
                                (else
                                 (f (cdr ls)))) )))) ;; matches receiver
        (call/cc receiver)) )))

(define parse-pawn
  (lambda (move)
    (let ((ls (list #\x #\+))
          (p (pawncc move)))
      (let ((recc (lambda (break)
                    (cond ((not (car p))  ; did not find a pawn - caddr p = length of move
                           (break (list #f)))
                              
                          ((= (caddr p) 2)   ; ex: e3
                           (println "ok, parse-pawn: move -> " move " to -> " (substring move 0 2) " " p)
                           (break (list #t (string-length move) (cadr p)
                                        (string->symbol (substring move 0 2)) #t )))

                          ;; NEW pawn queening - e8=Q
                          ((and (= (caddr p) 4) (char=?(string-ref move 2) #\=))
                           (println "ok, parse-pawn: move -> " move " to -> " (substring move 0 2) " " p)
                           (break (list #t (string-length move) (cadr p) (string->symbol(substring move 0 2)) #\=
                                        (string->symbol (substring move 3 4)) (car(piececc(substring move 3 4))) )))
                              
                          ((and (= (caddr p) 4) (char=? (string-ref move 1) (car ls))) ; ex: axb5
                           (println "ok, parse-pawn: move -> " move " to -> " (substring move 2 4) " " p)
                           (break (list #t (string-length move) (cadr p) #\x
                                        (string->symbol (substring move 2 4))(car(pawncc(substring move 2 4))) )))
                              
                          ((and (= (caddr p) 3) char=? (string-ref move 2) (cadr ls))  ; ex: d4+
                           (println "ok, parse-pawn: move -> " move " to -> " (substring move 0 2) " " p)
                           (break (list #t (string-length move) (cadr p) #\+
                                        (string->symbol (substring move 0 2)) #t )))

                          ((and (= (caddr p) 5) (char=? (string-ref move 1) (car ls))
                                (char=? (string-ref move 4) (cadr ls))) ;; axb5+
                           (println "ok, parse-pawn: move -> " move " to -> " (substring move 2 4) " " p)
                           (break (list #t (string-length move) (cadr p) #\x #\+ #\*
                                        (string->symbol (substring move 2 4))(car(pawncc(substring move 2 4))) )))

                          ;; special cases
                          ((and (= (caddr p) 5) (char=? (string-ref move 2) (car ls)))    ;; a3xb5
                           (println "ok, parse-pawn:  move ->  " move " to -> " (substring move 3 5) " " p)
                           (break (list #t (string-length move) (cadr p) #\x #\* (string->symbol(substring move 0 2))
                                        (string->symbol(substring move 3 5))(car(pawncc(substring move 3 5))) )))

                          ;; NEW e8=Q+
                          ((and (= (caddr p) 5) (char=?(string-ref move 2) #\=))
                           (println "ok, parse-pawn: move -> " move " to -> " (substring move 0 2) " " p)
                           (break (list #t (string-length move) (cadr p) (string->symbol(substring move 0 2)) #\= #\+
                                        (string->symbol (substring move 3 4)) (car(piececc(substring move 3 4))) )))
                           
                          
                          ((and (= (caddr p) 6) (char=? (string-ref move 2) (car ls))
                                (char=? (string-ref move 5) (cadr ls)) )  ;; a3xb5+
                           (println "ok, parse-pawn:  move ->  " move " to -> " (substring move 3 5) " " p)
                           (break (list #t (string-length move) (cadr p) #\x #\+ (string->symbol(substring move 0 2))
                                        (string->symbol(substring move 3 5))(car(pawncc(substring move 3 5))) )))
                          
                          (else
                           (break (list #f))) ))))
        (call/cc recc)) )))

;(parse-pawn "e8=Q")
;(parse-pawn "e8=Q+")
;(parse-pawn "exd5+")
;(parse-pawn "e8")

;; (char=? (list-ref piece 3) '#\x)
;; 'process-pawn with call 'move-piece with: turn,index(of piece to move),algebra-tosq, from, tosq
(define process-pawn
  (lambda (turn move)
    
    (println "*process-pawn: turn -> " turn " move -> " move)
    (let ((piece (parse-pawn move))
          (ls (list 8 9 10 11 12 13 14 15))) ;indices of Pawns in White/Black piece vectors
      (println "MPW process-pawn piece-> " piece " the list -> " ls)
      
      (cond ((car (check-move-legal? piece)) ; case: e3,d4 initial or standard pawn move
             (println "*process-pawn: found a pawn to move...." (cadr piece))
             
             (cond (( and (= 2 (list-ref piece 1)) (list-ref piece 4))
                     (println "*process-pawn: length 2, init/standard (e3),pawn -> " move " piece -> "
                              piece " to -> " (list-ref piece 3) ":" (get-square (list-ref piece 3)))
                     (let ((r1 (pawn-init-or-standard-move turn piece ls)))
                       (cond ((car r1)
                              (println "*process-pawn: OK, Standard Pawn move, r1-> " r1)
                              (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)(list-ref r1 4)(list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                     
                   ; case: e4+   OK!
                   (( and (= 3 (list-ref piece 1)) (char=? (list-ref piece 3) '#\+) (list-ref piece 5))
                    (println "length 3 pawn check (e4+) -> " move " piece -> " piece " to -> "
                             (list-ref piece 4) ":" (get-square (list-ref piece 4)))
                    (let ((r1 (pawn-check-move turn piece ls pawn-standard-move?)))
                      (cond ((car r1)
                             (println "process-pawn: OK, Standard+ move r1-> " r1)
                             (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                   (list-ref r1 4) (list-ref r1 5) move))
                             ;(move-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)(list-ref r1 4)(list-ref r1 5) move)
                             ;#t)
                            (else
                             (list #f)))))
                   
                   ; case: exd5  OK!  Testing enpassant capture!!!  MPW might need some WORK !!!
                   ; Note: (cadr *enpassant*) is +10 or -10 from enpassant pawn, below the pawn.
                   (( and (= 4 (list-ref piece 1)) (char? (list-ref piece 3))
                          (char=? (list-ref piece 3) '#\x) (list-ref piece 5))
                    (println "process-pawn: length 4  pawn capture (exd5)  -> " move " :piece -> " piece " to -> "
                             (list-ref piece 4) ":" (get-square (list-ref piece 4)))
                    (let ((r1 (pawn-capture-move turn piece ls)))  ;;?? maybe enpassant capture or standard capture
                      (cond ((car r1)
                             (println "process-pawn: OK, pawn capture move r1-> " r1)

                             ; check for possible enpassant,
                             ; enpassant set #t,1 (white) and blacks turn (0) then black can enpassant Wpawn
                             ; enpassant set #t,0 (black) and whites turn (1) then white can enpassant Bpawn
                             (cond ((and (car *enpassant*) (= (list-ref *enpassant* 2) 1) (= turn 0))
                                    (println "process-pawn trying enpassant of Wpawn by black *enpassant* " *enpassant*)
                                    (move-captured-piece-enpassant (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)
                                                                   (list-ref r1 4)(list-ref r1 5) (cadr *enpassant*) move)
                                    (set! *enpassant* (list #f)))

                                   ((and (car *enpassant*) (= (list-ref *enpassant* 2) 0) (= turn 1))
                                    (println "process-pawn trying enpassant of Bpawn by white *enpassant* " *enpassant*)
                                    (move-captured-piece-enpassant (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)
                                                                   (list-ref r1 4)(list-ref r1 5) (cadr *enpassant*) move)
                                    (set! *enpassant* (list #f)))

                                   (else
                                    (move-captured-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)
                                                         (list-ref r1 4)(list-ref r1 5) move)))

                             (list #t #\e))
                            (else
                            (list  #f #\e)))))
                   
                   ;; NEW pawn queening e8=Q Pawn will become a queen on opponents 8th sq.
                   (( and (= 4 (list-ref piece 1)) (char=? (list-ref piece 4) '#\=) (list-ref piece 6))
                    (println "process-pawn: found pawn queening... ex: e8=Q piece-> " piece)
                    (let ((r1 (pawn-init-or-standard-move turn piece ls)))
                       (cond ((car r1)
                              (println "*process-pawn: OK, Standard Pawn move plus Pawn Queening!! , r1-> " r1)
                              (list #t  move-queening-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))             
                              ;(move-queening-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)(list-ref r1 4)(list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))

                   ;;(symbol? (get-algebra(list-ref r1 4))) is #t
                   ;; special case: e8=Q+ Note: piece[3] = algebra of tosq ex: d8
                    (( and (= 5 (list-ref piece 1)) (char=? (list-ref piece 4) '#\=)  (char=? (list-ref piece 5) '#\+) (list-ref piece 7))
                     (println "process-pawn: len=5 (e8=Q+)  piece list-> " piece)
                     (let ((r1 (pawn-init-or-standard-move turn piece ls)))
                       (cond ((car r1)
                              (println "process-pawn: OK, standard pawn move,queening and check.. r1-> " r1
                                       " r2 (index) -> " (list-ref r1 2)
                                       " from -> " (list-ref r1 4) " to -> " (list-ref r1 5))
                              
                              ; need to make pawn a queen!!
                              (let ((v1 (choose-piece-vector)) ; get wpieces or bpieces vector
                                    (r3 '()))
                                (setlist! (vector-ref(v1 turn)(list-ref r1 2)) 1 9) ; queen value
                                (setlist! (vector-ref(v1 turn)(list-ref r1 2)) 5 5) ; now pawn is a queen
                                (cond ((= (readlist (vector-ref(v1 turn) (list-ref r1 2)) 4) 1)
                                       (set! r3 (list  "Q" (symbol->string(get-algebra(list-ref r1 4)))
                                                                          (symbol->string(get-algebra(list-ref r1 5))) "+"))
                                       (setlist! (vector-ref(v1 turn)(list-ref r1 2)) 2 'Q) ;white then piece is 'Q
                                       (setlist! (vector-ref *board* (list-ref r1 4)) 3 'Q))
                                      (else
                                       (set! r3 (list  "q" (symbol->string(get-algebra(list-ref r1 4)))
                                                                          (symbol->string(get-algebra(list-ref r1 5))) "+"))
                                       (setlist! (vector-ref(v1 turn)(list-ref r1 2)) 2 'q) ;black then piece is 'q
                                       (setlist! (vector-ref *board* (list-ref r1 4)) 3 'q)) )

                                (println " ** (e8=Q+) append-strings: " (apply string-append r3) " from: " (get-algebra(list-ref r1 4))
                                         " to: " (get-algebra(list-ref r1 5)))
                                ;(display-wpieces1);
                              
                                ; try to make new queen move (r3)
                                (let ((r1 (process-piece turn (apply string-append r3))))
                                  (cond ((car r1)
                                         (println "pawn queened can make Queen move! -> r3 "r3)
                                         (list #t  move-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                               (list-ref r1 4) (list-ref r1 5) move))
                                        (else
                                         (println "pawn queened cannot make Queen move! -> r3 " r3)
                                         (list #f)))) ))
                                             
                                ;(cond ((process-piece turn (apply string-append r3))
                                ;         (println "pawn queened can make Queen move! -> " r3)
                                ;         #t)
                                ;      (else
                                ;       (println "pawn queened cannot make Queen move! -> " r3)
                                ;       #f)) ))
                             (else
                               (list #f))) ))
                    
                   ; case: exd5+   OK!
                   (( and (= 5 (list-ref piece 1)) (char=? (list-ref piece 3) '#\x) (char=? (list-ref piece 4) '#\+)
                          (list-ref piece 7))
                    (println "process-pawn: len=5 p3 " (list-ref piece 3) " p4 " (list-ref piece 4)
                             " p7 " (list-ref piece 7) " sq " (get-square(list-ref piece 6)))
                    (let ((r1 (pawn-capture-check-move turn piece ls pawn-capture-move?)))
                      (cond ((car r1)
                             (println "process-pawn: OK, pawn capture+ move r1-> " r1)
                             (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                   (list-ref r1 4) (list-ref r1 5) move))
                             ;(move-captured-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)(list-ref r1 4)(list-ref r1 5) move)
                             ;#t)
                            (else
                             (list #f)))))
                    
                    ;; special case: e3xb5 OK...
                    (( and (= 5 (list-ref piece 1)) (char=? (list-ref piece 3) '#\x) (list-ref piece 7))
                     (println "process-pawn: len=5 (e3xf4)  piece list-> " piece " sq: " (get-square(list-ref piece 6)))
                     (let ((r1 (pawn-specific-capture-move turn piece ls)))
                       (cond ((car r1)
                              (println "process-pawn: OK, pawn specific capture move r1-> " r1)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))        
                              ;(move-captured-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)(list-ref r1 4)(list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                    
                    ; special case: e3xb5+   OK...
                    (( and (= 6 (list-ref piece 1)) (char=? (list-ref piece 3) '#\x)
                           (char=? (list-ref piece 4) '#\+)  (list-ref piece 7))
                     (println "length 5  pawn (e3xf4+)  -> " move " piece -> " piece " to -> "
                              (list-ref piece 6) ":" (get-square (list-ref piece 6)))
                     (let ((r1 (pawn-specific-capture-check-move turn piece ls pawn-capture-move?)))
                       (cond ((car r1)
                              (println "process-pawn: OK, pawn specific capture+ move r1-> " r1)
                              (list #t  move-captured-piece (list-ref r1 1) (list-ref r1 2) (list-ref r1 3)
                                    (list-ref r1 4) (list-ref r1 5) move))
                              ;(move-captured-piece (list-ref r1 1)(list-ref r1 2)(list-ref r1 3)(list-ref r1 4)(list-ref r1 5) move)
                              ;#t)
                             (else
                              (list #f)))))
                   (else
                    (println "process-pawn: Illegal pawn move ! " move)
                    (list #f))) ) 
            (else
             (println "process-pawn: illegal pawn move !!")
             (list #f))) )))

;(process-pawn 1 "d8=Q")
;(process-pawn 1 "d8=Q+")
;(reset-board)
;(pgngame "queentest.pgn")
;(process-pawn 1 "e8")
;(process-pawn 1 "e3+")
;(process-pawn 1 "exd5+")
;(process-pawn 1 "e5+")
;(process-pawn 1 "d6+") ; fail
;(process-pawn 1 "a6")

;(process-pawn 1 "d4")

;  ************ start of Pawn moves *********

  ; try to make pawn initial or standard move
  ; Note: 'piece is a list returned from 'parse-pawn
  ; Note: ls is list of Pawn indices in piece vectors
(define pawn-init-or-standard-move
  (lambda (turn piece ls)

    (println "** pawn-init-or-standard-move piece[3] -> " (list-ref piece 3))
    
     (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 3)) ls pawn-move-init-pos?)))
       (cond ((car r1) ; Inital move??
              (println "*pawn-init-or-standard-move: OK, Initial Pawn move. r1-> " r1)
              (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
             (else
              (let ((r2 (do-trypiece-pawn turn 1 (get-square(list-ref piece 3)) ls pawn-standard-move?)))
                (cond ((car r2) ; Standard move??
                       (println "pawn-init-or-standard-move - standard move.. r2 -> " r2)
                       (list #t turn (list-ref r2 1) (get-algebra(list-ref r2 4)) (list-ref r2 3) (list-ref r2 4)))
                      (else
                       (println "pawn-init-or-standard-move: Pawn, couldn't make init/standard move! r1 " r1 " r2 " r2)
                       (list #f))))))) ))

;pawn capture move (exd5)
; need to return data showing standard or enpassant type ??? checks it in 'process-pawn
(define pawn-capture-move-testing....
  (lambda (turn piece ls)

    (let ((p1 '()))
      (if (car  *enpassant*) ;is this an enpassant capture type move??
          (set! p1 pawn-capture-move-enpassant?)
          (set! p1 pawn-capture-move?))
      (println "pawn-capture-move, Testing!! will be capture-enpassant or standard-capture p1-> " p1)
;    (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls pawn-capture-move?)))
      
      (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls p1)))  
        (println "**pawn-capture-move: pawn capture (r1) from do-trypiece -> " r1)
        (cond ((car r1)
               (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
              (else
               (println "*pawn-caputre-move: Pawn couldn't make capture move! r1 " r1)
               (list #f)))))))

;; combo, pawn-capture-move-enpassant? and pawn-capture-move?
;; if *enpassant* = #t then try to make enpassant capture. if the capture fails
;; then try standard capture. this is kind of a Hack because opponents pawn could
;; take via enpassant or take another pawn via standard capture move
;; if *enpassant* = #f then just do standard capture move
(define pawn-capture-move
  (lambda (turn piece ls)

    (cond ((car  *enpassant*) ;is this an enpassant capture type move??
           (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls pawn-capture-move-enpassant?)))  
             (println "**pawn-capture-move: pawn capture (enpassant) r1-> " r1)
             (cond ((car r1)  ;; capture-move-enpassant? fails - try standard capture move
                    (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
                   (else
                    (println "*pawn-caputre-move: Pawn couldn't make capture (enpassant) move! r1 " r1)
                    (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls pawn-capture-move?)))
                      (println "**pawn-capture-move: pawn capture (standard) r1-> " r1)
                      (set! *enpassant* (list #f))
                      (cond ((car r1)
                             (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
                            (else
                             (println "*pawn-caputre-move: Pawn couldn't make capture (standard) move! r1 " r1)
                             (list #f))) )))))

          (else  ;; standard capture move
           (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls pawn-capture-move?)))  
             (println "**pawn-capture-move: pawn capture (standard) r1-> " r1)
             (cond ((car r1)
                    (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
                   (else
                    (println "*pawn-caputre-move: Pawn couldn't make capture (standard) move! r1 " r1)
                    (list #f))))) )))
             

;pawn capture move (exd5)  - original procedure...  
(define pawn-capture-move-1
  (lambda (turn piece ls)

    (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls pawn-capture-move?)))
      (println "**pawn-capture-move: pawn capture (r1) from do-trypiece -> " r1)
      (cond ((car r1)
             (list #t turn (list-ref r1 1) (get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
            (else
             (println "*pawn-caputre-move: Pawn couldn't make capture move! r1 " r1)
             (list #f))))))

; pawn specific capture move (e3xb5)
(define pawn-specific-capture-move
  (lambda (turn piece ls)
    
    (let ((v1 (choose-piece-vector)))  ; to allow for *bpiece* or *wpiece* vector
      (let ((r1 (index-of-algebra-piece (v1 turn) (list-ref piece 5) 15 1)))
        (cond ((car r1) ; Ok, found correct index
               (let ((r2 (do-trypiece-pawn turn 1 (get-square(list-ref piece 6))
                                           (cdr r1) pawn-capture-move?)))
                 (println "pawn-specific-capture-move (e3xb5) move r2 -> " r2)
                 (cond ((car r2)
                        (list #t turn (list-ref r2 1) (get-algebra(list-ref r2 4)) (list-ref r2 3)(list-ref r2 4)))
                       (else
                        (println "*process-pawn: Pawn coultn't make specific capture standard move! r1 " r1 " r2 " r2)
                        (list #f)))))
              (else
               (println "pawn-specific-capture-move Error wrong index! r1-> " r1)
               (list #f)) )))))

; pawn check move (e6+) (exf5+)
; (list-ref r1 1) = index (get-square(list-ref piece 4)) = intsq of algebra sq.
; (get-algebra(list-ref r1 4) = algebra of tosq.
; move-piece = turn,index,algebra from tosq move
; do-trypiece-pawn: turn, intpiece, tosq, ls (indices of pawns), procedure
; proc = pawn procedure like: pawn-standard-move? or something else.....
(define pawn-check-move
  (lambda (turn piece ls proc)
    (println "pawn-check-move turn: " turn " move: " move " piece " piece " ls " ls)

    (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 4)) ls proc)))
      (cond ((not (car r1))
             #f) ; False, cannot make move!!
            (else
             (println "pawn-check-move: so far looks good r1: " r1
                      " tosq " (get-square(list-ref piece 4)) " index " (list-ref r1 1))
      
             (let ((v1 (choose-opposite-vector))  ; opponents piece vector (king sq)
                   (v1a (choose-piece-vector)))   ; current turn's piece vector
               (println "pawn-check-move: Now change to temp tosq value via setlist!")
               (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4 (get-square(list-ref piece 4))) ; temp, try move to sq.
        
               (let ((v2 (do-trypiece-pawn turn 1 (list-ref(vector-ref (v1 turn) 4)4)
                                           (list(list-ref r1 1)) pawn-capture-move?)))
                 (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4 (list-ref r1 3)) ; restore previous value (from)
                 (println "pawn-check-move: Old value restorred to vector via setlist!")
          
                 (cond ((car v2) ; True?
                        (list #t turn (list-ref r1 1)(get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
                       (else
                        (println "pawn-check-move: Illegal move via do-trypiece-pawn, move " move)
                        (list #f))) )))))))

;; NEW!! needs work............ MPW most likely will not be used
(define pawn-queen-check-move
  (lambda (turn piece ls proc)
    (println "pawn-queen-check-move turn: " turn " move: " move " piece " piece " ls " ls)

    (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 3)) ls proc)))
      (cond ((not (car r1))
             #f) ; False, cannot make move!!
            (else
             (println "pawn-queen-check-move: so far looks good r1: " r1
                      " tosq " (get-square(list-ref piece 3)) " index " (list-ref r1 1))
      
             (let ((v1 (choose-opposite-vector))  ; opponents piece vector (king sq)
                   (v1a (choose-piece-vector)))   ; current turn's piece vector
               (println "pawn-queen-check-move: Now change to temp tosq value via setlist!")
               (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4 (get-square(list-ref piece 4))) ; temp, try move to sq.
               (setlist!(vector-ref (v1a turn) index) 5 5)    ; change to temp value, now pawn is a queen!
               
               (let ((v2 (do-trypiece-pawn turn 1 (list-ref(vector-ref (v1 turn) 4)4) (list(list-ref r1 1)) pawn-capture-move?)))
                 (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4 (list-ref r1 3)) ; restore previous value (from)
                 (setlist!(vector-ref (v1a turn) index) 5 1)    ; restore previous value back to a pawn!
                 (println "pawn-queen-check-move: Old value restorred to vector via setlist!")
          
                 (cond ((car v2) ; True?
                        (list #t turn (list-ref r1 1)(get-algebra(list-ref r1 4)) (list-ref r1 3) (list-ref r1 4)))
                       (else
                        (println "pawn-queen-check-move: Illegal move via do-trypiece-pawn, move " move)
                        (list #f))) )))))))

; same as 'pawn-check-move expect this is (exf6+) capture and king check.
; (list-ref piece 6) = algebra of tosq
; Only difference is the 'piece list is differrent then above proc. '#t5ex+*f6#t
(define pawn-capture-check-move
  (lambda (turn  piece ls proc)
    (println "pawn-capture-check-move turn: " turn " move: " move " piece " piece " ls " ls)

    (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 6)) ls proc)))
      (cond ((not (car r1))
             #f) ; False, cannot make move!!
            (else
             (println "pawn-capture-check-move: so far looks good r1: " r1
                      " tosq " (get-square(list-ref piece 6)) " index " (list-ref r1 1))
      
             (let ((v1 (choose-opposite-vector))  ; opponents piece vector (king sq)
                   (v1a (choose-piece-vector)))   ; current turn's piece vector
               (println "pawn-capture-check-move: Now change to temp tosq value via setlist!")
               (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4
                         (get-square(list-ref piece 6))) ; temp, try move to sq.
        
               (let ((v2 (do-trypiece-pawn turn 1 (list-ref(vector-ref (v1 turn) 4)4)
                                           (list(list-ref r1 1)) pawn-capture-move?)))
                 (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4
                           (list-ref r1 3)) ; restore previous value (from)
                 (println "pawn-capturecheck-move: Old value restorred to vector via setlist!")
          
                 (cond ((car v2) ; True?
                        (list #t  turn (list-ref r1 1)(get-algebra(list-ref r1 4)(list-ref r1 3) (list-ref r1 4))))
                       (else
                        (println "pawn-capture-check-move: couldn't make the move-> " move)
                        (list #f))) )))))))

;this one does:  e3xb5+
 
(define pawn-specific-capture-check-move
  (lambda (turn piece ls proc)
    (println "pawn-specific-capture-check-move turn: " turn " move: " move " piece " piece " ls " ls)

    (let* ((v1 (choose-piece-vector))  ; to allow for *bpiece* or *wpiece* vector
           (v1a (index-of-algebra-piece (v1 turn) (list-ref piece 5) 15 1)))
        
        (cond ((not (car v1a)) ; Ok, found correct index
               #f)
              (else
    ; going to try specific pawn via (cdr v1a)
               (let ((r1 (do-trypiece-pawn turn 1 (get-square(list-ref piece 6)) (cdr v1a) proc)))
                 (cond ((not (car r1))
                        #f) ; False, cannot make move!!
                       (else
                        (println "pawn-specific-capture-check-move: so far looks good r1: " r1
                                 " tosq " (get-square(list-ref piece 6)) " index " (list-ref r1 1))
      
                        (let ((v1 (choose-opposite-vector))  ; opponents piece vector (king sq)
                              (v1a (choose-piece-vector)))   ; current turn's piece vector
                          (println "pawn-specific-capture-check-move: Now change to temp tosq value via setlist!")
                          (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4
                                    (get-square(list-ref piece 6))) ; temp, try move to sq.
        
                          (let ((v2 (do-trypiece-pawn turn 1 (list-ref(vector-ref (v1 turn) 4)4)
                                                      (list(list-ref r1 1)) pawn-capture-move?)))
                            (setlist! (vector-ref (v1a turn) (list-ref r1 1)) 4
                                      (list-ref r1 3)) ; restore previous value (from)
                            (println "pawn-specific-capturecheck-move: Old value restorred to vector via setlist!")
          
                            (cond ((car v2) ; True?
                                   (list #t turn (list-ref r1 1)(get-algebra(list-ref r1 4))(list-ref r1 3) (list-ref r1 4)))
                                  (else
                                   (println "pawn-specific-capture-check-move: couldn't make the move-> " move)
                                   (list #f))) ))))))))))
    
    

;; check if pawn at initial position
;; and tosq not occupied by opponent or same color piece
;; +10, +20 or -10 - 20
(define pawn-move-init-pos?
  (lambda (turn from tosq)
    (println "pawn-move-init-pos? turn " turn " from " from " tosq " tosq)
    (cond ((= turn 1)
           (cond ((and (>= from 31) (<= from 38) (not(opponent-occupy? turn tosq)) (not(same-occupy? turn tosq)))
                  (println "checking ... Wpawn at initial position -> " from)
                  (cond ((= (+ 10 from) tosq)
                         (println "pawn-move-init-pos? Wpawn (+10) from: " from " to " tosq)
                         #t)
                        ((= (+ 20 from) tosq) 
                         (println "pawn-move-init-pos? Wpawn (+20) from: " from  " to " tosq)
                         (let ((r1 (enpassant? turn tosq)))
                           (cond ((car r1)
                                  (println "pawn-move-init-pos? (Wpawn) could be enpassant! r1-> " r1)
                                  (set! *enpassant* r1))
                                 (else
                                  (set! *enpassant* (list #f)))))
                         #t)
                        (else
                         #f)))
                 (else
                   #f)))
           
          ((= turn 0)
           (cond ((and (>= from 81) (<= from 88) (not(opponent-occupy? turn tosq)) (not(same-occupy? turn tosq)))
                  (println "checking ... Bpawn  at inital position -> "  from)
                  (cond ((= (- from 10) tosq)
                         (println "pawn-move-init-pos? Bpawn (-10) from: " from " to " tosq)
                         #t)
                        ((= (- from 20) tosq)
                         (println "pawn-move-init-pos? Bpawn (-20) from: " from " to " tosq)
                         (let ((r1 (enpassant? turn tosq)))
                           (cond ((car r1)
                                  (println "pawn-move-init-pos? (Bpawn) could be enpassant! r1-> " r1)
                                  (set! *enpassant* r1))
                                 (else
                                  (set! *enpassant* (list #f)))))
                         #t)
                        (else
                         #f)))
                 (else
                   #f))) )))
                   
;(pawn-move-init-pos? 1 33 53)
;(pawn-move-init-pos? 0 84 64)

;; standard 1 rank pawn move, #t if it can be made otherwise #f
(define pawn-standard-move?
  (lambda (turn from tosq)
    (println "pawn-standard-move? turn " turn " from " from " tosq " tosq)
    (cond  ((= turn 1)
            (cond ((and (not(opponent-occupy? turn tosq)) (not(same-occupy? turn tosq))
                        (= (+ 10 from) tosq))
                   #t)
                  (else
                   #f)))
           ((= turn 0)
            (cond ((and (not(opponent-occupy? turn tosq)) (not(same-occupy? turn tosq))
                        (= (- from 10) tosq))
                   #t)
                  (else
                   #f)))
           (else
            #f))))

;(pawn-standard-move? 0 83 73)

;; white can capture +11 or +9, black can capture -11 or -9
;; checking if value of board > 0 and opponent is occupying tosq
(define pawn-capture-move?
  (lambda (turn from tosq)
    (println "pawn-capture-move? turn -> " turn " from -> " from " to -> " tosq)
    (cond ((= turn 1)
           (cond ((or (= (+ 11 from) tosq) (= (+ 9 from) tosq))
                  (if (and (sq-nonzero-value? tosq) (opponent-occupy? turn tosq))
                      #t
                      #f))
                 (else
                  #f)))
          ((= turn 0)
           (cond ((or (= (- from 11) tosq) (= (- from 9) tosq))
                  (if (and (sq-nonzero-value? tosq) (opponent-occupy? turn tosq))
                      #t
                      #f))
                 (else
                  #f)))
          (else
           #f))))
           
;(pawn-capture-move? 1 43 54)
;(pawn-capture-move? 1 74 85)

;; white can capture +11 or +9, black can capture -11 or -9
;; checking if value of board > 0 and opponent is occupying tosq
;; tosq not occupied with no nonzero value - moving to open sq.
(define pawn-capture-move-enpassant?
  (lambda (turn from tosq)
    (println "pawn-capture-move-enpassant? turn -> " turn " from -> " from " to -> " tosq)
    (println "pawn-capture-move-enpassant? a-> " (- tosq 10) " b-> " (+ 10 tosq))
    (cond ((= turn 1)
           (cond ((or (= (+ 11 from) (+ 10 tosq)) (= (+ 9 from) (+ 10 tosq)))
                  (println "pawn-capture-move-enpassant? turn = 1 passed first test")
                  (if (and (not(sq-nonzero-value? (+ 10 tosq))) (not(opponent-occupy? turn (+ 10 tosq))))
                      #t
                      #f))
                 (else
                  #f)))
          ((= turn 0)
           (cond ((or (= (- from 11) ( - tosq 10)) (= (- from 9) (- tosq 10)))
                  (println "pawn-capture-move-enpassant? turn = 0 passed first test")
                  (if (and (not(sq-nonzero-value? (- tosq 10))) (not(opponent-occupy? turn (- tosq 10))))
                      #t
                      #f))
                 (else
                  #f)))
          (else
           #f))))

;; white can capture +11 or +9, black can capture -11 or -9
;; checking if value of board > 0 and opponent is occupying tosq
(define pawn-capture-move-chk?
  (lambda (turn from tosq)
    (cond ((= turn 1)
           (cond ((or (= (+ 11 from) tosq) (= (+ 9 from) tosq))
                  (if (king-checked? 1 tosq)
                      #t
                      #f))
                 (else
                  #f)))
          ((= turn 0)
           (cond ((or (= (- from 11) tosq) (= (- from 9) tosq))
                  (if (king-checked? 0 tosq)
                      #t
                      #f))
                 (else
                  #f)))
          (else
           #f))))
;(pawn-capture-move-chk? 1 84 95)

;; only check this routine on initial pawn move (+ 20) or (- 20)
;; white pawn from 51 to 58 and black pawn 51 to 58, enpassant = #t
;;     next move black could enpassant white
;; black pawn from 61 to 68 and white pawn 61 to 68, enpassant = #t
;;     next move white could enpassant black
;;  vector-ref 5 = intsq
(define enpassant?
  (lambda (turn tosq)
    (let* ((v1 (choose-opposite-vector))
           (v2 (v1 turn))
           (ls '()))
              
           (println "enpassant? turn " turn " tosq " tosq " vector " v2)
           (set! ls (list #f))
           
           (let f (( i 8))
             (cond (( < i 16) ; check opppostie piece vector (pawna) for possible enpassant
                    (println "vector index-> " i " sq: " (readlist (vector-ref v2  i) 5))
                    (println "** " (+ 1 (readlist(vector-ref v2 i) 5)) " " (- (readlist(vector-ref v2 i) 5) 1))

                    (cond ((or (= (+ 1 (readlist(vector-ref v2 i) 5)) tosq) (= (- (readlist(vector-ref v2 i) 5) 1) tosq))
                           (cond ((= turn 1)  ;white, black would move to this sq. if enpassant White
                                  (set! ls (list #t (- tosq 10)1))
                                  (println "(+a) ls-> " ls))
                                 (else       ; black, white would move to this sq. if enpssant black 
                                  (set! ls (list #t (+ tosq 10)0))
                                  (println "(+b) ls-> " ls)))
                           (println "** enpassant? + possible enpassant! at index-> " i " ls-> " ls)
                           (f(+ i 16)))
                          (else
                           (f(+ i 1)))))
                   (else
                    ls))))))
                                  

;(reset-board)

;(let ((r1 (enpassant? 1 52)))
;  (println "r1-> " r1))

;  (if (car r1)
;      (println "possible enpassant (car r1) " (car r1) " (cdr r1) " (cdr r1))
;      (println "no enpassant (car r1) " (car r1))))
