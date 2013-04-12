(load "chess-util.scm")
(load "chess-piece-moves.scm")
(load "chess-pawn-moves.scm")

;; log file
(define (open-logfile filename)
  (let ((p (open-output-file filename)))
    p))

(define (close-logfile p)
  (close-port p))

(define fptr (open-logfile "chesslog.out"))

;read wpieces/bpieces
; algegra,value,charpiece,color,sq,intpiece
(define *movels* '())

(define (reset-movels)
  (set! *movels* '()))

(define (read-pgn-file file)
  (println "read-pgn-file " file)
  (let ((p (open-input-file file)) (ls '())) 
    (let loop ((i 1))
      (let ((x (read p)))
        (cond ((eof-object? x)
               (println " read-pgn-file: eof for: " file " the list -> " ls)
               ls)
              ((number? x)
               (loop (+ i 1)))
              (else
               (set! ls (cons x ls))
               ;(print x)
               (loop (+ i 1))) )))))


;(let (( x (reverse(read-pgn-file "fischer1.pgn"))))
;  (let loop ((ls x))
;    (cond ((null? ls)
;           (newline)
;           #t)
;          (else
;           (print " " (car ls))
;           (cond ((symbol? (car ls))
;                  (println "found symbol")))
;           (loop (cdr ls)))) ))
    
(define (read-wpieces)
  (display "reading wpieces.txt...")(newline)
  (let ((p (open-input-file "wpieces.txt"))
        (ls '()))
    (do ((i 0 (+ 1 i)))
        ((> i 15) #t)
      (do ((j 0 (+ 1 j)))
          ((> j 6) #t)
        (let (( x (read p)))
          (set! ls (cons  x ls)) )) 
      (vector-set! *wpieces* i (reverse ls))
      (println "loop -> " i  " " (vector-ref *wpieces* i))
      (set! ls '()) )))


(define (display-wpieces1)
  (do ((i 0 (+ 1 i)))
      ((> i 15) #t)
    (println " " (readlist (vector-ref *wpieces* i) 1) " "
            (readlist (vector-ref *wpieces* i) 2) " "
            (readlist (vector-ref *wpieces* i) 3) " "
            (readlist (vector-ref *wpieces* i) 4) " "
            (readlist (vector-ref *wpieces* i) 5) " "
            (readlist (vector-ref *wpieces* i) 6) " "
            (readlist (vector-ref *wpieces* i) 7))))
            
(define (read-bpieces)
  (display "reading bpieces.txt....")(newline)
  (let ((p (open-input-file "bpieces.txt"))
        (ls '()))
    (do ((i 0 (+ 1 i)))
        ((> i 15) #t)
      (do ((j 0 (+ 1 j)))
          ((> j 6) #t)
        (let (( x (read p)))
          (set! ls (cons  x ls)) )) 
      (vector-set! *bpieces* i (reverse ls))
      (println "loop -> " i " " (vector-ref *bpieces* i))
      (set! ls '()) )))
;;(read-bpieces)

(define (display-bpieces1)
  (do ((i 0 (+ 1 i)))
      ((> i 15) #t)
    (println " " (readlist (vector-ref *bpieces* i) 1) " "
            (readlist (vector-ref *bpieces* i) 2) " "
            (readlist (vector-ref *bpieces* i) 3) " "
            (readlist (vector-ref *bpieces* i) 4) " "
            (readlist (vector-ref *bpieces* i) 5) " "
            (readlist (vector-ref *bpieces* i) 6) " "      
            (readlist (vector-ref *bpieces* i) 7) )))
    
; charsq,intsq,value,piece,color    
(define (read-board)
  (display "reading board.txt....")(newline)
  (let ((p (open-input-file "board.txt"))
        (ls '()))
    (do ((i 0 (+ 1 i)))
        ((> i 120) #t)
      (do ((j 0 (+ 1 j)))
          ((> j 4) #t)
        (let (( x (read p)))
          ;;(set! ls (append (list x) ls))
          (set! ls (cons  x ls)) )) 
      (vector-set! *board* i (reverse ls))
      (println "loop -> " i " " (vector-ref *board* i))
      (set! ls '()) )))

(define (display-board)
  (println "display board")
  (let ((ls '()))
    (do ((i 0 (+ 1 i)))
        ((> i 120) #t)
      (do ((j 1 (+ 1 j)))
          ((> j 5) #t)
        (set! ls (cons (readlist(vector-ref *board* i) j) ls)) )
      (if (not (number? (car(reverse ls))))
          (println (reverse ls)))
      (set! ls '()) )))



(define (db)
  (newline)
  (let f(( i 91))
    (cond ((> i 11)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                    (print " |" (readlist(vector-ref *board* temp) 4) "|")
                    (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline))))
           (f (- i 10))))))

;(db)

(define (db-algebra)
  (newline)
  (let f(( i 91))
    (cond ((> i 11)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                    (print  " |" (readlist(vector-ref *board* temp) 1) "|")
                    (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline))))
           (f (- i 10))))))
;(db-algebra)

(define (db-sqs)
  (newline)
  (let f(( i 91))
    (cond ((> i 11)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                    (print " |" (readlist(vector-ref *board* temp) 2) "|")
                    (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline))))
           (f (- i 10))))))
;(db-sqs)

(define (db-values)
  (newline)
  (let f(( i 91))
    (cond ((> i 11)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                    (print " |" (readlist(vector-ref *board* temp) 3) "|")
                    (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline))))
           (f (- i 10))))))
;(db-values)

; 1=white, 0=black
(define (db-colors)
  (newline)
  (let f(( i 91))
    (cond ((> i 11)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                    (if(= (readlist(vector-ref *board* temp)5) 0) 
                          (print "[" 'b "]")
                          (print "[" 'w "]"))
                    (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline))))
           (f (- i 10))))))


;; board numbers
(define (dbnum)
  (let f(( i 21))
    (cond ((< i 100)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                      (print "[" (readlist(vector-ref *board* temp) 2) "]")
                      (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline)) ))
           (f (+ i 10)) ))))

;; board algebra
(define (dbchar)
  (let f(( i 21))
    (cond ((< i 100)
           (let f1 ((j 0) (temp i))
             (cond (( < j 8)
                      (print "[" (readlist(vector-ref *board* temp) 1) "]")
                      (f1 (+ 1 j)(+ temp 1)))
                   (else
                    (newline)) ))
           (f (+ i 10)) ))))

;; vector-set!
;; hash table for board squares ex: a1 21, a2 22
;; hash-table-clear! ht  clears hash table
;; hash-table-get ht key
;; hash-table-put! key value
(define (read-algebra)
  (display "reading algebra.txt....")(newline)
  (let ((p (open-input-file "algebra.txt"))
        (ls '()))
    (do ((i 0 (+ 1 i)))
        ((> i 63) #t)
      (let (( k (read p))
            ( v (read p)))
        (table-set! *htsqs* k v)
        (println "loop -> " i " key " k " value " v)) )))

(define (read-sqs)
  (display "reading squares.txt....")(newline)
  (let ((p (open-input-file "squares.txt"))
        (ls '()))
    (do ((i 0 (+ 1 i)))
        ((> i 63) #t)
      (let (( k (read p))
            ( v (read p)))
        (table-set! *htalgebra* k v)
        (println "loop -> " i " key " k " value " v)) )))

;; reset game with reading vales from files
;(read-board)
;(read-algebra)
;(read-sqs)
;(read-wpieces)
;(read-bpieces)
;(reset-movels)

; pretty-print the moves
(define (pp-movels ls)
  (newline)
  (let ((i 1))
    (let loop ((ls ls)(j 1))
      (cond ((null? ls)
             (newline)
             #t)
            (else
             (cond ((odd? j )
                    (print i " " (car ls) " ")
                    (set! i (+ 1 i)))
                    (else
                     (print(car ls) " ")
                     (set! i i)))
             (loop (cdr ls)(+ 1 j)))) )))

(pp-movels (reverse *movels*))


;(if (member (substring(symbol->string x) 1 1) ls)
;        (println "match " )                       
;        (println "no match "))

;; used by any procedure except 'cmd-evaluate-move ...
;; those procedures will return a list with (car r1) either #t or #f
(define (process-piece-or-pawn  turn move) ;; move is a SYMBOL
  (println "process-piece-or-pawn: turn " turn " move-> " move
           " move[0] " (substring(symbol->string move) 0 1))
  
  (let ((ls (list "R" "N" "B" "Q" "K" "O"  )))
    (cond ((member (substring(symbol->string move) 0 1) ls)
           (println "process-piece-or-pawn processing a Piece!")
           (process-piece turn   (symbol->string move)))
          (else
           (println "process-piece-or-pawn processing a Pawn!")
           (process-pawn turn (symbol->string move))) )))

;; called by procedure 'cmd-evaluate-move ... String
;; those procedures will return a list with (car r1) either #t or #f
(define (process-piece-or-pawn-evaluate  turn move)
  (println "process-piece-or-pawn-evaluate: turn " turn " move-> " move)

  (let ((pieces (list "R" "N" "B" "Q" "K" "O" ))
        (lower  (list "r" "n" "b" "q" "k"))
        (pawns (list "a" "b" "c" "d" "e" "f" "g" "h")))
    (cond ((member (substring move 0 1) pieces)
           (println "process-piece-or-pawn-eval processing a Piece!")
           (process-piece turn  move))

          ((and (member (substring move 0 1) lower) (> (string-length move) 5)) ; ex: ng8xc3
           (let ((s2 (lower-to-upper move)))
             (println "process-piece-or-pawn-evaluate (converting black move to uppercase!! "
                      (append-strings (list s2 (substring move 1 6))))
           (process-piece turn (append-strings(list s2(substring move 1 6)))) ))
          
          ((member (substring move 0 1) pawns)
            (println "process-piece-or-pawn-eval processing a Pawn!")
            (process-pawn turn move))
          (else
           (println "process-piece-or-pawn-evaluate, Error illegal move -> " move)
           (list #f)) )))

;; MPW testing to return values from process-piece or process-pawn
;; so we can make the move outside those procedures.
;; (list-ref r1 1) is the procedure to call (list-ref r1 7) is the move to make...
(define (pgngame-new file)
  (println "staring, pgngame-new....")
  
  (let (( x (reverse(read-pgn-file file))))
    (let loop ((ls x) (i 1))
      (println "pgngame loop i -> " i " the list -> " ls)
      (cond ((null? ls)
             ;(println "pgngame null ls -> " ls)
            #t)
            (else
             (cond ((= i 1) ;white
                    (println "whites PGN move")
                    (let ((r1 (process-piece-or-pawn 1 (car ls))))
                      (cond ((car r1)
                             (cond ((and (char? (cadr r1)) (char=? (cadr r1) #\c))
                                   (println " MPW castle move, r1 -> " r1)
                                   (loop (cdr ls)(- 1 i)))
                                   ((and (char? (cadr r1)) (char=? (cadr r1) #\e))
                                    (println " MPW white enpassant move, r1 -> " r1)
                                    (loop (cdr ls)(- 1 i)))
                                   (else
                                    (println "** MPW r1 -> " r1)
                                    (cmd-apply-move r1) ; make the move
                                    (loop (cdr ls)(- 1 i)))))
                            (else
                             (println "pgngame: MPW illegal white PGN move!!")
                             #f))))
                   
                   ((= i 0) ;black
                    (println "blacks PGN move")
                    (let ((r1 (process-piece-or-pawn 0 (car ls))))
                      (cond ((car r1)
                             (cond ((and (char? (cadr r1)) (char=? (cadr r1) #\c))
                                   (println "MPW castle move, r1 -> " r1)
                                   (loop (cdr ls)(+ 1 i)))
                                   ((and (char? (cadr r1)) (char=? (cadr r1) #\e))
                                    (println " MPW black enpassant move, r1 -> " r1)
                                    (loop (cdr ls)(+ 1 i)))
                                   (else
                                    (println "** MPW r1 -> " r1)
                                    (cmd-apply-move r1) ; make the move
                                    (loop (cdr ls)(+ 1 i)))))
                            (else
                             (println "pgngame: MPW illegal black PGN move!!")
                             #f))))
                    
                   (else
                    (println "pgngame: bad read of PGN file!!")
                    (loop (cdr ls)(+ 2 i))))) ))))


(pgngame-new "kasparov-1.pgn")
(pgngame-new "testpgn.pgn")
; test for pawn queening
;(pgngame-new-new "queentest.pgn") 

;; interactive functions

(define (readprompt prompt)
  (display prompt)
  (read))



;; We first move a piece via cmd-move (just temp move)  then check to determine
;; if the moved piece could be attacked directly by an opponents piece
;;
;;Note : r1[5] = from r1[6] = to
;; currently evaluates what pieces could capture potential moved piece
;; Note: only look at pieces that have not been captured!!
(define (cmd-evaluate-move r1 turn)
  (println "cmd-evaluate-move in progress.... r1 -> " r1 " turn " turn)
  
   (setboard! (list-ref r1 5)(list-ref r1 6)) ; from -> to
   (db)
      
  (let ((v1 (choose-opposite-vector))
        (temp '())
        (all '())
        (p (open-logfile "chess.out"))
        (opponent-turn '()))

    ;; the current player made a move. determine what opponent pieces
    ;; could capture the current pieces move.
    (if (= turn 0)
        (set! opponent-turn 1)  ; opponent of black
        (set! opponent-turn 0)) ; opponent of white
              
    (do ((i 0(+ 1 i))) ;; opponents Pieces
        ((> i 7)#t)

      ;; piece[3] R, piece[1] a1, x, (r1) b5
      ;; temporarly move piece from old to new to determine if piece could be captured
      (set! temp (list (symbol->string(readlist(vector-ref(v1 turn) i) 3))
                       (symbol->string(readlist(vector-ref(v1 turn) i) 1)) "x"
                       (symbol->string(list-ref r1 4))) )
      (println "MPW Pieces!  temp-> " temp)
      
      (let ((r2 (process-piece-or-pawn-evaluate opponent-turn (apply string-append temp))))
        (if (car r2)
            (begin
              (set! all (cons (list-ref r2 7) all))
              (write r2 p)
              (display "\n" p)
              (write all p)
              (display "\n")
              (println "(*piece potential capture opponents..) cmd-evalate-move r2-> " r2 " all-> " all))
            (println "cmd-evaluate-move (PIECES) no caputre r2-> " r2)) ))

    (do ((i 8(+ 1 i))) ;; opponents Pawns
        ((> i 15)#t)
      
      (set! temp (list (symbol->string(readlist(vector-ref(v1 turn) i) 1)) "x"
                       (symbol->string(list-ref r1 4))) )  ;; temp is a LIST
      (println " MPW Pawns! temp-> "  temp)
      
      (let ((r2 (process-piece-or-pawn-evaluate opponent-turn (apply string-append temp))))
        (if (car r2)
            (begin
              (set! all (cons (list-ref r2 7) all))
              (write r2 p)
              (display "\n" p)
              (write all p)
              (display "\n")    
              (println "WOLFE!! (PAWNS) cmd-evalate-move r2-> " r2 " all-> " all))
            (println "cmnd-evaluate-move (PAWNs) no capture r2-> " r2)) ))
            
  ;; lets put the piece back to where it came from!
    (setboard! (list-ref r1 6)(list-ref r1 5)) ; to -> from
    (db)   ;; just look at the board....as it was...
    (close-port p)
    all))  ;; 'all is all the pieces (in pgn format) that could attack piece

      
(define (cmd-apply-move r1)
  (println "cmd-apply-move r1 -> " r1)
  (println "** r1(1) " (list-ref r1 1) " r1(2) " (list-ref r1 2)
           " r1(3) " (list-ref r1 3) " r1(4) " (list-ref r1 4)
           " r1(5) " (list-ref r1 5) " r1(6) " (list-ref r1 6)
           " r1(7) " (list-ref r1 7))
  
  (apply (list-ref r1 1) (list (list-ref r1 2) (list-ref r1 3)
                               (list-ref r1 4)(list-ref r1 5)
                               (list-ref r1 6) (list-ref r1 7))) )


;; if cmd-evalute-move then need to temporarly make the move
;; so evaluate will see a piece on the 'tosq....
(define (cmd-move turn)
  (if (string=? (symbol->string turn) "b")
      (set! turn 0)
      (set! turn 1))
  
  (let ((move (readprompt "enter move "))) ;; move is a SYMBOL
    (println "** cmd-move ** " (string? move)(symbol? move)(list? move)(car(list move)))
    (let ((r1 (process-piece-or-pawn turn  move )))
      (println "cmd-move: returned from process-piece-or-pawn, applying r1 -> " r1 " the move-> " move)

      (let ((rec (lambda (break)
                   (if (not(car r1))
                       (begin
                         (println "cmd-move: illegal move->! " move);
                         (break #f)))  ;; cannot allow an illegal move to continue to process
                   (let loop ((i 0))
                     (println "cmd-move in prompting loop....")
                     (let ((cmd (readprompt "apply,evalute,board,squares,quit move (a,e,db,dbsqs,q) : ")))
                       (cond ((string=? (symbol->string cmd) "a")
                              (cmd-apply-move r1)
                              (loop (+ 1 0)))
                             ((string=? (symbol->string cmd) "e")
                              (println "** Results of evaluate ** "(cmd-evaluate-move r1 turn))
                              (loop (+ 1 0)))
                             ((string=? (symbol->string cmd) "db")
                              (db)
                              (loop (+ 1 0)))
                             ((string=? (symbol->string cmd) "dbsqs")
                              (db-sqs)
                              (loop (+ 1 0)))
                             ((string=? (symbol->string cmd) "q")
                              (break #t))
                             (else
                              (println "wrong input ..")
                              (loop (+ 1 0)))))) )))
        (call/cc rec)))))
    

;; get cmd from user, determine if process-piece or process-pawn is true
(define (interact )
  (let ((rec (lambda (break)
               (let loop ((i 0))
                 (let (( cmd (readprompt "enter b,w,board,squares,quit (b,w,db,dbsqs,q) : ")))
                   (cond ((string=? (symbol->string cmd) "q")
                          (println "quit game...")
                          (break #t))
                         ((string=? (symbol->string cmd) "db")
                          (db)
                          (loop (+ i 0)))
                         ((string=? (symbol->string cmd) "dbsqs")
                          (db-sqs)
                          (loop (+ i 0)))
                         ((string=? (symbol->string cmd) "b")
                          (cmd-move cmd) ; try to make the move (black)
                          (loop (+ i 0)))
                         ((string=? (symbol->string cmd) "w")
                          (cmd-move cmd) ; try to make the move (white)
                          (loop (+ i 0)))
                         (else
                           (println "wrong input..")
                           (loop (+ 1 0)))))) )))
    (call/cc rec)))
                
(interact)


;; reset game with reading vales from files
(define (reset-board)
  (read-board)
  (read-algebra)
  (read-sqs)
  (read-wpieces)
  (read-bpieces)
  (reset-movels))

(reset-board)

;; older procedures!! ...

;; get move from user, determine if process-piece or process-pawn is true
(define (pgngame-old file)

  (let (( x (reverse(read-pgn-file file))))
    (let loop ((ls x) (i 1))
      (cond ((null? ls)
             #t)
            (else
             (cond ((= i 1) ;white
                    (println "whites PGN move")
                    (cond ((or (process-piece 1 (symbol->string(car ls)))
                               (process-pawn 1 (symbol->string(car ls))))
                           (loop (cdr ls)(- 1 i))) ; legal
                          (else
                           (println "illegal white PGN move!!")
                           #f)))
                   ((= i 0) ;black
                    (println "blacks PGN move")
                    (cond ((or (process-piece 0 (symbol->string(car ls)))
                               (process-pawn 0  (symbol->string(car ls))))
                           (loop (cdr ls)(+ 1 i))) ; legal
                          (else
                           (println "illegal black PGN move!!")
                           #f)))
                   (else
                    (println "bad read of PGN file!!")
                    (loop (cdr ls)(+ 2 i)))) )))))


(define (process-piece-or-pawn-old  turn ls)
  (println "process-piece-or-pawn: turn " turn " ls " ls)

  (let ((r1 (process-piece turn (symbol->string(car ls)))))
    (cond ((eq? r1 #t)  ;; just returns either #t or #f
           r1)
          (else
           (process-pawn turn (symbol->string(car ls)))))))

;; MPW testing to return values from process-piece or process-pawn
;; so we can make the move outside those procedures.
(define (pgngame-new-old file)

  (let (( x (reverse(read-pgn-file file))))
    (let loop ((ls x) (i 1))
      (cond ((null? ls)
             #t)
            (else
             (cond ((= i 1) ;white
                    (println "whites PGN move")
                    (let ((r1 (process-piece-or-pawn 1 ls)))
                      (cond ((eq? r1 #t)
                             (loop (cdr ls)(- 1 i)))
                            (else
                             (println "illegal white PGN move!!")
                             #f))))
                   
                   ((= i 0) ;white
                    (println "blacks PGN move")
                    (let ((r1 (process-piece-or-pawn 0 ls)))
                      (cond ((eq? r1 #t)
                             (loop (cdr ls)(+ 1 i)))
                            (else
                             (println "illegal black PGN move!!")
                             #f))))
                    
                   (else
                    (println "bad read of PGN file!!")
                    (loop (cdr ls)(+ 2 i)))) )))))

;; get move from user, determine if process-piece or process-pawn is true
(define (startgame turn)
   (let (( move #t))
     (let loop ((i turn))
       (if(< i 2)
         (cond ((= i 0)  ; black
                (set! move (readprompt "enter black's move: "))
                (cond ((string=? (symbol->string move) "end")
                       (println "black ending game...")
                       (loop (+ 2 i)))
                      ((string=? (symbol->string move) "display-w")
                       (display-wpieces1)
                       (loop (+ 0 i )))
                      ((string=? (symbol->string move) "db")
                       (db)
                       (loop (+ 0 i)))
                       ((string=? (symbol->string move) "dbnum")
                       (db-sqs)
                       (loop (+ 0 i)))
                        ((string=? (symbol->string move) "dbchar")
                       (db-algebra)
                       (loop (+ 0 i)))
                      (else
                       (println "blacks's move -> " move)
                       (if (or (process-piece 0 (symbol->string move))
                               (process-pawn 0 (symbol->string move)))  ; Black's move legal?
                           (loop (+ 1 i))     ; legal
                           (loop (+ 0 i)))) ))
               ((= i 1) ; white
                (set! move (readprompt "enter white's move: "))
                (cond ((string=? (symbol->string move) "end")
                       (println "white ending game...")
                       (loop (+ 2 i)))
                      ((string=? (symbol->string move) "display-b")
                       (display-bpieces1)
                       (loop (+ 0 1 )))
                      ((string=? (symbol->string move) "db")
                       (db)
                       (loop (+ 0 1 )))
                      ((string=? (symbol->string move) "dbnum")
                       (db-sqs)
                       (loop (+ 0 1 )))
                      ((string=? (symbol->string move) "dbchar")
                       (db-algebra)
                       (loop (+ 0 1 )))
                      (else
                       (println "whites's's move -> " move)
                       (if (or (process-piece 1 (symbol->string move))
                               (process-pawn 1 (symbol->string move))) ; Whites's move legal?
                           (loop (- 1 i)) ; legal
                           (loop (+ 0 1)) ))))) ))) #t)



