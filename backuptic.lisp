(defparameter *playerSym* nil)
(defparameter *playerTurn* 0)
(defparameter *AISymbol* 'O)
(defparameter *win* 0)


(setf nums (make-array '(16) :initial-contents '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
;(setq startList(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))


;(format t "~A ~%" startList)

(setq playerMoves (list ))
(setq AIMOVES (list ))


(defun initial-state (gameBoard)

(print "Numbers on the Graph to use for Tic tac toe")


    (format t " ~%")
  (format t "~a ~a ~a ~a ~%" (aref nums 0) (aref nums 1) (aref nums 2) (aref nums 3))
  (format t "~a ~a ~a ~a ~%" (aref nums 4) (aref nums 5) (aref nums 6) (aref nums 7))
  (format t "~a ~a ~a ~a ~%" (aref nums 8) (aref nums 9) (aref nums 10) (aref nums 11))
  (format t "~a ~a ~a ~a ~%" (aref nums 12) (aref nums 13) (aref nums 14) (aref nums 15))
  (format t " ~%")


(print "Tic tac Toe")

    (format t " ~%")
  (format t "~a ~a ~a ~a ~%" (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard 3))
  (format t "~a ~a ~a ~a ~%" (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard 7))
  (format t "~a ~a ~a ~a ~%" (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard 11))
  (format t "~a ~a ~a ~a ~%" (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard 15))
  (format t " ~%")


)

(defun PLAYER (gameBoard)
	(setq check 0)
	(format t "Enter X or O: ")
	(setq sym (read))
	(if (equal sym 'X)
		(setq check 1))
	
	(if (equal sym 'O)
		(setq check 2 ))
	
	(if (eq check 0)
		(PLAYER gameBoard))
	(if (eq check 2)
		(setf *AISymbol* 'X))
	
	
	(case check
    (1 (return-from PLAYER 'X))
    (2 (return-from PLAYER 'O))
    (3 (return-from PLAYER nil))
    )

	
)



(defun ACTION (gameBoard)
	(setq tempList (list ))
	(dotimes ( i 15)
		(if (equal '_(aref gameBoard i))
			(push i tempList))
			
			
			)
	
	(return-from ACTION tempList)

)


(defun CHECKPOS(pos gameBoard)
	
	(if (equal '_ (aref gameBoard pos))
		(return-from CHECKPOS t)
		(return-from CHECKPOS nil))
	
	
)


(defun Check-Results(gameBoard action SYM)

	                                      
  (or (CHECK-TERM (aref gameBoard action) (aref gameBoard 1) (aref gameBoard 2) SYM)
		(CHECK-TERM (aref gameBoard 0) (aref gameBoard action) (aref gameBoard 2) SYM)
		(CHECK-TERM (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard action) SYM)
		
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 2) (aref gameBoard 3) SYM) 
		(CHECK-TERM (aref gameBoard 1) (aref gameBoard action) (aref gameBoard 3) SYM) 
			(CHECK-TERM (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard action) SYM) 

	  
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 5) (aref gameBoard 6) SYM) 
	  (CHECK-TERM (aref gameBoard 4) (aref gameBoard action) (aref gameBoard 6) SYM) 
	  (CHECK-TERM (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard action) SYM) 
	  
	  
	  
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 6) (aref gameBoard 7) SYM) 
	  (CHECK-TERM (aref gameBoard 5) (aref gameBoard action) (aref gameBoard 7) SYM) 
	  (CHECK-TERM (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard action) SYM)
	  
	  
	  
	  
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 9) (aref gameBoard 10) SYM)
	  (CHECK-TERM (aref gameBoard 8) (aref gameBoard action) (aref gameBoard 10) SYM)
	  (CHECK-TERM (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard action) SYM)
	  
	  
	  
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 10) (aref gameBoard 11) SYM)
	  (CHECK-TERM (aref gameBoard 9) (aref gameBoard action) (aref gameBoard 11) SYM)
	  (CHECK-TERM (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard action) SYM)
	  
	  
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 13) (aref gameBoard 14) SYM)
		(CHECK-TERM (aref gameBoard 12) (aref gameBoard action) (aref gameBoard 14) SYM)
		(CHECK-TERM (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard action) SYM)
		



	  
      (CHECK-TERM (aref gameBoard action) (aref gameBoard 14) (aref gameBoard 15) SYM)
	  (CHECK-TERM (aref gameBoard 13) (aref gameBoard action) (aref gameBoard 15) SYM)
	  (CHECK-TERM (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard action) SYM)
	  
	  
	  
	  
                                        
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 4) (aref gameBoard 8) SYM)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 8) (aref gameBoard 12) SYM)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 5) (aref gameBoard 9) SYM)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 9) (aref gameBoard 13) SYM)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 6) (aref gameBoard 10) SYM)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 10) (aref gameBoard 14) SYM)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 7) (aref gameBoard 11) SYM)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 11) (aref gameBoard 15) SYM)
                                        
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 5) (aref gameBoard 10) SYM)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 10) (aref gameBoard 15) SYM)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 9) (aref gameBoard 14) SYM)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 6) (aref gameBoard 11) SYM)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 5) (aref gameBoard 8) SYM)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 6) (aref gameBoard 9) SYM)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 9) (aref gameBoard 12) SYM)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 10) (aref gameBoard 13) SYM)
		
		
      )
	




)




(defun TERMINAL-TEST (gameBoard)
	
	                                      
  (or (CHECK-TERM (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard 2) *AISymbol*) 
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard 3) *AISymbol*)        
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard 6) *AISymbol*)   
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard 7) *AISymbol*) 
      (CHECK-TERM (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard 10) *AISymbol*)
      (CHECK-TERM (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard 11) *AISymbol*)
      (CHECK-TERM (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard 14) *AISymbol*)   
      (CHECK-TERM (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard 15) *AISymbol*)
                                       
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 4) (aref gameBoard 8) *AISymbol*)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 8) (aref gameBoard 12) *AISymbol*)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 5) (aref gameBoard 9) *AISymbol*)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 9) (aref gameBoard 13) *AISymbol*)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 6) (aref gameBoard 10) *AISymbol*)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 10) (aref gameBoard 14) *AISymbol*)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 7) (aref gameBoard 11) *AISymbol*)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 11) (aref gameBoard 15) *AISymbol*)
                                        
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 5) (aref gameBoard 10) *AISymbol*)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 10) (aref gameBoard 15) *AISymbol*)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 9) (aref gameBoard 14) *AISymbol*)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 6) (aref gameBoard 11) *AISymbol*)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 5) (aref gameBoard 8) *AISymbol*)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 6) (aref gameBoard 9) *AISymbol*)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 9) (aref gameBoard 12) *AISymbol*)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 10) (aref gameBoard 13) *AISymbol*)
		
		(CHECK-TERM (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard 2) *playerSym*) 
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard 3) *playerSym*)        
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard 6) *playerSym*)   
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard 7) *playerSym*) 
      (CHECK-TERM (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard 10) *playerSym*)
      (CHECK-TERM (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard 11) *playerSym*)
      (CHECK-TERM (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard 14) *playerSym*)   
      (CHECK-TERM (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard 15) *playerSym*)
                                       
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 4) (aref gameBoard 8) *playerSym*)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 8) (aref gameBoard 12) *playerSym*)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 5) (aref gameBoard 9) *playerSym*)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 9) (aref gameBoard 13) *playerSym*)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 6) (aref gameBoard 10) *playerSym*)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 10) (aref gameBoard 14) *playerSym*)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 7) (aref gameBoard 11) *playerSym*)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 11) (aref gameBoard 15) *playerSym*)
                                        
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 5) (aref gameBoard 10) *playerSym*)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 10) (aref gameBoard 15) *playerSym*)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 9) (aref gameBoard 14) *playerSym*)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 6) (aref gameBoard 11) *playerSym*)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 5) (aref gameBoard 8) *playerSym*)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 6) (aref gameBoard 9) *playerSym*)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 9) (aref gameBoard 12) *playerSym*)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 10) (aref gameBoard 13) *playerSym*) 
      )
	


)










(defun PLAYERMOVECHECK(gameBoard)

(terpri)
    (princ "Enter Value from 0 to 15 for posistion on gameBoard: ")
    (setq pos (read))
    ;(defvar *name* (read))
	(setq answer(CHECKPOS pos gameBoard))
	(if answer
		(push pos playerMoves))
		
	(if answer
		(return-from PLAYERMOVECHECK pos)
			(PLAYERMOVECHECK gameBoard))
)


(defun MINIMAX-DECISION (gameBoard)

	


)

(defun CHECK-TERM(H1 H2 H3 SYM)

	(setq check1 0)
	(setq check2 0)
	(setq check3 0)
	(setq check4 0)
	(setq check5 0)
	(setq playerC 0)
	(if (eq H1 H2)
		(setq check1 1))
		
	(if (eq H1 H3)
		(setq Check2 1))
		
	(if (eq H1 SYM)
		(setq Check3 1))
	
	(if (eq H2 SYM)
		(setq check4 1))
	
	(if (eq H3 SYM)
		(setq check5 1))
	
	(setq total (+ check1 check2 check3 check4 check5))
	
	(setq total2 0)
	(if (eq SYM *playerSym*)
		(setq total2 5))
	
	
		
	
		
	
	(if (eq total total2)
			(setf *win* -1)
			(setf *win* 1))
			

		
		
		
	
	
	
	(if (eq total 5)
		(return-from CHECK-TERM t)
		(return-from CHECK-TERM nil))
	

)


(setq minTemList(list ))



(defun MAX-VALUE(gameBoard)
	(print "Max after term")
	(if (TERMINAL-TEST gameBoard)
		(return-from MAX-VALUE(UTILITY gameBoard)))
	
	(print "Max after term")
	(setq act (ACTION gameBoard))
	;(setf tempboard (make-array (length gameBoard)))
	;(setq tempboard (copy-seq gameBoard))
	(setq val -100)
	(loop for x in act
		do()
			(setq val (max val (MIN-VALUE(RESULT gameBoard x *AISymbol*))))
			(if 
	)
	(print "makxxxx  value ")
	(print val)
	(return-from MAX-VALUE val)
)
)

(setq maxTemList(list ))

(defun MIN-VALUE(gameBoard)
	
	;(setf tempboard(make-array (length gameBoard)))
	(print "Made it here")
	(if (TERMINAL-TEST gameBoard)
		(return-from MIN-VALUE(UTILITY gameBoard)))
	
	(setq act (ACTION gameBoard))
	
	
	
	(print "Inside MIN")
	;(setq tempboard (copy-seq gameBoard))
	(print "board copied")
	;(print tempboard)
	(setq val 100)
	
	(loop for x in act
		do()
			(print x)
			(setq val (min val (MAX-VALUE(RESULT gameBoard x *playerSym*))))
				
	)
	(print val)
	(return-from MIN-VALUE val)
)


(defun RESULT (gameBoard action SYM)


	;(setf temp-board(make-array (length gameBoard)))
	;(setq temp-board (copy-seq gameBoard))
	
	(setf (aref gameBoard action) SYM)
	(initial-state gameBoard)
	;(initial-state temp-board)
	;(print "UTILITY Value returned ")
	;(setq val (UTILITY temp-board))
	;(if (eq val 1)
		;(push action maxTemList))
	;(if (eq val -1)
		;(push action minTemList))
	
	;(return-from RESULT val)
	
	(return-from RESULT gameBoard)


	)



(defun UTILITY(gameBoard)
	
	(setq movesleftifany (ACTION gameBoard))
	
	(cond ( (TERMINAL-TEST gameBoard)
		(return-from UTILITY *win*))
	( (eq movesleftifany nil)
		(return-from UTILITY 0))
	( t(return-from UTILITY 0))
		
	)
	
	
			

)





(defun connect-3 (&key (gameBoard (make-array '(16) :initial-contents '(_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _))))

	(setq tem (ACTION gameBoard))
	(print tem)
    (initial-state gameBoard)
	(if (equal *playerSym* nil)
		(progn
		(setf *playerSym* (PLAYER gameBoard))
		
		))
	(terpri)	
	(print "CPU Symbol")	
	(print *AISymbol*)
	(terpri)
	(print "Player Symbol")	
	(print *playerSym*)
	
	(setq posistionPlayer (PLAYERMOVECHECK gameBoard))

	
	(setf (aref gameBoard posistionPlayer) *playerSym*)
	(setf (aref nums posistionPlayer) *playerSym*)
	;(setf (aref gameBoard 0) *playerSym*)
	;(setf (aref nums 0) *playerSym*)
	
	;(setf (aref gameBoard 1) *playerSym*)
	;(setf (aref nums 1) *playerSym*)
	
	
	;(setf (aref gameBoard 3) *AISymbol*)
	;(setf (aref nums 3) *AISymbol*)
	(print (ACTION gameBoard))

	
	;(print (CHECKPOS 8 gameBoard))
	;(setf (aref gameBoard 0) *AISymbol*)
	;(setf (aref gameBoard 5) *AISymbol*)
	;(setf (aref gameBoard 10) *AISymbol*)

	(print(TERMINAL-TEST gameBoard))
	(print "UTILITY")
	(print (UTILITY gameBoard))
	;(setf (aref nums checked) *playerSym*)
	(setf (aref gameBoard 15) *AISymbol*)
	(setf (aref nums 15) *AISymbol*)
	(setf (aref gameBoard 14) *AISymbol*)
	(setf (aref nums 14) *AISymbol*)
	;(setf (aref gameBoard 14) *AISymbol*)
	;(setf (aref nums 14) *AISymbol*)
	;(setf (aref gameBoard 13) *AISymbol*)
	;(setf (aref nums 13) *AISymbol*)
	(print(TERMINAL-TEST gameBoard))
	(print "UTILITY------------2")
	(print (UTILITY gameBoard))
	(setf temp-board(make-array (length gameBoard)))
	(setq temp-board (copy-seq gameBoard))
	
	(print "best value")
	 (print(MIN-VALUE temp-board))
	
	;(print "TEMP LIST")
	;(print maxTemList)
	;(setq vall (car maxTemList))
	;(setf (aref gameBoard vall) *AISymbol*)
	;(print nums)
	;(initial-state gameBoard)
	;(print (CHECKPOS 8 gameBoard))
	
	;(setq checked2 (ACTION gameBoard))
	;(print (CHECKPOS 8 gameBoard))
	;(setf (aref gameBoard checked2) *playerSym*)
	;(setf (aref nums checked2) *playerSym*)
	
	(print nums)
	(initial-state gameBoard)
	;(print (CHECKPOS 8 gameBoard))
	
	
    
	)






(connect-3)