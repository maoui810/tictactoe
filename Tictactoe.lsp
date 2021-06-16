
;Manpreet Singh Harjot Singh Amna Tiwana 
;game starts with (connect-3) call to function
; play the game when it starts it will ask you for X or O value for user 
;Symbol then AI will take the first turn on the gride next the user 
;will be allowed to enter 0 to 15 values use should only put 0 to 15 numbers otherwise systme will crash 
;once user adds his value the nums grid and gameboard grid is updated to show X or O on board 
;Then minimax decision is called we send gamebaord to it and it makes a copy of orginal gameboard then sends that copy to mini-value fucntion to get the 
;best move for the AI the MAx-value checks best moves for player or 
;which he might take and min checks for ai the functions this goes back and forth putting values on until one of them finds 3 in row first sysbol to put on 3 in row is best value 
;Then if min had higher value than max it will put that at highest priorty but if max has higer value then that means next move AI might lose so it must choose the Max best value 
;At last best value is returned 
;poped from minTempList first value that had 3 in row is returned from minimax decision and in main function it is put on the gamebaord 
;the cycle keeps going until there are no spots on the board or there is a winer at which point the game ends with winer for AI or HUMAN or TIE


(defparameter *playerSym* nil)
(defparameter *playerTurn* 0)
(defparameter *AISymbol* 'O)
(defparameter *win* 0)

(defparameter *ACTUALwinforAI* 0)

(defparameter *actualwincheck* 0)



(setf nums (make-array '(16) :initial-contents '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
;(setq startList(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))


;(format t "~A ~%" startList)

(setq playerMoves (list ))
(setq AIMOVES (list ))

; prints the number gride for guide and prints the tic tac toe board 
(defun initial-state (gameBoard)

(print "USE Number GRID to GUIDE your next MOVE ")

    (format t " ~%")
  (format t "~A ~A ~A ~A ~%" (aref nums 0) (aref nums 1) (aref nums 2) (aref nums 3))
  (format t "~A ~A ~A ~A ~%" (aref nums 4) (aref nums 5) (aref nums 6) (aref nums 7))
  (format t "~A ~A ~A ~A ~%" (aref nums 8) (aref nums 9) (aref nums 10) (aref nums 11))
  (format t "~A ~A ~A ~A ~%" (aref nums 12) (aref nums 13) (aref nums 14) (aref nums 15))
  (format t " ~%")



	(print "Tic tac Toe game Board")

    (format t " ~%")
  (format t "~A ~A ~A ~A ~%" (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard 3))
  (format t "~A ~A ~A ~A ~%" (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard 7))
  (format t "~A ~A ~A ~A ~%" (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard 11))
  (format t "~A ~A ~A ~A ~%" (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard 15))
  (format t " ~%")


)

; lets the symbols for the game for user and AI 
(defun PLAYER (gameBoard)
(print "Symbol you want to represent HUMAN X or O")
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


; Action fuction makes list of avaiable moves and returns that list 
(defun ACTION (gameBoard)
	(setq tempList (list ))
	(dotimes ( i 16)
		(if (equal '_(aref gameBoard i))
			(push i tempList))
			
			
			)
	
	(return-from ACTION tempList)

)

; checks if possition is good returns True for good and nil for false

(defun CHECKPOS(pos gameBoard)
	
	(if (equal '_ (aref gameBoard pos))
		(return-from CHECKPOS t)
		(return-from CHECKPOS nil))
	
	
)





;Terminal Test goes threw the entire actions of the game to see if there are any that match if the do it returns T otherwise nil

(defun TERMINAL-TEST (gameBoard)
	
	                                      
  (or 
  
  (CHECK-TERM (aref gameBoard 0) (aref gameBoard 5) (aref gameBoard 10) *AISymbol*); add these values to the function to see if they in row and check if its same symbol
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 10) (aref gameBoard 15) *AISymbol*)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 9) (aref gameBoard 14) *AISymbol*)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 6) (aref gameBoard 11) *AISymbol*)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 5) (aref gameBoard 8) *AISymbol*)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 6) (aref gameBoard 9) *AISymbol*)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 9) (aref gameBoard 12) *AISymbol*)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 10) (aref gameBoard 13) *AISymbol*)
	  
 
                                       
      (CHECK-TERM (aref gameBoard 0) (aref gameBoard 4) (aref gameBoard 8) *AISymbol*)
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 8) (aref gameBoard 12) *AISymbol*)
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 5) (aref gameBoard 9) *AISymbol*)
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 9) (aref gameBoard 13) *AISymbol*)
      (CHECK-TERM (aref gameBoard 2) (aref gameBoard 6) (aref gameBoard 10) *AISymbol*)
      (CHECK-TERM (aref gameBoard 6) (aref gameBoard 10) (aref gameBoard 14) *AISymbol*)
      (CHECK-TERM (aref gameBoard 3) (aref gameBoard 7) (aref gameBoard 11) *AISymbol*)
      (CHECK-TERM (aref gameBoard 7) (aref gameBoard 11) (aref gameBoard 15) *AISymbol*)
	  
        (CHECK-TERM (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard 2) *AISymbol*) 
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard 3) *AISymbol*)        
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard 6) *AISymbol*)   
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard 7) *AISymbol*) 
      (CHECK-TERM (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard 10) *AISymbol*)
      (CHECK-TERM (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard 11) *AISymbol*)
      (CHECK-TERM (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard 14) *AISymbol*)   
      (CHECK-TERM (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard 15) *AISymbol*)                                 
      
		
		
                                       
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
	  
	  (CHECK-TERM (aref gameBoard 0) (aref gameBoard 1) (aref gameBoard 2) *playerSym*) 
      (CHECK-TERM (aref gameBoard 1) (aref gameBoard 2) (aref gameBoard 3) *playerSym*)        
      (CHECK-TERM (aref gameBoard 4) (aref gameBoard 5) (aref gameBoard 6) *playerSym*)   
      (CHECK-TERM (aref gameBoard 5) (aref gameBoard 6) (aref gameBoard 7) *playerSym*) 
      (CHECK-TERM (aref gameBoard 8) (aref gameBoard 9) (aref gameBoard 10) *playerSym*)
      (CHECK-TERM (aref gameBoard 9) (aref gameBoard 10) (aref gameBoard 11) *playerSym*)
      (CHECK-TERM (aref gameBoard 12) (aref gameBoard 13) (aref gameBoard 14) *playerSym*)   
      (CHECK-TERM (aref gameBoard 13) (aref gameBoard 14) (aref gameBoard 15) *playerSym*)
	  
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



; this function compares 3 values on gride then decides if they are equal if so then updateds win -1 or 1 which untilties uses returns here it sets the win variagble -1 or 1
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

; holds actions for AI to take 
(setq minTemList(list ))


; choose counter move for AI to predicts player next move he will likely take next and  if any moves then returns that value 
(defun MAX-VALUE(gameBoard)
	(setq check 0)
	
	;checks if board is terminal meaning if there are 3 player symbols in row acrross 
	(if (TERMINAL-TEST gameBoard)
		(return-from MAX-VALUE(UTILITY gameBoard)))
	
	(setq act (ACTION gameBoard))
	
	
	
	(setq  TEMPLIST(list ))
	;(setf tempboard (make-array (length gameBoard)))
	;(setq tempboard (copy-seq gameBoard))
	(setq val -100)
	(loop for x in (ACTION gameBoard)
		do()
			(if (not (TERMINAL-TEST gameBoard))
				(setq val (max val (MIN-VALUE(RESULT gameBoard x *playerSym*))))); adds action to results and compares values and retuns max value
			
			(setq bb (list x val))
			(if (eq val -1)
			(push bb temMINHOld)) ;adds action along with its value to list
			
	)
	(setq minTemList TEMPLIST) ; assigns to main list for min action 
	
	(return-from MAX-VALUE val); returns max value
)

(setq maxTemList(list ))

(defun MIN-VALUE(gameBoard)
	
	;(setf tempboard(make-array (length gameBoard)))
	
	
	
	
	(if (TERMINAL-TEST gameBoard)
		(return-from MIN-VALUE(UTILITY gameBoard)))
	
	(setq act (ACTION gameBoard))
	
	;(print tempboard)
	(setq val 100)
	(setq temMINHOld(list )) ; temp list which gest pushed into minTempList
	(loop for x in (ACTION gameBoard)
		do()
			(if (not (TERMINAL-TEST gameBoard))
				(setq val (min val (MAX-VALUE(RESULT gameBoard x *AISymbol*))))) ; finds min value and sets to value using result function
			
			(setq bb (list x val))
			(if (eq val 1)
			(push bb temMINHOld))
			
			
	)
	(setq minTemList temMINHOld)
	;(print val)
	(return-from MIN-VALUE val)
)


; RESULT takes game board and symbol and returns updated temp-board so it adds the action from list and retuns
(defun RESULT (gameBoard action SYM)


	;(setf temp-board(make-array (length gameBoard)))
	;(setq temp-board (copy-seq gameBoard))
	
	(setf (aref gameBoard action) SYM)
	;(initial-state gameBoard)
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


; evealutes the game board and checks to see if its in terminal state if so who won retuns 0 if no one won other wise returns -1 or 1 demedning on symbol of player or AI 
(defun UTILITY(gameBoard)
	
	(setq movesleftifany (ACTION gameBoard))
	
	(cond ( (TERMINAL-TEST gameBoard)
		(return-from UTILITY *win*))
	( (eq movesleftifany nil)
		(return-from UTILITY 0))
	( t(return-from UTILITY 0))
		
	)
	
	
			

)

; this fuction takes the orginal game board and makes a copy then sends taht to MIN-VALUE fuction which returns best value and from minTemList pop the first valid action and returns it
(defun MINIMAX-DECISION (gameBoard)
	;make Copy of orginal game board to test Ai Moves
	
	(print (ACTION gameBoard)); prins the actions still aI can take on board so actions list

	(setf temp-board(make-array (length gameBoard)))
	(setq temp-board (copy-seq gameBoard))
	
	;return best value 
	 (setq a (MIN-VALUE temp-board));Run the MIN-Value Function to return best value
	

	;(print maxTemList) ;printout of best moves for MAX which is player in my case 
	
	
	;(print minTemList) ; print out best moves for AI
	
	(setq bestmove (reverse minTemList ))
	(setq bestmovePLayer (reverse maxTemList ))
	
	
	
	(setq mov (car bestmove))
	(setq maxMoveIF (car bestmovePLayer))
	;(print "HJHHHHHHHHHHHHHHHHHHHHHHH")
	;(setq compareMAXVL (cadr maxMoveIF))
	
	(setq verybestmoveforAI (car mov))
	
	(return-from MINIMAX-DECISION verybestmoveforAI); returns best action for Ai

)


(defun WINNERCHECK(gameBoard)

(setq WHO_WON (UTILITY gameBoard))
(print "END OF THE GAME")

(if (eq WHO_WON 1)
	(print "AI WON THE GAME"))

(if (eq WHO_WON -1)
	(print "HUMAN WON GAME YEEE"))

)



(defun connect-3 (&key (gameBoard (make-array '(16) :initial-contents '(_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)))); makes the array and fills it with inintal values of _

    (initial-state gameBoard) ; Print and Display the Board 
	(if (equal *playerSym* nil) ; Checks if SYMBOLs have been assigned to players if not do so otherwise skip
		(progn
		(setf *playerSym* (PLAYER gameBoard))
		
		))
		
		
	(terpri)	
	(print "CPU Symbol")	
	(print *AISymbol*) ; shows AI symbol
	(terpri)
	(print "Player Symbol")	
	(print *playerSym*) ; Shows player symbol
	;(loop for x from 0 to 15
	;do()
	;(setf (aref gameBoard x) *AISymbol*)

	;)
	
	;loops until someone wins or tie
	(loop for x from 0 to 16
	do()
	
	(setq test1 (TERMINAL-TEST gameBoard));checks terminal if somone won
	(setq test2 (ACTION gameBoard)) ; checks list of avaiable actions on board to take if any
	(if (TERMINAL-TEST gameBoard)
		(WINNERCHECK gameBoard))
	(setq T1 0) 
	(setq T2 0)
	
	
	(if (eq test1 nil) ;cheks if test one is true or false then sets T1 to True or returns from function
		(setq T1 t)
		(return))
		
	
	
	(if (eq test2 nil) ; checks if ther are any moves left on board if true means no more moves and its end of the game as Tie
		(setq T2 nil)
		(setq T2 t))
	
	
	(if (eq T2 nil)
		(progn 
		(print "Game IS OVER NO MORE MOVES LEFT ON BOARD")
		(Print "Game WAS A TIE between AI and HUMAN")
		(return)))
	
	
		
	
	
	(if (eq T1 T2) ; if bother game isnt eneded by terminal and there are moves left keep going with game else return the winner
	(progn
	(setq best_Move_AI (MINIMAX-DECISION gameBoard))
	(setf (aref gameBoard best_Move_AI) *AISymbol*)
	(setf (aref nums best_Move_AI) *AISymbol*)
	
	(initial-state gameBoard)
	
	
	(if (not (TERMINAL-TEST gameBoard))
	(progn
	(setq posistionPlayer (PLAYERMOVECHECK gameBoard))
	(setf (aref gameBoard posistionPlayer) *playerSym*)
	(setf (aref nums posistionPlayer) *playerSym*)
	(print nums)
	(initial-state gameBoard)
	)
	(WINNERCHECK gameBoard)); pritns who won
	))
	
	
	
	
	
	
	); end of loop
	
	
	
    
	); end of fuction 






(connect-3)