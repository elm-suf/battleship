;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import ENV ?ALL) (export ?ALL))
; ################################################################
; ########################  TEMPLATES ############################
; ################################################################
(deftemplate guess 
	(slot y)
	(slot x)
	(slot content 
	(allowed-values water left right middle top bot nil sub flag ) (default nil))
	(slot executed)
)

(deftemplate fireable-per-y 
	(slot y)
	(slot count)
)

(deftemplate fireable-per-x 
	(slot x)
	(slot count)
)


(deftemplate free-cells-x
	(slot count)
	(slot x)
)

(deftemplate free-cells-y
	(slot count)
	(slot y)
)

(deftemplate last-guessed
	(slot x)
	(slot y)
	(slot content)
	(slot msg (default "Guess   "))
)

; ################################################################
; ##########################  Rules ##############################
; ################################################################

(defrule guess-occured
	(declare (salience 100))
 	?guess <- (last-guessed (msg ?msg) (x ?x) (y ?y) (content ?t))
	?gx <- (fireable-per-x (x ?x) (count ?cx))
	?gy <- (fireable-per-y (y ?y) (count ?cy))
	?fx <- (free-cells-x (x ?x) (count ?cfx))
	?fy <- (free-cells-y (y ?y) (count ?cfy))	
 => 
	(if (neq ?t  water)
	 then 
		(modify ?gx (count (- ?cx 1)))
		(modify ?gy (count (- ?cy 1)))
	)
	(modify ?fx (count (- ?cfx 1)))
	(modify ?fy (count (- ?cfy 1)))
	(retract ?guess)
	(printout t ?msg "	["  ?x ", " ?y "] " ?t "." crlf)

)

(defrule init-count-y
	(declare (salience 10))
	(k-per-col (col ?r) (num ?n))
	=> 
	(assert(fireable-per-y (y ?r)(count ?n)))
	(assert(free-cells-y (y ?r)(count 10)))
)

(defrule init-fireable-x 
	(declare (salience 10))
	(k-per-row (row ?r) (num ?n))
	=> 
	(assert(fireable-per-x(x ?r) (count ?n)))
	(assert(free-cells-x (x ?r)(count 10)))
)

(defrule k-cells
	(declare (salience 9))
	(k-cell (x ?x) (y ?y) (content ?t))
	?guess <- (guess (x ?x) (y ?y) (content nil))
	=>
	; (printout t "InitialFact:[" ?x ", " ?y "] contains " ?t "." crlf)
	(modify ?guess (content ?t))
	(assert (last-guessed (msg InitialFact) (x ?x) (y ?y) (content ?t)))
)

(defrule all-water-x
	(declare (salience 8))
	(fireable-per-x (x ?x) (count 0))
	?guess <- (guess (x ?x) (y ?y)  (content nil))
 =>
	(modify ?guess (content water))
	(assert (last-guessed (msg Guess-XX) (x ?x) (y ?y) (content water)))
	; (printout t "Guess-X [" ?x ", " ?y "]" crlf)
)

(defrule all-water-y
	(declare (salience 8))
	(fireable-per-y (y ?y) (count 0))
	?guess <- (guess (x ?x) (y ?y)  (content nil))
 =>
	(modify ?guess (content water))
	(assert (last-guessed (msg Guess-YY) (x ?x) (y ?y) (content water)))
)

; ==================================

(defrule guessed-some-tl
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y2&=(- ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-Tl) (x ?x2) (y ?y2) (content water)))
)

(defrule guessed-some-bl
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y2&=(- ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-BL) (x ?x2) (y ?y2) (content water)))
)

(defrule guessed-some-tr
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y2&=(+ ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-TR) (x ?x2) (y ?y2) (content water)))
)

(defrule guessed-some-br
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y2&=(+ ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
 	(assert (last-guessed (msg Guess-BR) (x ?x2) (y ?y2) (content water)))
)

(defrule guessed-some-tp
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y))
	(test (or (eq ?c sub) (eq ?c top) (eq ?c right) (eq ?c left)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-TP) (x ?x2) (y ?y) (content water)))
)

(defrule guessed-some-bt
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y))
	(test (or (eq ?c sub) (eq ?c bot) (eq ?c right) (eq ?c left)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-BT) (x ?x2) (y ?y) (content water)))
)

(defrule guessed-some-lf
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x ) (y ?y2&=(- ?y 1)))
	(test (or (eq ?c sub) (eq ?c top) (eq ?c left) (eq ?c bot)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-LF) (x ?x) (y ?y2) (content water)))
)

(defrule guessed-some-rt
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x ) (y ?y2&=(+ ?y 1)))
	(test (or (eq ?c sub) (eq ?c top) (eq ?c right) (eq ?c bot)))
 =>
	(modify ?gg (content water))
	(assert (last-guessed (msg Guess-RT) (x ?x) (y ?y2) (content water)))
)

; ===========
(defrule guess-flag-left
	(guess (content left) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x) (y ?y2&=(+ ?y 1)))
 =>
	(modify ?gg (content flag))
	(assert (last-guessed  (x ?x) (y ?y2) (content flag)))
)

(defrule guess-flag-right
	(guess (content right) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x) (y ?y2&=(- ?y 1)))
 =>
	(modify ?gg (content flag))
	(assert (last-guessed  (x ?x) (y ?y2) (content flag)))
)

(defrule guess-flag-bot
	(guess (content bot) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y))
 =>
	(modify ?gg (content flag))
	(assert (last-guessed  (x ?x2) (y ?y) (content flag)))
)

(defrule guess-flag-top
	(guess (content top) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y))
 =>
	(modify ?gg (content flag))
	(assert (last-guessed  (x ?x2) (y ?y) (content flag)))
)

(defrule guess-remaining-cells-x
	(declare (salience 7))

	(fireable-per-x (x ?x) (count ?count))
	(free-cells-x (x ?x) (count ?count))
	?gg <- (guess (x ?x) (y ?y) (content nil))
 =>
 	(modify ?gg (content flag))
 	(assert (last-guessed (msg Remaining-X)(x ?x) (y ?y) (content flag)))	
)

(defrule guess-remaining-cells-y
	(declare (salience 7))
	(fireable-per-y (y ?y) (count ?count))
	(free-cells-y (y ?y) (count ?count))
	?gg <- (guess (x ?x) (y ?y) (content nil))
 =>
 	(modify ?gg (content flag))
 	(assert (last-guessed (msg Remaining-Y)(x ?x) (y ?y) (content flag)))	
)


(defrule guess-middle-hor
	?gg <- (guess (content flag) (x ?x) (y ?y))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x) (y ?y1&=(+ ?y 1)))
	(guess (content ?c2&:(and (neq ?c2 water) (neq ?c2 nil))) (x ?x) (y ?y2&=(- ?y 1)))
 =>
	(modify ?gg (content middle))	
	(printout t "Middle-H	["  ?x ", " ?y "] " middle "." crlf)
)


(defrule guess-middle-ver
	?gg <- (guess (content flag) (x ?x) (y ?y))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x1&=(+ ?x 1)) (y ?y))
	(guess (content ?c2&:(and (neq ?c2 water) (neq ?c2 nil))) (x ?x2&=(- ?x 1)) (y ?y))
 =>
	(modify ?gg (content middle))	
	(printout t "Middle-V	["  ?x ", " ?y "] " middle "." crlf)
)

(defrule guess-top-1
	?gg <- (guess (content flag) (x ?x) (y ?y))
	(guess (content ?c2&:(eq ?c2 water)) (x ?x2&=(- ?x 1)) (y ?y))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x1&=(+ ?x 1)) (y ?y))
 =>
	(modify ?gg (content top))	
	(printout t "TOP		["  ?x ", " ?y "] " top "." crlf)

)

(defrule guess-top-2
	?gg <- (guess (content flag) (x 0) (y ?y))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x 1) (y ?y))
 =>
	(modify ?gg (content top))
	(printout t "TOP		["  0 ", " ?y "] " top "." crlf)
)

(defrule guess-bot-1
	?gg <- (guess (content flag) (x ?x) (y ?y))
	(guess (content ?c2&:(eq ?c2 water)) (x ?x2&=(+ ?x 1)) (y ?y))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x1&=(- ?x 1)) (y ?y))
 =>
	(modify ?gg (content bot))	
	(printout t "BOT	["  ?x ", " ?y "] " bot "." crlf)

)

(defrule guess-bot-2
	?gg <- (guess (content flag) (x 9) (y ?y))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x 8) (y ?y))
 =>
	(modify ?gg (content bot))
	(printout t "BOT-2	["  9 ", " ?y "] " bot "." crlf)
)
; =====

(defrule guess-left-1
	?gg <- (guess (content flag) (x ?x) (y ?y))
	(guess (content ?c2&:(eq ?c2 water)) (x ?x) (y ?y1&=(- ?y 1)))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x) (y ?y2&=(+ ?y 1)))
 =>
	(modify ?gg (content left))	
	(printout t "LEFT		["  ?x ", " ?y "] " left "." crlf)

)

(defrule guess-left-2
	?gg <- (guess (content flag) (x ?x) (y 0))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x) (y 1))
 =>
	(modify ?gg (content left))
	(printout t "LEFT		["?x", 0] left ." crlf)
)


(defrule guess-right-1
	?gg <- (guess (content flag) (x ?x) (y ?y))
	(guess (content ?c2&:(eq ?c2 water)) (x ?x) (y ?y1&=(+ ?y 1)))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x) (y ?y2&=(- ?y 1)))
 =>
	(modify ?gg (content right))	
	(printout t "RIGHT		["  ?x ", " ?y "] " right "." crlf)

)

(defrule guess-right-2
	?gg <- (guess (content flag) (x ?x) (y 9))
    (guess (content ?c1&:(and (neq ?c1 water) (neq ?c1 nil))) (x ?x) (y 8))
 =>
	(modify ?gg (content right))
	(printout t "RIGHT		["?x", 9] right ." crlf)
)




(defrule k-middle-guess-r-l
(declare (salience 100))
	(guess (content middle) (x ?x) (y ?y))
	(guess (content water)  (x ?x2&=(or (+ ?x 1)(- ?x 1))) (y ?y))
	?right  <- (guess (content ?cright) (x  ?x) (y ?yright&=(+ ?y 1)))
	?left   <- (guess (content ?cleft ) (x  ?x) (y  ?yleft&=(- ?y 1)))
 =>
 	(printout t "WTF:[" ?x ", " ?y "]" crlf)
	(printout t "WWTTFF:[" ?x2 ", " ?y "]" crlf)
	(if (eq ?cright nil) then 
		(modify ?right (content flag))
		(assert (last-guessed (msg Guess-K_MID) (x ?x) (y ?yright) (content flag)))
	)
	(if (eq ?cleft nil) then  
		(modify ?left  (content flag))
		(assert (last-guessed (msg Guess-K_MID) (x ?x) (y ?yleft) (content flag)))	
	)
)


(defrule k-middle-guees-r-l-2
	(guess (content middle) (x ?x&:(or (= ?x 0) (= ?x 9))) (y ?y))
	?right  <- (guess (content ?cright) (x  ?x) (y ?yright&=(+ ?y 1)))
	?left   <- (guess (content ?cleft ) (x  ?x) (y  ?yleft&=(- ?y 1)))
 =>
	(if (eq ?cright nil) then 
		(modify ?right (content flag))
		(assert (last-guessed (msg Guess-K-09) (x ?x) (y ?yright) (content flag)))
	)
	(if (eq ?cleft nil) then  
		(modify ?left  (content flag))
		(assert (last-guessed (msg Guess-K-09) (x ?x) (y ?yleft) (content flag)))	
	)
)


(defrule k-middle-guess-t-b
	(guess (content middle) (x ?x) (y ?y))
	(guess (content water)  (x ?x) (y ?y2&=(or (+ ?y 1)(- ?y 1))))
	?bot  <- (guess (content ?cbot) (y  ?y) (x ?xbot&=(+ ?x 1)))
	?top  <- (guess (content ?ctop ) (y  ?y) (x  ?xtop&=(- ?x 1)))
 =>
	(if (eq ?cbot nil) then 
		(modify ?bot (content flag))
		(assert (last-guessed (msg Guess-TB_MID) (x ?xbot) (y ?y) (content flag)))
	)
	(if (eq ?ctop nil) then  
		(modify ?top  (content flag))
		(assert (last-guessed (msg Guess-TB_MID) (x ?xtop) (y ?y) (content flag)))	
	)
)


(defrule k-middle-guess-t-b-1
	(guess (content middle) (y ?y&:(or (= ?y 0) (= ?y 9))) (x ?x))
	?bot  <- (guess (content ?cbot) (y  ?y) (x ?xbot&=(+ ?x 1)))
	?top   <- (guess (content ?ctop ) (y  ?y) (x  ?xtop&=(- ?x 1)))
 =>
	(if (eq ?cbot nil) then 
		(modify ?bot (content flag))
		(assert (last-guessed (msg Guess-TB-09) (x ?xbot) (y ?y) (content flag)))
	)
	(if (eq ?ctop nil) then  
		(modify ?top (content flag))
		(assert (last-guessed (msg Guess-TB-09) (x ?xtop) (y ?y) (content flag)))	
	)
)

(defrule flag-to-sub
	; (declare (salience 10))
	?gg <- (guess (content flag) (x ?x&:(or (= ?x 0) (= ?x 9))) (y ?y&:(or (= ?y 0) (= ?y 9))))
	(guess (content water) (x ?x2&:(or (- ?x 1) (+ ?x 1))) (y ?y))
	(guess (content water) (x ?x) (y ?y2&:(or (- ?y 1) (+ ?y 1))))
 =>
	(modify ?gg (content sub))
	(printout t "SUB	["  ?x ", " ?y "]." crlf)
)


(defrule flag-to-sub-2
	; (declare (salience 10))
	?gg <- (guess (content flag) (x ?x&:(or (= ?x 0) (= ?x 9))) (y ?y))
	(guess (content water) (x ?x2&:(or (- ?x 1) (+ ?x 1))) (y ?y))
	(guess (content water) (x ?x) (y ?y2&=(+ ?y 1)))
	(guess (content water) (x ?x) (y ?y3&=(- ?y 1)))
 =>
	(modify ?gg (content sub))
	(printout t "SUB-1	["  ?x ", " ?y "]." crlf)
)


(defrule flag-to-sub-3
	; (declare (salience 10))
	?gg <- (guess (content flag) (y ?y&:(or (= ?y 0) (= ?y 9))) (x ?x))
	(guess (content water) (y ?y2&:(or (- ?y 1) (+ ?y 1))) (x ?x))
	(guess (content water) (y ?y) (x ?x2&=(+ ?x 1)))
	(guess (content water) (y ?y) (x ?x3&=(- ?x 1)))
 =>
	(modify ?gg (content sub))
	(printout t "SUB-3	["  ?x ", " ?y "]." crlf)
)


(defrule flag-sub-4
	?gg <- (guess (content flag) (y ?y) (x ?x))
	(guess (content water) (x ?x1&=(+ ?x 1)) (y ?y))
	(guess (content water) (x ?x2&=(- ?x 1)) (y ?y))
	(guess (content water) (x ?x) (y ?y1&=(+ ?y 1)))
	(guess (content water) (x ?x) (y ?y2&=(- ?y 1)))
 =>
	(modify ?gg (content sub))
	(printout t "SUB-4	["  ?x ", " ?y "]." crlf)
)


(defrule exec-guess	
	(declare (salience 100))
	(status (step ?s) (currently running))
	?gg <- (guess (x ?x) (y ?y) (content flag) (executed false))
 =>
 	(printout t "FIRE:[" ?x ", " ?y "]" crlf)
	(modify ?gg (executed true))
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
	(pop-focus)
)

(defrule exec-solve	
	(declare (salience -1))
	(status (step ?s) (currently running))
 =>
 	(printout t "SOLVE!!" ?s crlf)
	(assert (exec (step ?s) (action solve)))
	(pop-focus)
)



(deffacts agent-facts
	(guess (y 0) (x 0) (content nil) (executed false))
	(guess (y 0) (x 1) (content nil) (executed false))
	(guess (y 0) (x 2) (content nil) (executed false))
	(guess (y 0) (x 3) (content nil) (executed false))
	(guess (y 0) (x 4) (content nil) (executed false))
	(guess (y 0) (x 5) (content nil) (executed false))
	(guess (y 0) (x 6) (content nil) (executed false))
	(guess (y 0) (x 7) (content nil) (executed false))
	(guess (y 0) (x 8) (content nil) (executed false))
	(guess (y 0) (x 9) (content nil) (executed false))
	(guess (y 1) (x 0) (content nil) (executed false))
	(guess (y 1) (x 1) (content nil) (executed false))
	(guess (y 1) (x 2) (content nil) (executed false))
	(guess (y 1) (x 3) (content nil) (executed false))
	(guess (y 1) (x 4) (content nil) (executed false))
	(guess (y 1) (x 5) (content nil) (executed false))
	(guess (y 1) (x 6) (content nil) (executed false))
	(guess (y 1) (x 7) (content nil) (executed false))
	(guess (y 1) (x 8) (content nil) (executed false))
	(guess (y 1) (x 9) (content nil) (executed false))
	(guess (y 2) (x 0) (content nil) (executed false))
	(guess (y 2) (x 1) (content nil) (executed false))
	(guess (y 2) (x 2) (content nil) (executed false))
	(guess (y 2) (x 3) (content nil) (executed false))
	(guess (y 2) (x 4) (content nil) (executed false))
	(guess (y 2) (x 5) (content nil) (executed false))
	(guess (y 2) (x 6) (content nil) (executed false))
	(guess (y 2) (x 7) (content nil) (executed false))
	(guess (y 2) (x 8) (content nil) (executed false))
	(guess (y 2) (x 9) (content nil) (executed false))
	(guess (y 3) (x 0) (content nil) (executed false))
	(guess (y 3) (x 1) (content nil) (executed false))
	(guess (y 3) (x 2) (content nil) (executed false))
	(guess (y 3) (x 3) (content nil) (executed false))
	(guess (y 3) (x 4) (content nil) (executed false))
	(guess (y 3) (x 5) (content nil) (executed false))
	(guess (y 3) (x 6) (content nil) (executed false))
	(guess (y 3) (x 7) (content nil) (executed false))
	(guess (y 3) (x 8) (content nil) (executed false))
	(guess (y 3) (x 9) (content nil) (executed false))
	(guess (y 4) (x 0) (content nil) (executed false))
	(guess (y 4) (x 1) (content nil) (executed false))
	(guess (y 4) (x 2) (content nil) (executed false))
	(guess (y 4) (x 3) (content nil) (executed false))
	(guess (y 4) (x 4) (content nil) (executed false))
	(guess (y 4) (x 5) (content nil) (executed false))
	(guess (y 4) (x 6) (content nil) (executed false))
	(guess (y 4) (x 7) (content nil) (executed false))
	(guess (y 4) (x 8) (content nil) (executed false))
	(guess (y 4) (x 9) (content nil) (executed false))
	(guess (y 5) (x 0) (content nil) (executed false))
	(guess (y 5) (x 1) (content nil) (executed false))
	(guess (y 5) (x 2) (content nil) (executed false))
	(guess (y 5) (x 3) (content nil) (executed false))
	(guess (y 5) (x 4) (content nil) (executed false))
	(guess (y 5) (x 5) (content nil) (executed false))
	(guess (y 5) (x 6) (content nil) (executed false))
	(guess (y 5) (x 7) (content nil) (executed false))
	(guess (y 5) (x 8) (content nil) (executed false))
	(guess (y 5) (x 9) (content nil) (executed false))
	(guess (y 6) (x 0) (content nil) (executed false))
	(guess (y 6) (x 1) (content nil) (executed false))
	(guess (y 6) (x 2) (content nil) (executed false))
	(guess (y 6) (x 3) (content nil) (executed false))
	(guess (y 6) (x 4) (content nil) (executed false))
	(guess (y 6) (x 5) (content nil) (executed false))
	(guess (y 6) (x 6) (content nil) (executed false))
	(guess (y 6) (x 7) (content nil) (executed false))
	(guess (y 6) (x 8) (content nil) (executed false))
	(guess (y 6) (x 9) (content nil) (executed false))
	(guess (y 7) (x 0) (content nil) (executed false))
	(guess (y 7) (x 1) (content nil) (executed false))
	(guess (y 7) (x 2) (content nil) (executed false))
	(guess (y 7) (x 3) (content nil) (executed false))
	(guess (y 7) (x 4) (content nil) (executed false))
	(guess (y 7) (x 5) (content nil) (executed false))
	(guess (y 7) (x 6) (content nil) (executed false))
	(guess (y 7) (x 7) (content nil) (executed false))
	(guess (y 7) (x 8) (content nil) (executed false))
	(guess (y 7) (x 9) (content nil) (executed false))
	(guess (y 8) (x 0) (content nil) (executed false))
	(guess (y 8) (x 1) (content nil) (executed false))
	(guess (y 8) (x 2) (content nil) (executed false))
	(guess (y 8) (x 3) (content nil) (executed false))
	(guess (y 8) (x 4) (content nil) (executed false))
	(guess (y 8) (x 5) (content nil) (executed false))
	(guess (y 8) (x 6) (content nil) (executed false))
	(guess (y 8) (x 7) (content nil) (executed false))
	(guess (y 8) (x 8) (content nil) (executed false))
	(guess (y 8) (x 9) (content nil) (executed false))
	(guess (y 9) (x 0) (content nil) (executed false))
	(guess (y 9) (x 1) (content nil) (executed false))
	(guess (y 9) (x 2) (content nil) (executed false))
	(guess (y 9) (x 3) (content nil) (executed false))
	(guess (y 9) (x 4) (content nil) (executed false))
	(guess (y 9) (x 5) (content nil) (executed false))
	(guess (y 9) (x 6) (content nil) (executed false))
	(guess (y 9) (x 7) (content nil) (executed false))
	(guess (y 9) (x 8) (content nil) (executed false))
	(guess (y 9) (x 9) (content nil) (executed false))
)