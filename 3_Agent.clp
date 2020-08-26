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
	(slot content (allowed-values water left right middle top bot nil sub ) (default nil))
)

(deftemplate fireable-per-y 
	(slot y)
	(slot count)
)

(deftemplate fireable-per-x 
	(slot x)
	(slot count)
)


; ################################################################
; ##########################  Rules ##############################
; ################################################################

(defrule init-fireable-y
	; (declare (salience 10))
	(k-per-col (col ?r) (num ?n))
	=> 
	(assert(fireable-per-y (y ?r)(count ?n)))
)

(defrule init-fireable-x 
	; (declare (salience 10))
	(k-per-row (row ?r) (num ?n))
	=> 
	(assert(fireable-per-x(x ?r) (count ?n)))
)

(defrule k-cells
	(k-cell (x ?x) (y ?y) (content ?t))
	?guess <- (guess (x ?x) (y ?y) (content nil))
	?gx <- (fireable-per-x (x ?x) (count ?cx))
	?gy <- (fireable-per-y (y ?y) (count ?cy))
	=>
	(modify ?guess (content ?t))
	(modify ?gx (count (- ?cx 1)))
	(modify ?gy (count (- ?cy 1)))

)

(defrule all-water-x
	(fireable-per-x (x ?x) (count 0))
	?guess <- (guess (x ?x) (y ?y)  (content nil))
 =>
	(modify ?guess (content water))
	(printout t "Guess-X [" ?x ", " ?y "]" crlf)
)

(defrule all-water-y
	(fireable-per-y (y ?y) (count 0))
	?guess <- (guess (x ?x) (y ?y)  (content nil))
 =>
	(modify ?guess (content water))
	(printout t "Guess-Y [" ?x ", " ?y "]" crlf)
)

; ==================================

(defrule guessed-some-tl
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y2&=(- ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(printout t "Guess TL ["  ?x2 ", " ?y2 "]" crlf)
)

(defrule guessed-some-tr
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y2&=(- ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(printout t "Guess TR ["  ?x2 ", " ?y2 "]" crlf)
)

(defrule guessed-some-bl
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y2&=(+ ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(printout t "Guess BL ["  ?x2 ", " ?y2 "]" crlf)
)

(defrule guessed-some-br
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y2&=(+ ?y 1)))
	(test (and (neq ?c water) (neq ?c nil)))
 =>
	(modify ?gg (content water))
	(printout t "Guess BR  ["  ?x2 ", " ?y2 "]" crlf)
)

(defrule guessed-some-tp
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(- ?x 1)) (y ?y))
	(test (or (eq ?c sub) (eq ?c top) (eq ?c right) (eq ?c left)))
 =>
	(modify ?gg (content water))
	(printout t "Guess TP ["  ?x2 ", " ?y "]" crlf)
)

(defrule guessed-some-bt
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x2&=(+ ?x 1)) (y ?y))
	(test (or (eq ?c sub) (eq ?c bot) (eq ?c right) (eq ?c left)))
 =>
	(modify ?gg (content water))
	(printout t "Guess BT ["  ?x2 ", " ?y "]" crlf)
)

(defrule guessed-some-lf
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x ) (y ?y2&=(- ?y 1)))
	(test (or (eq ?c sub) (eq ?c top) (eq ?c right) (eq ?c bot)))
 =>
	(modify ?gg (content water))
	(printout t "Guess LF ["  ?x ", " ?y2 "]" crlf)
)

(defrule guessed-some-rt
	(guess (content ?c) (x ?x) (y ?y))
	?gg <- (guess (content nil) (x ?x ) (y ?y2&=(+ ?y 1)))
	(test (or (eq ?c sub) (eq ?c top) (eq ?c left) (eq ?c bot)))
 =>
	(modify ?gg (content water))
	(printout t "Guess RT ["  ?x ", " ?y2 "]" crlf)
)


(defrule print-what-i-know-since-the-beginning
	(k-cell (x ?x) (y ?y) (content ?t) )
 =>
	(printout t "I know that cell [" ?x ", " ?y "] contains " ?t "." crlf)
)

(deffacts agent-facts
	(guess (y 0) (x 0) (content nil))
	(guess (y 0) (x 1) (content nil))
	(guess (y 0) (x 2) (content nil))
	(guess (y 0) (x 3) (content nil))
	(guess (y 0) (x 4) (content nil))
	(guess (y 0) (x 5) (content nil))
	(guess (y 0) (x 6) (content nil))
	(guess (y 0) (x 7) (content nil))
	(guess (y 0) (x 8) (content nil))
	(guess (y 0) (x 9) (content nil))
	(guess (y 1) (x 0) (content nil))
	(guess (y 1) (x 1) (content nil))
	(guess (y 1) (x 2) (content nil))
	(guess (y 1) (x 3) (content nil))
	(guess (y 1) (x 4) (content nil))
	(guess (y 1) (x 5) (content nil))
	(guess (y 1) (x 6) (content nil))
	(guess (y 1) (x 7) (content nil))
	(guess (y 1) (x 8) (content nil))
	(guess (y 1) (x 9) (content nil))
	(guess (y 2) (x 0) (content nil))
	(guess (y 2) (x 1) (content nil))
	(guess (y 2) (x 2) (content nil))
	(guess (y 2) (x 3) (content nil))
	(guess (y 2) (x 4) (content nil))
	(guess (y 2) (x 5) (content nil))
	(guess (y 2) (x 6) (content nil))
	(guess (y 2) (x 7) (content nil))
	(guess (y 2) (x 8) (content nil))
	(guess (y 2) (x 9) (content nil))
	(guess (y 3) (x 0) (content nil))
	(guess (y 3) (x 1) (content nil))
	(guess (y 3) (x 2) (content nil))
	(guess (y 3) (x 3) (content nil))
	(guess (y 3) (x 4) (content nil))
	(guess (y 3) (x 5) (content nil))
	(guess (y 3) (x 6) (content nil))
	(guess (y 3) (x 7) (content nil))
	(guess (y 3) (x 8) (content nil))
	(guess (y 3) (x 9) (content nil))
	(guess (y 4) (x 0) (content nil))
	(guess (y 4) (x 1) (content nil))
	(guess (y 4) (x 2) (content nil))
	(guess (y 4) (x 3) (content nil))
	(guess (y 4) (x 4) (content nil))
	(guess (y 4) (x 5) (content nil))
	(guess (y 4) (x 6) (content nil))
	(guess (y 4) (x 7) (content nil))
	(guess (y 4) (x 8) (content nil))
	(guess (y 4) (x 9) (content nil))
	(guess (y 5) (x 0) (content nil))
	(guess (y 5) (x 1) (content nil))
	(guess (y 5) (x 2) (content nil))
	(guess (y 5) (x 3) (content nil))
	(guess (y 5) (x 4) (content nil))
	(guess (y 5) (x 5) (content nil))
	(guess (y 5) (x 6) (content nil))
	(guess (y 5) (x 7) (content nil))
	(guess (y 5) (x 8) (content nil))
	(guess (y 5) (x 9) (content nil))
	(guess (y 6) (x 0) (content nil))
	(guess (y 6) (x 1) (content nil))
	(guess (y 6) (x 2) (content nil))
	(guess (y 6) (x 3) (content nil))
	(guess (y 6) (x 4) (content nil))
	(guess (y 6) (x 5) (content nil))
	(guess (y 6) (x 6) (content nil))
	(guess (y 6) (x 7) (content nil))
	(guess (y 6) (x 8) (content nil))
	(guess (y 6) (x 9) (content nil))
	(guess (y 7) (x 0) (content nil))
	(guess (y 7) (x 1) (content nil))
	(guess (y 7) (x 2) (content nil))
	(guess (y 7) (x 3) (content nil))
	(guess (y 7) (x 4) (content nil))
	(guess (y 7) (x 5) (content nil))
	(guess (y 7) (x 6) (content nil))
	(guess (y 7) (x 7) (content nil))
	(guess (y 7) (x 8) (content nil))
	(guess (y 7) (x 9) (content nil))
	(guess (y 8) (x 0) (content nil))
	(guess (y 8) (x 1) (content nil))
	(guess (y 8) (x 2) (content nil))
	(guess (y 8) (x 3) (content nil))
	(guess (y 8) (x 4) (content nil))
	(guess (y 8) (x 5) (content nil))
	(guess (y 8) (x 6) (content nil))
	(guess (y 8) (x 7) (content nil))
	(guess (y 8) (x 8) (content nil))
	(guess (y 8) (x 9) (content nil))
	(guess (y 9) (x 0) (content nil))
	(guess (y 9) (x 1) (content nil))
	(guess (y 9) (x 2) (content nil))
	(guess (y 9) (x 3) (content nil))
	(guess (y 9) (x 4) (content nil))
	(guess (y 9) (x 5) (content nil))
	(guess (y 9) (x 6) (content nil))
	(guess (y 9) (x 7) (content nil))
	(guess (y 9) (x 8) (content nil))
	(guess (y 9) (x 9) (content nil))
)