;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Module: ops.lisp
;;; different worlds and operators for the GPS planner.
;;; =========================================



(in-package :user)

(defstruct op "An GPS operator"
  (action nil) 
  (preconds nil) 
  (add-list nil) 
  (del-list nil))

(defun executing-p (x)
  "Is x of the form: (execute ...) ?"
  (starts-with x 'execute))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'execute (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))




;;; ================= Pumpkin Pie Recipe ====================
(defparameter *pumpkin-pie-world* '(pie-crust-on-counter ingredients-on-counter small-bowl-on-counter large-bowl-on-counter pie-tin-on-counter in-center-kitchen))

(defparameter *pumpkin-ops*
  (list
    ;;; operator 1
   (make-op :action 'go-to-oven
	    :preconds '(in-center-kitchen)
	    :add-list '(at-oven)
	    :del-list '(in-center-kitchen))

    ;;; operator 2
   (make-op :action 'preheat-oven-425
	    :preconds '(at-oven)
	    :add-list '(oven-preheated-425))

    ;;; operator 3
   (make-op :action 'go-to-counter
	    :preconds '(at-oven oven-preheated-425)
	    :add-list '(at-counter)
	    :del-list '(at-oven))

    ;;; operator 4
   (make-op :action 'put-pie-crust-in-pie-tin
	    :preconds '(pie-crust-on-counter pie-tin-on-counter at-counter)
	    :add-list '(pie-shell-ready))

    ;;; operator 5
   (make-op :action 'add-three-fourths-cup-sugar-to-small-bowl
	    :preconds '(small-bowl-on-counter ingredients-on-counter at-counter pie-shell-ready)
	    :add-list '(sugar-in-bowl))

    ;;; operator 6
   (make-op :action 'add-1-teaspoon-cinnamon-to-small-bowl
	    :preconds '(small-bowl-on-counter ingredients-on-counter at-counter sugar-in-bowl)
	    :add-list '(cinnamon-in-bowl))

    ;;; operator 7
   (make-op :action 'add-one-half-teaspoon-salt-to-small-bowl
	    :preconds '(small-bowl-on-counter ingredients-on-counter at-counter cinnamon-in-bowl)
	    :add-list '(salt-in-bowl))

    ;;; operator 8
   (make-op :action 'add-one-half-teaspoon-ginger-to-small-bowl
	    :preconds '(small-bowl-on-counter ingredients-on-counter at-counter salt-in-bowl)
	    :add-list '(ginger-in-bowl))

    ;;; operator 9
   (make-op :action 'add-one-half-teaspoon-cloves-to-small-bowl
	    :preconds '(small-bowl-on-counter ingredients-on-counter at-counter ginger-in-bowl)
	    :add-list '(cloves-in-bowl))

    ;;; operator 10
   (make-op :action 'mix-ingredients-in-small-bowl
	    :preconds '(small-bowl-on-counter at-counter cloves-in-bowl)
	    :add-list '(small-bowl-ingredients-mixed))

    ;;; operator 11
   (make-op :action 'crack-eggs-in-large-bowl
	    :preconds '(large-bowl-on-counter ingredients-on-counter at-counter small-bowl-ingredients-mixed)
	    :add-list '(eggs-in-large-bowl))

    ;;; operator 12
   (make-op :action 'beat-eggs
	    :preconds '(large-bowl-on-counter ingredients-on-counter at-counter eggs-in-large-bowl)
	    :add-list '(eggs-beaten))

    ;;; operator 13
   (make-op :action 'add-can-pumpkin-to-large-bowl
	    :preconds '(large-bowl-on-counter ingredients-on-counter at-counter eggs-beaten)
	    :add-list '(pumpkin-in-large-bowl))

    ;;; operator 14
   (make-op :action 'add-contents-of-small-bowl-to-large-bowl
	    :preconds '(large-bowl-on-counter at-counter small-bowl-on-counter pumpkin-in-large-bowl small-bowl-ingredients-mixed)
	    :add-list '(spices-in-large-bowl)
	    :del-list '(small-bowl-ingredients-mixed sugar-in-bowl cinnamon-in-bowl salt-in-bowl ginger-in-bowl cloves-in-bowl))

    ;;; operator 15
   (make-op :action 'mix-ingredients-in-large-bowl
	    :preconds '(large-bowl-on-counter at-counter spices-in-large-bowl)
	    :add-list '(pumpkin-and-spices-mixed))

    ;;; operator 16
   (make-op :action 'stir-in-evaporated-milk-in-large-bowl
	    :preconds '(large-bowl-on-counter at-counter pumpkin-and-spices-mixed)
	    :add-list '(pie-filling-ready))

    ;;; operator 17
   (make-op :action 'pour-pie-filling-into-pie-shell
	    :preconds '(large-bowl-on-counter at-counter pie-shell-ready pie-filling-ready)
	    :add-list '(pie-filling-in-pie-shell)
	    :del-list '(spices-in-large-bowl pumpkin-and-spices-mixed pie-filling-ready eggs-in-large-bowl eggs-beaten pumpkin-in-large-bowl))

    ;;; operator 18
   (make-op :action 'hold-uncooked-pie
	    :preconds '(at-counter pie-filling-in-pie-shell)
	    :add-list '(holding-uncooked-pie))

    ;;; operator 19
   (make-op :action 'go-to-oven-with-pie
	    :preconds '(at-counter oven-preheated-425 holding-uncooked-pie)
	    :add-list '(at-oven-with-pie)
	    :del-list '(at-counter))

    ;;; operator 20
   (make-op :action 'place-pie-in-oven-15-minutes
	    :preconds '(at-oven-with-pie oven-preheated-425 holding-uncooked-pie)
	    :add-list '(pie-in-oven-425 15-minutes-past)
	    :del-list '(holding-uncooked-pie))

    ;;; operator 21
   (make-op :action 'reduce-temperature-to-350-degrees
	    :preconds '(15-minutes-past pie-in-oven-425 at-oven-with-pie oven-preheated-425)
	    :add-list '(pie-in-oven-350)
	    :del-list '(pie-in-oven-425 15-minutes-past))

    ;;; operator 22
   (make-op :action 'wait-40-minutes
	    :preconds '(pie-in-oven-350 at-oven-with-pie)
	    :add-list '(40-minutes-past))

    ;;; operator 23
   (make-op :action 'take-pie-out-and-let-rest
	    :preconds '(40-minutes-past pie-in-oven-350 at-oven-with-pie)
	    :add-list '(pie-is-ready)
	    :del-list '(40-minutes-past pie-in-oven-350))
    )
  )


(mapc #'convert-op *pumpkin-ops*)

(provide :ops)
