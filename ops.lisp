;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Module: ops.lisp
;;; different worlds and operators for the GPS planner.
;;; =========================================

;;; =========================================
;;; blocks-ops plan 1 (a-on-b b-on-c)
;;; (START)
;;; (EXECUTE PUT-C-FROM-A-ON-T)
;;; (EXECUTE PUT-B-FROM-T-ON-C)
;;; (EXECUTE PUT-A-FROM-T-ON-B)
;;; NIL
;;;
;;; blocks-ops plan 2 (b-on-c a-on-b)
;;; (START)
;;; (EXECUTE PUT-C-FROM-A-ON-T)
;;; (EXECUTE PUT-B-FROM-T-ON-C)
;;; (EXECUTE PUT-A-FROM-T-ON-B)
;;; NIL
;;;
;;; banana-ops plan (not-hungry)
;;; (START)
;;; (EXECUTE PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
;;; (EXECUTE CLIMB-ON-CHAIR)
;;; (EXECUTE DROP-BALL)
;;; (EXECUTE GRASP-BANANAS)
;;; (EXECUTE EAT-BANANAS)
;;; NIL
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

;;; ================= Son At School ====================

(defparameter *school-world* '(son-at-home car-needs-battery
					   have-money have-phone-book))

(defparameter *school-ops*
  (list
    ;;; operator 1
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   ;;; operator 2
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   ;;; operator 3
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   ;;; operator 4
   (make-op :action 'telephone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   ;;; operator 5
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   ;;; operator 6
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))

;;; ================= Sussman's Anomaly ====================

(defparameter *blocks-world* '(a-on-t b-on-t c-on-a clear-c clear-b))

(defparameter *blocks-ops*
  (list
   ;;; operator 1
   (make-op :action 'put-a-from-t-on-b
	    :preconds '(a-on-t clear-a clear-b b-on-c)
	    :add-list '(a-on-b)
	    :del-list '(a-on-t))
   ;;; operator 2
   (make-op :action 'put-c-from-a-on-t
	    :preconds '(c-on-a clear-c a-on-t)
	    :add-list '(clear-a c-on-t))
   ;;; operator 3
   (make-op :action 'put-b-from-t-on-c
	    :preconds '(b-on-t clear-b clear-c c-on-t)
	    :add-list '(b-on-c))
   )
  )
	    
;;; ================= Monkey and Bananas ====================

(defparameter *banana-world* '(at-door on-floor has-ball hungry chair-at-door))

(defparameter *banana-ops*
  (list
    ;;; operator 1
   (make-op :action 'eat-bananas
	    :preconds '(has-bananas)
	    :add-list '(not-hungry))
   ;;; operator 2
   (make-op :action 'grasp-bananas
	    :preconds '(at-bananas empty-handed)
	    :add-list '(has-bananas))
   ;;; operator 3
   (make-op :action 'climb-on-chair
	    :preconds '(chair-at-middle-room at-middle-room on-floor)
	    :add-list '(at-bananas))
   ;;; operator 4
   (make-op :action 'push-chair-from-door-to-middle-room
	    :preconds '(chair-at-door at-door)
	    :add-list '(chair-at-middle-room at-middle-room on-floor))
   ;;; operator 5
   (make-op :action 'drop-ball
	    :preconds '(has-ball)
	    :add-list '(empty-handed))
    )
  )

;;; ================= Pumpkin Pie Recipe ====================
  
(mapc #'convert-op *school-ops*)
(mapc #'convert-op *blocks-ops*)
(mapc #'convert-op *banana-ops*)

(provide :ops)
