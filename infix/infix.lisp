;; infix.lisp: perform simple calculations using infix notation
;;             example: (infix 3 + (4 * 5) ^ 2) ---> 403
;;
;; Copyright (C) 2014 Andrea Rossetti
;;                    http://andrear.altervista.org

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun ^ (a b) (expt a b))

(deftype asstype () `(member right left nonassoc))

(defstruct opprop (prio nil :type integer) (ass nil :type asstype))

(defvar *operator-table*
  '((^ . #S(opprop :prio 0 :ass right))
    (* . #S(opprop :prio 1 :ass  left))
    (/ . #S(opprop :prio 1 :ass  left))
    (+ . #S(opprop :prio 2 :ass  left))
    (- . #S(opprop :prio 2 :ass  left))))

(defun find-opprop (op)
  (cdr (assoc op *operator-table*)))


(defun opprop-lessp (op1 op2)
  (let* ((opp1 (find-opprop op1)) ; opp1: example for key '+ is (2 :left)
	 (opp1p (opprop-prio opp1))
         (opp2 (find-opprop op2)) ; opp2: example for key '^ is (0 :right)
	 (opp2p (opprop-prio opp2)))
    (cond 
     ((= opp1p opp2p) (eq (opprop-ass opp1) :left))
     ((< opp1p opp2p) t)
     ((> opp1p opp2p) nil)
     (t		      (error "Could not compare operators (~s . ~s) and (~s . ~s)" op1 opp1 op2 opp2)))))

(defmacro infix (a &optional op1 b op2 &rest r)
  (cond 
    ((null op1) (if (and 
		     (listp a)
		     (not (and (symbolp (car a)) (fboundp (car a))))
		     (null (find-opprop (car a)))
		     (not (eq (car a) 'infix)))
		    `(infix ,@a) ; then "(expression)"
		    `,a))        ; else number or non-infix expression 
   ((null op2)             `(,op1 (infix ,a) (infix ,b)))		   ; put operator at first pos
   ((opprop-lessp op1 op2) `(infix (,op1 (infix ,a) (infix ,b)) ,op2 ,@r)) ; reduce terms on the left
   (t                      `(infix ,a ,op1 (infix ,b ,op2 ,@r)))))         ; reduce terms on the right

