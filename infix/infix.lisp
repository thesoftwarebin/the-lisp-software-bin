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

(defvar *operator-table* '((^ (0 :right)) (* (1 :left)) (+ (2 :left)) (- (2 :left))))

(defun operator-priority (op)
  (or 
   (cadr (assoc op *operator-table*))
   (list (length *operator-table*) :left)))

(defun left-operator-wins (op1 op2)
  (let ((pr1 (operator-priority op1))  ; pr1: example for key '+ is (2 :left)
        (pr2 (operator-priority op2))) ; pr2: example for key '^ is (0 :right)
    (cond 
     ((and (eq (car pr1) (car pr2))) (not (eq (cadr pr1) :right)))
     ((< (car pr1) (car pr2))        t)
     ((> (car pr1) (car pr2))        nil)
     (t                              (progn (format nil "left-operator-wins: internal error 1 = can't determine who wins") t)))))
 
(defmacro pre (a)
  (if 
      (and 
       (listp a)
       (not (fboundp (car a)))
       (null (assoc (car a) *operator-table*))
       (not (eq (car a) 'infix)))
      (cons 'infix `,a)
    `,a))

(defmacro infix (a &optional op1 b op2 &rest r)
  (cond 
   ((null op1)                   `(pre ,a))                                  ; term is number or "(expression)"
   ((null op2)                   `(,op1 (pre ,a) (pre ,b)))                  ; put operator at first pos
   ((left-operator-wins op1 op2) `(infix (,op1 (pre ,a) (pre ,b)) ,op2 ,@r)) ; reduce terms on the left
   (t                            `(infix ,a ,op1 (infix ,b ,op2 ,@r)))))     ; reduce terms on the right
