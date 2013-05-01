#|| 
    Copyright (C) 2013 Andrea Rossetti (aka "The Software Bin")
                         http://andrear.altervista.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program, it's in the included file agpl.txt. 

    If agpl-3.0.txt is missing, you can read it here:

        <http://www.gnu.org/licenses/agpl-3.0.txt>

||#

(defpackage #:org.altervista.andrear.shortest-path
  (:use 
   #:org.tfeb.hax.memoize 
   #:cl)
  (:export 
   #:shortest-path 
   #:graph-to-string 
   #:graph-to-png 
   #:path-to-list 
   #:test-graph
   #:printpath
   #:progname)
   (:documentation 
    "The following package solves the shortest path problem
    using a completely functional approach. The shortest
    path is defined as a recursive function with the
    following arguments:

    - graph edges
    - source node
    - destination node
    - list of nodes already visited by the algorithm
 
   org.tfeb.hax.memoize package must be loaded before
   shortest-path package. It is used to memoize the
   intermediate results and provide some speedup for
   bigger graphs.

   This initial release only solves simplified problems
   where the cost function returns constantly the
   edge weight 1, but the solver can be easily extended
   by changing the definition of function (cost edges i j).

   Please note that this package has actually been made by
   a LISP newbie who just wanted to train himself a bit,
   and has NOT been tested thoroughly for correctness nor
   performance.

   --------------------------------------------------------

   Usage example:

   (ql:quickload :memoize) ; loading easily with quicklisp
   (load \"shortestpath.lisp\")
   (use-package :org.altervista.andrear.shortest-path)
   (setq g (test-graph)) ; returns graph g
   (shortest-path g 1 8) ; shortest path from node 1 to 8
   ; answer is: (3 ((2 ((1 ((0 NIL) (1 3))) (3 7))) (7 8))) 
   ; i.e. the path is (1 3)->(3 7)->(7 8)")
)

(in-package #:org.altervista.andrear.shortest-path)

(defvar progname "C:\\Users\\andrea\\Documents\\Programmi\\Graphviz\\bin\\dot.exe")

(defun aggregate-lists (lst) 
   "Utility function to merge many lists into a single one."
   (remove-duplicates
    (reduce 
     (lambda (a b)
       (concatenate 'list a b))  
     lst)))

(defun has-edge (edges from to)
  (not 
   (null 
    (member to (d-star-plus edges from)))))

(defun cost (edges i j)
  (if (has-edge edges i j) 1 999))

(defun d-star-plus (edges from)
  (cadr 
   (assoc from edges)))

(defun d-star-plus-edges (edges from)
  (mapcar (lambda (i) (list from i)) (d-star-plus edges from)))

(defun group-minus (v element)
  (remove-if 
   (lambda (n) (equal n element)) 
   v))

(defun d-star-plus-group (edges group)
  (remove-if-not 
   (lambda (n) (null (member (cadr n) group))) ; remove if not rhs in group
   (aggregate-lists
    (mapcar (lambda (i) (d-star-plus-edges edges i)) group))))

(defun d-star-plus-group-star-minus-node (edges group node)
  (remove-if-not 
   (lambda (n) (equal node (cadr n)))
   (d-star-plus-group edges group)))

(defun shortestpath-argmin (edge-and-value-1 edge-and-value-2)
  (if 
      (<= 
       (car edge-and-value-1) 
       (car edge-and-value-2)) 
      edge-and-value-1 
    edge-and-value-2))

(org.tfeb.hax.memoize:def-memoized-function shortest-path-private (edges s d visited)
 "Find recursively the shortest path from s to d for the graph defined by edges. 
    `visited' is the list of nodes visited by the function, and must be initially
    set to the list of all nodes."
 (cond
  ( (or (equal s d) (null visited) (equal (list s) visited)) (list 0 nil))
  ( t 
    (let 
	(
	 (visited-minus-d (group-minus visited d))
	 (i (d-star-plus-group-star-minus-node edges (group-minus visited d) d)))
      (cond 
       ((null i) (list 100000 nil))
       (t (reduce 
	   'shortestpath-argmin
	   (mapcar 
	    (lambda (k) 
	      (let 
		  ((shortestpath-sub-1 (shortest-path-private edges s (car k) visited-minus-d)))
		(list
		 (+
		  (car shortestpath-sub-1)
		  (cost edges (car k) d)) 
		 (list shortestpath-sub-1 (list (car k) d)))))
	    i))))))))

(defun shortest-path (graph-structure source-node destination-node)
  "Find recursively the shortest path from s to d for the given graph."
  (progn
    (if (not (org.tfeb.hax.memoize:function-memoized-p 'shortest-path-private))
	(org.tfeb.hax.memoize:memoize-function 'shortest-path-private))
    (org.tfeb.hax.memoize:clear-memoized-function 'shortest-path-private)
    (shortest-path-private (cadr graph-structure) source-node destination-node (car graph-structure))))

(defun graph-to-string (edges)
  (format nil "digraph g {~%~{~^ ~d~^~%~}~%}" (mapcar (lambda (i) (format nil "~A -> {~{~A~^ ~}}" (car i) (cadr i))) edges)))

(defun graph-to-png (edges pngfilename)
  (with-open-stream 
   (s (EXT:RUN-PROGRAM progname :arguments '("-Tpng" "-Grankdir=LR" "-o" pngfilename) :input :stream :output :terminal :wait t)) 
   (princ (graph-to-string edges) s)))

(defun path-to-list (s) 
  "Convert a path returned by `shortest-path' to a (slightly more readable) string."
  (let ( (ss1 (caadr s)) (ss2 (cadr s)) ) 
    (if (null ss1) nil (list (path-to-list ss2) ss1))))

(defun test-graph ()
  "Provide an example graph (it's a list of nodes and a list of delta-stars for every node)."
  '(
    (1 2 3 4 5 6 7 8)
    (
     (1 (2 3)) 
     (2 (3 4)) 
     (3 (6 7)) 
     (4 (5 7))
     (5 (1))
     (6 (7))
     (7 (8 5))
     (8 (6)))))

#|| 

;; --- helper functions for tracing or debugging
;(trace 
; (shortest-path :max-depth 8))
;(setq CUSTOM:*TRACE-INDENT* " ")



||#
