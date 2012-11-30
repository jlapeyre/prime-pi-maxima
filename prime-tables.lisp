(in-package :prime-tables)

(defstruct (prime-table)
  (data '#() :type array)
  (exp 0 :type real)
  (base 10 :type fixnum)
  (incr 0 :type real)
  (max 0 :type real))

;; this is referenced in data table files
;(defvar *prime-pi-tables* '() )
(defparameter *prime-tables-structs* '())
(defparameter *prime-table-maxes* '())
;;(defparameter *prime-tables* '())

;; looks like I don't need this now
;; The tables are constructed as arrays.
(defun prime-data-list-to-array (data-list)
  "Somehow was not able to initialize array with large literal
   list. So make the array in two stages, with the literal list
   created first."
  (make-array (length data-list) :initial-contents data-list))

(defun set-prime-table (data-array  exp)
  (let* ((base 10)
         (incr (expt base exp)))
;;         (data-array (prime-data-list-to-array data-list)))
    (make-prime-table
     :data data-array
     :exp exp
     :base base
     :incr incr
     :max (* incr (1- (length data-array))))))

(defun look-up-pi-and-rem-in-table (x table)
  (let ((incr (prime-table-incr table)))
    (multiple-value-bind (indv remv) (floor x incr)
;;      (format t "incr ~a indv ~a  remv ~a~%" incr indv remv)
      (list (aref (prime-table-data table) indv)
            (1+ (* incr indv))
            remv))))

(defun look-up-pi-and-rem (x)
  (let ((pos (position-if #'(lambda (y) (< x y)) *prime-table-maxes*)))
    (unless pos (format t "*** x is too large~%"))
;;    (format t "** using table with incr ~a~%" (prime-table-incr (elt *prime-tables-structs* pos)))
    (look-up-pi-and-rem-in-table x (elt *prime-tables-structs* pos))))

(defun setup-prime-pi-tables-2 ()
  (let ((res) (tables (reverse *prime-pi-tables*))) ; they were pushed onto the list
    (loop for i from 1 to (length tables) do
      (push (set-prime-table (pop tables) i) res))
    (nreverse res)))

(defun setup-prime-pi-tables-1 ()
  (setf *prime-tables-structs* (setup-prime-pi-tables-2))
  (setf *prime-table-maxes*
    (mapcar #'prime-table-max *prime-tables-structs*))
  t)

(defun prime-pi-table-filename (base)
  (concatenate 'string "./1d"
               (format nil "~2,'0D" base) ".lisp"))

(defun old-load-prime-pi-tables ()
  "Don't load file 1d00, it uses 10^k rather than k 10^n."
  (let ((min-base 1) (max-base 22))
    (loop for i from min-base to max-base do
         (load (prime-pi-table-filename i)))))

(defun load-prime-pi-tables ()
  "Don't load file 1d00, it uses 10^k rather than k 10^n.")
;  (load "./prime_pi_tables.lisp"))

(defun setup-prime-pi-tables ()
;;  (format t "length is ~a~%" (length *prime-pi-tables*))
  (unless (> (length *prime-pi-tables*) 0)
    (load-prime-pi-tables))
  (setup-prime-pi-tables-1))

(setup-prime-pi-tables)

;; Load the tables
;;(prime-tables::setup-prime-pi-tables)

#|
 example of how to use tables

(defun prime-pi-with-table (x)
  (destructuring-bind (pi-tab min rem)
      (prime-tables::look-up-pi-and-rem x)
;;    (format t "pi-tab ~a min ~a rem ~a~%" pi-tab min rem)
    (decf min)
    (if (>= min 0)
        (+ pi-tab (prime-pi min (+ min rem)))
        pi-tab)))

|#

;;  use these commnads
;;;   (prime-tables::load-prime-pi-tables)
;;;    (prime-tables::setup-prime-pi-tables)



;; (defvar *prime-tables-structs* (setup-prime-tables))
;; (look-up-pi-and-rem-in-table (* 3 (expt 10 9)) (second *prime-tables-structs*))
;; (look-up-pi-and-rem (+ (expt 10 1) 7))
