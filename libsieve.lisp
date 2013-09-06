;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(asdf:oos 'asdf:load-op :cffi)
(in-package :prime-pi)

;; This is a cffi interface to the C++ library 'primesieve' by Kim Walisch

;; may want to set the following
;; cffi::*foreign-library-directories*

(define-foreign-library libgomp
  (:unix "libgomp.so.1") (t (:default "libgomp")))
(use-foreign-library libgomp)

(define-foreign-library
  (libprimesieve :search-path 
                 (list "." *default-pathname-defaults* *load-pathname*))
  (:unix #p"libprimesieve.so")
   (t (:default "libprimesieve")))

(use-foreign-library libprimesieve)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  finding how to close is a pita use this 
;; (close-foreign-library 'libprimesieve)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the interface functions to the C libarary here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; number of primes between min and max
(defcfun "prime_pi" :uint64 (k-tuplet :int) 
  (num-threads :int) (progress :int) (min :uint64) (max :uint64))

;; number of primes between min and max using mulitple processor threads
(defcfun "p_prime_pi" :uint64 (num-threads :int) 
  (min :uint64) (max :uint64))

(defcfun "check_error_status" :int )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the functions to be called by 
;; maxima functions here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prime-twins (k-tuplet n-threads progress min max)
  (let ((res (prime-pi k-tuplet n-threads progress min max))
        (error-status (check-error-status)))
    (if (eq error-status 0) res nil)))

(defun prime-pi-with-table (n-threads progress x)
  (destructuring-bind (pi-tab min rem)
      (prime-tables::look-up-pi-and-rem x)
    (decf rem)
    (if (<= rem 0)
        pi-tab
      (let* ((res (prime-pi 1 n-threads progress min (+ min rem)))
             (error-status (check-error-status)))
        (if (eq error-status 0) 
            (progn
;              (prime-tables::record-cached-prime-pi x (+ pi-tab res))
              (+ pi-tab res))
          nil)))))

;; not used now.
#|
(defun parallel-prime-pi-with-table (n-threads x)
  (destructuring-bind (pi-tab min rem)
      (prime-tables::look-up-pi-and-rem x)
    (decf rem)
    (if (>= rem 0)
        (progn
          (+ pi-tab (p-prime-pi n-threads min (+ min rem))))
        pi-tab)))
|#
