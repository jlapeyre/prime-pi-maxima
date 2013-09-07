;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
(in-package :maxima)

;;; These are the highest level functions for the prime_pi package.
;;; They are called directly from maxima.
;;; The functions that they call should be callable from CL
;;; independently from maxima.

;; The reader macros here are kind of redundant. The lisps we can't
;; use are also filtered at the mext level.

(max-doc:set-cur-sec 'max-doc::number-theory-fandv)
(defmfun1:set-mext-package "prime_pi")

#-(or gcl clisp allegro)
(defmfun1 ($prime_pi :doc)  ((n :uint-64) &opt ( ($threads n-threads) 1  :non-neg-int )
                     ( $status nil :bool ))
  :desc 
  ("Computes the prime counting function. The option "
   :arg "threads" " specifies the maximum number of cpu threads to use. The routine
    may use fewer threads, depending on the value of " :argdot "n"
   " The value of " :arg "n" " is limited to 2^64, that is, about 1.84e+18."
   :par ""
   " The percent of the calculation that is finished is printed during the
   calculation if the option " :arg "status" " is true. The status will only work under 
   some terminals.")

  (setf $status (if $status 1 0))
  (prime-pi::prime-pi-with-table n-threads $status  n))

#-(or gcl clisp allegro)
(progn
  (add-call-desc 
   '( "prime_pi" ("n") ("returns the number of primes less than or equal to " :arg "n" ".")))

  (max-doc:see-also "prime_pi" '("prime_pi_soe" "next_prime" "prev_prime"))

  (max-doc:implementation "prime_pi"  
   "This algorithm is fast, especially for a general purpose mathematics program.
    It combines a segmented sieve implemented as a C library with tables.")

  (max-doc:author 
   "prime_pi" 
   '("Kim Walisch (C library)" "Tomas Oliveira e Silva (tables)" "John Lapeyre (lisp)")))

#-(or gcl clisp allegro)
(defmfun1 ($prime_twins :doc)  ((min :uint-64) &optional (max :uint-64)
                                &opt ( ($threads n-threads) 1  :non-neg-int )
                                ( $status nil :bool ) ($ktuplet 2 (:int-range 1 7) ))
  :desc 
  ("The option " :opt "ktuplet" " counts the " :opt "ktuplet" 
   "-constellation rather than the twins. "
   :opt "ktuplet" " must be an integer between 1 and 7.")
  (setf $status (if $status 1 0))
  (if max t
      (progn (setf max min) (setf min 2)))
  (prime-pi::prime-twins $ktuplet  n-threads $status min max))

#-(or gcl clisp allegro)
(max-doc::see-also "prime_twins" '("prime_pi" "next_prime" "prev_prime" "primep"))

#-(or gcl clisp allegro)
(add-call-desc  
 '( "prime_twins" ("n") ("returns the number of prime twins less than or equal to " :arg "n" "."))
 '( "prime_twins" ("nmin" "nmax") 
    ("returns the number of prime twins between " :arg "nmin" " and " :arg "max" ".")))

#-(or gcl clisp allegro)
(max-doc::implementation "prime_twins"  "No tables are used in this algorithm.")
