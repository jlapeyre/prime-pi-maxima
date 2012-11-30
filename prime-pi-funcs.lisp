;;; Copyright (C) 2012 John Lapeyre
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(max-doc::set-cur-sec 'max-doc::number-theory-fandv)

#-(or gcl clisp allegro)
(defmfun1 ($prime_pi :doc)  ((n :uint-64) &opt ( ($threads n-threads) 1  :non-neg-int )
                     ( $status nil :bool ))
  "Computes the prime counting function. The option <threads> specifies the maximum number of cpu
 threads to use. Fewer threads may be used depending on <n>. The percent of the calculation that 
 is finished is printed during the
 calculation if the option 'status' is true. The status will only work under certain terminals."
  (setf $status (if $status 1 0))
  (prime-pi::prime-pi-with-table n-threads $status  n))

#-(or gcl clisp allegro)
(progn
  (add-call-desc '( "prime_pi" ("n") ("returns the number of primes less than or equal to " arg "n" ".")))
  (max-doc::see-also "prime_pi" '("prime_pi_soe" "next_prime" "prev_prime"))
  (max-doc::implementation "prime_pi"  "The algorithm combines a segmented sieve with tables."))



#-(or gcl clisp allegro)
(defmfun1 ($prime_twins :doc)  ((min :uint-64) &optional (max :uint-64)
                                &opt ( ($threads n-threads) 1  :non-neg-int )
                                ( $status nil :bool ) ($ktuplet 2 (:int-range 1 7) ))
  "The option ktuplet -> <k> counts the <k>-constellation rather than the twins. k must
   be an integer between 1 and 7."
  (setf $status (if $status 1 0))
  (if max t
      (progn (setf max min) (setf min 2)))
  (prime-pi::prime-twins $ktuplet  n-threads $status min max))

(max-doc::see-also "prime_twins" '("prime_pi" "next_prime" "prev_prime" "primep"))

(add-call-desc  '( "prime_twins" ("n") ("returns the number of prime twins less than or equal to " arg "n" "."))
 '( "prime_twins" ("nmin" "nmax") ("returns the number of prime twins between " arg "nmin" " and "
                                   arg "max" ".")))

(max-doc::implementation "prime_twins"  "No tables are used in this algorithm.")
