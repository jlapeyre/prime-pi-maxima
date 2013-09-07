;;; In principle, this can be used to load
;;; the prime-pi code for table lookup and sieve
;;; directly from lisp without maxima.
;;; But, due to asdf hell, etc., it probably won't
;;; work.
(load "./packages-prime.lisp")
(load "./prime-tables")
(prime-tables::setup-prime-pi-tables)
(load "./libsieve")
(load "./prime-pi-funcs")
