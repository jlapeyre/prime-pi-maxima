(in-package :common-lisp-user)

(asdf:oos 'asdf:load-op :cffi)

(if (find-package :prime-pi ) t
    (defpackage :prime-pi (:use :common-lisp :cffi)))

(if (find-package :prime-tables ) t
    (defpackage :prime-tables (:use :common-lisp )))
