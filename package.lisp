;;;; package.lisp

(uiop:define-package #:clark
  (:use #:cl)
  (:import-from :alexandria-2
                #:alist-plist)

  (:import-from #:serapeum
                #:partial
                #:defconst
                #:~>>
                #:->
                #:assure)

  (:export #:defroutes
           #:routes))

(uiop:define-package #:handler
  (:use #:cl)
  (:import-from #:clark
                #:defroutes)
  (:import-from #:serapeum
                #:->)
  (:import-from #:ppcre
                #:split)
  (:export #:site))
