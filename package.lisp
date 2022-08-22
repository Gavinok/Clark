;;;; package.lisp

(uiop:define-package #:compojure-clone
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
  (:import-from #:compojure-clone
                #:defroutes)
  (:import-from #:serapeum
                #:->)
  (:import-from #:ppcre
                #:split)
  (:export #:generate-request-handler))
