;;;; package.lisp

(uiop:define-package #:compojure-clone
  (:use #:cl)
  (:import-from :alexandria-2
   :alist-plist :line-up-last)
  (:import-from :serapeum
   :partial :defconst :~>> :-> :assure))

(uiop:define-package #:handler
  (:use #:cl)
  (:import-from :compojure-clone
   :defroutes)
  (:import-from :serapeum
   :->)
  (:import-from :ppcre
   :split))
