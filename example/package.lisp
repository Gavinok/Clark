;;;; package.lisp

(uiop:define-package example-site
  (:use :cl)
  (:import-from :compojure-clone
   :defroutes))

