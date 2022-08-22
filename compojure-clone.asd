;;;; compojure-clone.asd

(asdf:defsystem #:compojure-clone
  :description "Describe compojure-clone here"
  :author "Gavin Jaeger-Freeborn"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack)
  :components ((:file "package")
               (:file "compojure-clone")
               (:file "handler")))
