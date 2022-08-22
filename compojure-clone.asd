;;;; compojure-clone.asd

(asdf:defsystem #:compojure-clone
  :description "Describe compojure-clone here"
  :author "Gavin Jaeger-Freeborn"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:clack #:alexandria #:serapeum #:cl-ppcre)
  :components ((:file "package")
               (:file "compojure-clone")
               (:file "handler" :depends-on ("compojure-clone"))))

(asdf:defsystem #:compojure-clone-test
  :description "Test Suite For The Compojure-Clone Project"
  :author "Gavin Jaeger-Freeborn"
  :license "MIT"
  :depends-on (#:compojure-clone #:fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "core")))))
