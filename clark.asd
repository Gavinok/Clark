;;;; clark.asd

(asdf:defsystem #:clark
  :description "Describe clark here"
  :author "Gavin Jaeger-Freeborn"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:clack #:alexandria #:serapeum #:cl-ppcre)
  :components ((:file "package")
               (:file "clark")
               (:file "handler" :depends-on ("clark"))))

(asdf:defsystem #:clark-test
  :description "Test Suite For The Compojure-Clone Project"
  :author "Gavin Jaeger-Freeborn"
  :license "MIT"
  :depends-on (#:clark #:fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "core")))))
