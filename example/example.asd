;;;; example.asd

(asdf:defsystem #:example-site
  :description "Describe compojure-clone here"
  :author "Gavin Jaeger-Freeborn"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:clark)
  :components ((:file "package")
               (:file "example" :depends-on ("package"))))

