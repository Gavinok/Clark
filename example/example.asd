;;;; example.asd

(asdf:defsystem #:example-site
  :description "Describe compojure-clone here"
  :author "Gavin Jaeger-Freeborn"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:compojure-clone)
  :components ((:file "package")
               (:file "example-site" :depends-on ("package"))))

