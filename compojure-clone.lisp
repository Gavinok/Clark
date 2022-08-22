;; https://github.com/weavejester/compojure/wiki/Destructuring-Syntax
;; https://github.com/ring-clojure/ring/wiki/Concepts#requests
;;;; compojure-clone.lisp

(in-package #:compojure-clone)

(deftype HTTP-Method ()
  `(member :GET :POST :PUT :DELETE))

(defvar *method-handlers* `(:GET () :POST () :PUT () :DELETE ())
  "Contains a list of http methods and a list of functions to call based
on the contents of the request")

(defun create-route (path destructuring operation)
  `(list
    ;; Store the url path to this endpoint
    ,path
    ;; Create a list to match against the query args with
    ,(if (listp destructuring)
         (cons 'list
               (mapcar (lambda (arg)
                         (string-downcase (symbol-name arg)))
                       destructuring))
         ;; Kinda a hack to avoid evaluating the symbol
         `',destructuring)
    ;; Create a lambda that executas the given operation while using
    ;; these given arguments as variables
    (lambda ,(cond
          ((listp destructuring) destructuring)
          ((symbolp destructuring) (list destructuring))
          (t (error "Destructuring lambda list must be either a list or symbol.")))
      ,operation)))

(defmacro routes (&body routes)
  (let ((method-lists '(:GET :POST :PUT :DELETE))
        (formatted-routes (mapcar
                           (lambda (route)
                             (destructuring-bind (method path destructuring operation) route
                               (cons
                                ;; Used for matching against the possible methods
                                (intern (symbol-name method) "KEYWORD")
                                (create-route path destructuring operation))))
                           routes)))
    (cons 'list
          (alist-plist
           (mapcar
            (lambda (method)
              (line-up-last ; ->> But from alexandria
               formatted-routes
               ;; Match the routes against the stored method
               (remove-if-not (lambda (route) (eql (car route) method)))
               ;; Remove the method from each route since
               ;; it is nolonger needed
               (mapcar #'cdr)
               ;; Ensure that the nested lists will be evaluated
               ;; as a list and not a function call
               (append `(,method list))
               ))
            method-lists)))))

(defmacro defroutes (symbol &body routes)
  `(defvar ,symbol (routes ,@routes)))

#|
;;; Example usage
(routes
  (GET "/hello"      ()       "hello")
  (GET "/nope/world" ()       "Nope World")
  (GET "/fullthing"  env      (format nil "~a" env))
  (GET "/nope"       (x)      (format nil x))
  (GET "/long"       (x y z)  (format nil "~a"
                                      (+ (parse-integer x)
                                         (parse-integer y)
                                         (parse-integer z)))))

;; or

(defroutes tmp
  (GET "/hello"      ()       "hello")
  (GET "/nope/world" ()       "Nope World")
  (GET "/fullthing"  env      (format nil "~a" env))
  (GET "/nope"       (x)      (format nil x))
  (GET "/long"       (x y z)  (format nil "~a"
                                      (+ (parse-integer x)
                                         (parse-integer y)
                                         (parse-integer z)))))
|#

#|
;;; Generated code expected by the handler
(list :get
      (list (list "/hello" (list) (lambda () "hello"))
            (list "/nope/world" (list) (lambda () "Nope World"))
            (list "/fullthing" 'env (lambda (env) (format nil "~a" env)))
            (list "/nope" (list "x") (lambda (x) (format nil x)))
            (list "/long" (list "x" "y" "z")
                  (lambda (x y z)
                    (format nil "~a"
                            (+ (parse-integer x) (parse-integer y)
                               (parse-integer z))))))
      :post (list) :put (list) :delete (list))
|#
