;; https://github.com/weavejester/compojure/wiki/Destructuring-Syntax
;; https://github.com/ring-clojure/ring/wiki/Concepts#requests
;;;; compojure-clone.lisp

(in-package #:compojure-clone)

(defvar *method-handlers* `(:GET () :POST () :PUT () :DELETE ())
  "Contains a list of http methods and a list of functions to call based
on the contents of the request")

(defconst +supported-methods+ '(:GET :POST :PUT :DELETE))

(defun list-of-symbols-p (list)
  "Test that the list only contains symbols."
  (every #'symbolp list))

(deftype List-Of-Symbols ()
  `(satisfies list-of-symbols-p))

(defun list-of-strings-p (list)
  "Test that the list only contains strings."
  (every #'stringp list))

(deftype List-Of-strings ()
  `(satisfies list-of-strings-p))

(deftype HTTP-Method ()
  `(member ,@+supported-methods+))

(-> create-route (string (or list symbol) t) list)
(defun create-route (path lambda-list expression)
  "Creates a new internal representation of a route. The basic structure
is as follows:

(PATH LAMBDA-LIST-STR FUNCTION)

PATH is the full url path to the endpoint.

LAMBDA-LIST-STR is a string representation of the function arguments.
These are then used to match against query paramaters so storing them
as a string is just to save on the runtime cost of converting them at
runtime.

Function is as you would expect a function taking the same number of
arguments in the same order of the lambda list. The function will
evaluate the given EXPRESSION"
  `(list
    ;; Store the url path to this endpoint
    ,path
    ;; Create a list to match against the query args with
    ,(if (listp lambda-list)
         (cons 'list
               (assure list-of-strings
                 (mapcar (lambda (arg)
                           (string-downcase (symbol-name arg)))
                         lambda-list)))
         ;; Kinda a hack to avoid evaluating the symbol
         `',lambda-list)
    ;; Create a lambda that evaluates the given expression while using
    ;; these given arguments as variables.
    (lambda ,(cond
          ((listp lambda-list) lambda-list)
          ((symbolp lambda-list) (list lambda-list)))
      ;; If there are arguments attempt to declare their types
      ,(when (consp lambda-list)
         `(declare
           ,@(mapcar (lambda (arg)
                      `(type String ,arg))
                    lambda-list)))
      ,expression)))

(defun define-all-routes (routes)
  (let ((formatted-routes (mapcar
                           (lambda (route)
                             (destructuring-bind (method path destructuring operation) route
                               (cons
                                ;; Used for matching against the possible methods
                                (assure http-method
                                  (intern (symbol-name method) "KEYWORD"))
                                (create-route path destructuring operation))))
                           routes)))
    (cons 'list
          (alist-plist
           (mapcar
            (lambda (method)
              (~>> ; ->> But from alexandria
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
            +supported-methods+)))))

(defmacro routes (&body routes)
  (define-all-routes routes))

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
