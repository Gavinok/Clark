(in-package #:handler)

#|
(equal (car (extract-args-from-query-string "x=10" (list "x"))) "10")
|#
(-> extract-args-from-query-string ((or String Null) List) t)
(defun extract-args-from-query-string (query-str pattern)
  "Based on the query string and the arguments given attempt to
destructure the query string to match the expression"
  ;; Split each arg by its parameter
  ;; http://localhost:3000/foobar?x=foo&y=bar&z=baz&w=qux
  (let ((queries (split "&" query-str)))
    (loop :for query :in queries
          :for arg :in pattern
          ;; Split the parameter and it's value appart
          ;; aka x=foo -> x foo
          :for q = (split "=" query)
          :when (equal arg (first q))
            :collect (second q))))

(deftype Clack-Env () `Cons)

(-> site (List) (-> (Clack-Env) List))
(defun site (route-list)
  (lambda (env)
    (format t "~a~%" env)
    (let* ((method-type   (the Keyword
                               (getf env :request-method)))
           (path          (the String
                               (getf env :path-info)))
           (query-str     (the (or String Null)
                               (getf env :query-string)))
           (method-routes (getf route-list method-type))
           ;; Look up the apropriate list for this path
           (route
             ;; (assoc path
             ;;        method-routes
             ;;        :test #'equal)
             (clark::lookup-route path method-routes)))
      ;; Look up available routes based on this method type
      (if route
          `(200 ();; (:content-type "text/plain")
                (
                 ,(format nil "~a"
                          (typecase (second route)
                            ;; (GET "/" (x y) (format nil "x was equal to ~a y" x y))
                            (List  (apply (eval (third route))
                                          (extract-args-from-query-string
                                           query-str (second route))))
                            ;; (GET "/" request (format nil "~a" request))
                            (Symbol (apply (eval (third route))
                                           (list env)))))))
          `(404 (:content-type "text/plain")
                ("Page could not be found"))))))


(->  inner-request-handler (Clack-Env) List)
(defun inner-request-handler (env)
  (funcall (site
            (clark::routes
              (GET "/hello"      ()       "hello")
              (GET "/nope/world" ()       "Nope World")
              (GET "/fullthing"  env      (format nil "~a" env))
              (GET "/nope"       (x)      (format nil x))
              (GET "/long"       (x y z)  (format nil "<h2>~a</h2>"
                                                  (+ (parse-integer x)
                                                     (parse-integer y)
                                                     (parse-integer z))))))
           env))

;; (defun request-handler (env)
;;   (inner-request-handler env))
#|
;; create a basic app
(defvar *app* #'request-handler)
(defvar *handler* (clack:clackup *app*))
|#
