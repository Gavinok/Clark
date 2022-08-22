(in-package #:handler)

#|
(equal (car (extract-args-from-query-string "x=10" (list "x"))) "10")
|#
(defun extract-args-from-query-string (query-str pattern)
  (cond
    ;; (GET "/" request (str request))
    ((symbolp pattern) query-str)
    ;; (GET "/" (x y z) (str "x was equal to " x " y is " y " z is " z))
    ((list pattern)
     ;; Split each arg by its parameter
     ;; http://localhost:3000/foobar?x=foo&y=bar&z=baz&w=qux
     (let ((queries (ppcre:split "&" query-str)))
       (loop :for query :in queries
             :for arg :in pattern
             ;; Split the parameter and it's value appart
             :for q = (ppcre:split "=" query)
             :when (equal arg (first q))
               :collect (second q))))))

(defun inner-request-handler (env)
  (format t "~a~%" env)
  (let* ((method-type (getf env :request-method))
         (path        (getf env :path-info))
         (query-str   (getf env :query-string))

         ;; Look up the apropriate list for this path
         (arg (assoc path
                     (getf temp-handler-list method-type)
                     :test #'equal)))
    ;; Look up available routes based on this method type
    (if arg
        `(200 (:content-type "text/plain")
              (
               ,(format nil "~a"
                        (apply
                         (third arg)
                         (extract-args-from-query-string query-str (second arg))))))
        `(404 (:content-type "text/plain")
              ("Page could not be found")))))

#|
;; create a basic app
(defvar *app*
  #'request-handler)
|#
(defun request-handler (env)
  (inner-request-handler env))
