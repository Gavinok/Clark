(ql:quickload :clack)
(ql:quickload :trivia)

(defvar temp-handler-list
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
      :post (list) :put (list) :delete (list)))

(defun request-handler (env)
  (inner-request-handler env))

(defvar *app*
  #'request-handler)

(defun start-server ()
  (defparameter hdlr (clack:clackup *app* :server :hunchentoot :port 4008)))

(defun restart-server ()
  (clack:stop hdlr)
  (start-server))

(restart-server)
