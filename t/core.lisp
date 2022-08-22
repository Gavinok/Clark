(defpackage core.test
  (:use :cl :compojure-clone :handler :fiveam))

(in-package #:core.test)

(setf 5am:*debug-on-failure* t)


(5am:test valid-routes
  (5am:finishes (compojure-clone::routes
                  (GET "/hello"      nil       "hello")))
  (5am:finishes (compojure-clone::routes
                  (GET "/hello"      ()       "hello")))
  (5am:finishes (compojure-clone::routes
                  (GET "/fullthing"  env      (format nil "~a" env))))
  (5am:finishes (compojure-clone::routes
                  (GET "/nope"       (x)      (format nil x))))
  (5am:finishes (compojure-clone::routes
                  (GET "/long"       (x y z)  (format nil "~a"
                                                      (+ (parse-integer x)
                                                         (parse-integer y)
                                                         (parse-integer z)))))))

(5am:test invalid-routes
  (5am:signals TYPE-ERROR (macroexpand-1 `(compojure-clone::routes
                                       (GET "/hello"      "hello"   "hello"))))
  (5am:signals TYPE-ERROR (macroexpand-1 `(compojure-clone::routes
                                       (YEET "/hello"      ()       "hello"))))
  (5am:signals ERROR (eval `(compojure-clone::routes
                              (GET "/hello"      (y))))))

(5am:test generate-request-handler
  (5am:is (string=
           (car (third (funcall (generate-request-handler
                                 (compojure-clone::routes
                                   (GET "/noargs" () "hello")))
                                (list
                                 :REQUEST-METHOD :GET
                                 :PATH-INFO "/noargs"
                                 :REQUEST-URI "/noargs"
                                 :QUERY-STRING NIL))))
           "hello"))
  (5am:is (string=
           (car (third (funcall (generate-request-handler
                                 (compojure-clone::routes
                                   (GET "/twoargs" (x y)
                                        (+ (parse-integer x) (parse-integer y)))))
                                (list
                                 :REQUEST-METHOD :GET
                                 :PATH-INFO "/twoargs"
                                 :REQUEST-URI "/twoargs"
                                 :QUERY-STRING "x=1&y=2"))))
           "3"))
  (5am:is (string=
           (car (third (funcall (generate-request-handler
                                 (compojure-clone::routes
                                   (GET "/fullenv" request (format nil "~a" request))))
                                (list
                                 :REQUEST-METHOD :GET
                                 :PATH-INFO "/fullenv"
                                 :REQUEST-URI "/fullenv"
                                 :QUERY-STRING "x=1&y=2"))))
           (format nil "~a"
                   (list
                    :REQUEST-METHOD :GET
                    :PATH-INFO "/fullenv"
                    :REQUEST-URI "/fullenv"
                    :QUERY-STRING "x=1&y=2"))))
  (5am:is (string=
           (let ((routes  (compojure-clone::routes
                            (GET "/fullenv" request (format nil "~a" request)))))
             (car (third (funcall (generate-request-handler routes)
                                  (list
                                   :REQUEST-METHOD :GET
                                   :PATH-INFO "/fullenv"
                                   :REQUEST-URI "/fullenv"
                                   :QUERY-STRING "x=1&y=2")))))
           (format nil "~a"
                   (list
                    :REQUEST-METHOD :GET
                    :PATH-INFO "/fullenv"
                    :REQUEST-URI "/fullenv"
                    :QUERY-STRING "x=1&y=2"))))
  (5am:is (string=
           (progn
             (compojure-clone::defroutes routes
               (GET "/fullenv" request (format nil "~a" request)))
             (car (third (funcall (generate-request-handler routes)
                                  (list
                                   :REQUEST-METHOD :GET
                                   :PATH-INFO "/fullenv"
                                   :REQUEST-URI "/fullenv"
                                   :QUERY-STRING "x=1&y=2")))))
           (format nil "~a"
                   (list
                    :REQUEST-METHOD :GET
                    :PATH-INFO "/fullenv"
                    :REQUEST-URI "/fullenv"
                    :QUERY-STRING "x=1&y=2")))))
