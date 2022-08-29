(defpackage core.test
  (:use :cl :clark :handler :fiveam))

(in-package #:core.test)

(setf 5am:*debug-on-failure* t)


(5am:test valid-routes
  (5am:finishes (clark::routes
                  (GET "/hello"      nil       "hello")))
  (5am:finishes (clark::routes
                  (GET "/hello"      ()       "hello")))
  (5am:finishes (clark::routes
                  (GET "/fullthing"  env      (format nil "~a" env))))
  (5am:finishes (clark::routes
                  (GET "/nope"       (x)      (format nil x))))
  (5am:finishes (clark::routes
                  (GET "/long"       (x y z)  (format nil "~a"
                                                      (+ (parse-integer x)
                                                         (parse-integer y)
                                                         (parse-integer z)))))))

(5am:test invalid-routes
  (5am:signals TYPE-ERROR (macroexpand-1 `(clark::routes
                                       (GET "/hello"      "hello"   "hello"))))
  (5am:signals TYPE-ERROR (macroexpand-1 `(clark::routes
                                       (YEET "/hello"      ()       "hello"))))
  (5am:signals ERROR (eval `(clark::routes
                              (GET "/hello"      (y))))))

(5am:test request-responses
  (5am:is (string=
           (car (third (funcall (handler:site
                                 (clark::routes
                                   (GET "/noargs" () "hello")))
                                (list
                                 :REQUEST-METHOD :GET
                                 :PATH-INFO "/noargs"
                                 :REQUEST-URI "/noargs"
                                 :QUERY-STRING NIL))))
           "hello"))
  (5am:is (string=
           (car (third (funcall (handler:site
                                 (clark::routes
                                   (GET "/twoargs" (x y)
                                        (+ (parse-integer x) (parse-integer y)))))
                                (list
                                 :REQUEST-METHOD :GET
                                 :PATH-INFO "/twoargs"
                                 :REQUEST-URI "/twoargs"
                                 :QUERY-STRING "x=1&y=2"))))
           "3"))
  (5am:is (string=
           (car (third (funcall (handler:site
                                 (clark::routes
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
           (let ((routes  (clark::routes
                            (GET "/fullenv" request (format nil "~a" request)))))
             (car (third (funcall (handler:site routes)
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
             (clark::defroutes *routes*
               (GET "/fullenv" request (format nil "~a" request)))
             (car (third (funcall (handler:site *routes*)
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
             (clark::defroutes *routes*
               (GET "/fullenv/withname" request (format nil "~a" request)))
             (car (third (funcall (handler:site *routes*)
                                  (list
                                   :REQUEST-METHOD :GET
                                   :PATH-INFO "/fullenv/withname"
                                   :REQUEST-URI "/fullenv/withname"
                                   :QUERY-STRING "x=1&y=2")))))
           (format nil "~a"
                   (list
                    :REQUEST-METHOD :GET
                    :PATH-INFO "/fullenv/withname"
                    :REQUEST-URI "/fullenv/withname"
                    :QUERY-STRING "x=1&y=2"))))
  (5am:is (string=
           (progn
             (clark::defroutes *routes*
               (GET "/fullenv/" request (format nil "~a" "bad-case"))
               (GET "/fullenv/withname" request (format nil "~a" request)))
             (car (third (funcall (handler:site *routes*)
                                  (list
                                   :REQUEST-METHOD :GET
                                   :PATH-INFO "/fullenv/withname"
                                   :REQUEST-URI "/fullenv/withname"
                                   :QUERY-STRING "x=1&y=2")))))
           (format nil "~a"
                   (list
                    :REQUEST-METHOD :GET
                    :PATH-INFO "/fullenv/withname"
                    :REQUEST-URI "/fullenv/withname"
                    :QUERY-STRING "x=1&y=2"))))
  ;; (5am:is (string=
  ;;          (let ((routes  (clark::routes
  ;;                           (GET "/people/:name" (name)
  ;;                                (format nil "Hello ~a" (name))))))
  ;;            (car (third (funcall (handler:site routes)
  ;;                                 (list
  ;;                                  :REQUEST-METHOD :GET
  ;;                                  :PATH-INFO "/fullenv"
  ;;                                  :REQUEST-URI "/fullenv"
  ;;                                  :QUERY-STRING "x=1&y=2")))))
  ;;          (format nil "~a"
  ;;                  (list
  ;;                   :REQUEST-METHOD :GET
  ;;                   :PATH-INFO "/fullenv"
  ;;                   :REQUEST-URI "/fullenv"
  ;;                   :QUERY-STRING "x=1&y=2"))))
  )
