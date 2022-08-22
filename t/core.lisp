(defpackage core.test
  (:use :cl :compojure-clone :fiveam))

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
