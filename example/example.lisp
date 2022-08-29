(in-package #:example-site)

(defun meetings ()
  (format nil "<h1>Meetings</h2>~&~a"
          (reduce #'serapeum:concat
                  (mapcar (serapeum:partial #'format nil "<p>~a:00</p>")
                          (sort (loop :repeat 10
                                      :collect (random 24))
                                #'<)))))
(defun calculator ()
  "A simple calculator page which can be used as an example of how to
reach out to other "
  (format nil
          "
<form action=\"/add\" method=\"GET\">
  <div>
    <label for=\"opone\">First Operand</label>
    <input type=\"text\" id=\"opone\" name=\"x\" placeholder=\"1\" required>
  </div>
  <div>
    <label for=\"optwo\">Second Operand</label>
    <input type=\"text\" id=\"optwo\" name=\"y\" placeholder=\"2\" required>
  </div>
  <div>
    <input type=\"submit\">
  </div>
</form>
"))

(defun adder (x y)
  (format nil "~a" (+ (parse-integer x)
                      (parse-integer y))))

(defroutes *app-routes*
  (GET "/"           ()    "Hello World")
  (GET "/meetings"   ()    (meetings))
  (GET "/add"        (x y) (adder x y))
  (GET "/calculator" ()    (format nil "<head><title>calculator</title></head>~a"
                                   (calculator)))

  ;; TODO Implement wild cards
  (GET "/people/:name" (name)
       (format nil "Hello ~a" (name))))

(defvar *app* (handler:site *app-routes*))

(defun stop-server ()
    (clack:stop *handler*))

(defun start-server ()
  (defparameter *handler* (clack:clackup *app*)))
