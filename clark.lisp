;; https://github.com/weavejester/compojure/wiki/Destructuring-Syntax
;; https://github.com/ring-clojure/ring/wiki/Concepts#requests
;;;; clark.lisp

(in-package #:clark)

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

(-> create-route (String (or List Symbol) T) List)
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
               (assure List-Of-Strings
                 (mapcar (lambda (arg)
                           (string-downcase (symbol-name arg)))
                         lambda-list)))
         ;; Kinda a hack to avoid evaluating the symbol
         `',lambda-list)
    ;; Create a lambda that evaluates the given expression while using
    ;; these given arguments as variables.
    '(lambda ,(cond
           ((listp lambda-list) lambda-list)
           ((symbolp lambda-list) (list lambda-list)))
      ;; If there are arguments attempt to declare their types
      ,(when (consp lambda-list)
         `(declare
           ,@(mapcar (lambda (arg)
                       `(type String ,arg))
                     lambda-list)))
      ,expression)))

(defun match-root-path (root-path)
  (lambda (route)
    (equal (caar route) root-path)))

(defun unwrap-paths (route)
  (let ((path-list (car route)))
    (if (null path-list)
        route                           ; root of this tree so the
                                        ; actual path is nil (since it
                                        ; should just be the root)
        (cons (car path-list)
              (list (list (cons nil (cdr route))))) ; if it's not the root unwrap it
        )))

(defun route-is-decomposed-p (inner-routes)
  (some (lambda (route)
          (format t "len ~a" (length (car route)))
          (> (length (car route))
             1))
        inner-routes))

(defun %break-routes-into-tree (routes-with-split-path)
  (let* ((sorted-routes
           ;; 3. Sort based on the first element of the list
           (sort routes-with-split-path #'string> :key #'caar))
         ;; - Iterate over the list finding each variation in the fist element
         (routes-at-level (remove-duplicates
                           (mapcar #'caar sorted-routes)
                           :test #'equal)))
    ;; For each root path
    (mapcar (lambda (path)
              ;; 7. cons the first element to the list
              (cons path
                    ;; 6. create a new list from the rest of the list
                    (list
                     (let ((lower-paths
                             ;; 5. mapcar remove the first element from the list (using cdr)
                             (mapcar (lambda (route)
                                       (cons (cdar route)
                                             (cdr route)))
                                     ;; filter the list to only those with the matching route
                                     (remove-if-not (match-root-path path)
                                                    sorted-routes))))
                       ;; If all routes have been decomposed
                       (if (route-is-decomposed-p lower-paths)
                           (progn
                             (format t "~a~%" lower-paths)
                             (%break-routes-into-tree lower-paths))
                           (progn
                             (format t "~a~%" lower-paths)
                             (mapcar #'unwrap-paths
                                     lower-paths)))))))
            routes-at-level)))

(defun break-routes-into-tree (route-per-method)
  "Takes all the routes per method as input e.g. For GET we
route-per-method = (\"/home/about\" (\"args\") (lambda))"
  (mapcar (lambda (x) `',x)
          (%break-routes-into-tree (mapcar (alexandria:compose
                                            ;; 2. split on /
                                            (lambda (route)
                                              (cons (let ((path (first route)))
                                                      (ppcre:split "/" path))
                                                    (cdr route)))
                                            #'eval)
                                           route-per-method))))
(defun define-all-routes (routes)
  (let ((formatted-routes (mapcar
                           (lambda (route)
                             (destructuring-bind (method path destructuring operation) route
                               (cons
                                ;; Used for matching against the possible methods
                                (assure HTTP-Method
                                  (intern (symbol-name method) "KEYWORD"))
                                (create-route path destructuring operation))))
                           routes)))
    ;; TODO Drop this likely unnecessary eval
    `',(eval (cons 'list
                   (alist-plist
                    (mapcar
                     (lambda (method)
                       (~>>
                        formatted-routes
                        ;; Match the routes against the stored method
                        (remove-if-not (lambda (route) (eql (car route) method)))
                        ;; Remove the method from each route since
                        ;; it is nolonger needed
                        (mapcar #'cdr)
                        ;; (format nil "~a")
                        (break-routes-into-tree)
                        ;; (format nil "~a")
                        ;; Ensure that the nested lists will be evaluated
                        ;; as a list and not a function call
                        (append `(,method list))
                        ))
                     +supported-methods+))))))

(defmacro routes (&body routes)
  (define-all-routes routes))

(defmacro defroutes (symbol &body routes)
  `(defparameter ,symbol ,(define-all-routes routes)))

#|
;;; Example usage
(routes
  (GET "/hello"      ()       "hello")
  (GET "/nope/world" ()       "Nope World")
  (GET "/nope/world/war" (v)       "Nope World")
  (GET "/nope/world/world" (w)       "Nope World")
  (GET "/fullthing"  env      (format nil "~a" env))
  (GET "/fullthing/are"  env      (format nil "~a" env))
  (GET "/nope"       (x)      (format nil "~a" x))
  (GET "/long"       (x y z)  (format nil "~a"
				      (+ (parse-integer x)
					 (parse-integer y)
					 (parse-integer z)))))

;; or
|#

(defroutes tmp
  (GET "/hello"      ()       "hello")
  (GET "/nope/world" ()       "Nope World")
  (GET "/nope/world/war" (v)       "Nope World")
  (GET "/nope/world/world" (w)       "Nope World")
  (GET "/fullthing"  env      (format nil "~a" env))
  (GET "/fullthing/are"  env      (format nil "~a" env))
  (GET "/nope"       (x)      (format nil "~a" x))
  (GET "/long"       (x y z)  (format nil "~a"
				      (+ (parse-integer x)
					 (parse-integer y)
					 (parse-integer z)))))

#|
;;; Generated code expected by the handler
'(:GET
 ((""
   (("nope"
     (("world"
       ((NIL NIL #<FUNCTION (LAMBDA ()) {53466D1B}>)
        ("war" ("v") #<FUNCTION (LAMBDA (V)) {534104CB}>)
        ("world" ("w") #<FUNCTION (LAMBDA (W)) {5343CF9B}>)))
      (NIL ((NIL ("x") #<FUNCTION (LAMBDA (X)) {5344FE7B}>)))))
    ("long" ((NIL ("x" "y" "z") #<FUNCTION (LAMBDA (X Y Z)) {5340D47B}>)))
    ("hello" ((NIL NIL #<FUNCTION (LAMBDA ()) {53461F1B}>)))
    ("fullthing"
     ((NIL ENV #<FUNCTION (LAMBDA (ENV)) {53423A6B}>)
      ("are" ENV #<FUNCTION (LAMBDA (ENV)) {53423CBB}>))))))
 :POST NIL :PUT NIL :DELETE NIL)
|#

(defun %lookup-route (split-path route-tree)
  (let ((matched-tree (find (car split-path)
                            route-tree
                            :key #'car :test #'equal)))
    (if matched-tree
        (progn (format t "~a~&" (second split-path))
               (format t "~a~&" (car (car (cadr matched-tree))))
               (if (not (and matched-tree (cdr split-path)))
                   (cadr matched-tree)
                   (%lookup-route (cdr split-path) (cadr matched-tree)))))))

(defun lookup-route (path route-tree)
  (find nil
        (%lookup-route (ppcre:split "/" path)
                       route-tree)
        :key #'car))

