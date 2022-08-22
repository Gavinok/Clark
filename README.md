# Compojure-Clone
### _Gavin Jaeger-Freeborn_

This project is a clone [Compojure](https://github.com/weavejester/compojure)
brought to Common Lisp. Yes this is yet another Clack based routing
library. Still in the vary early stages and not recommended for any
serious use.

### Example Code

This is the basic concept for how routes will be defined. The full
integration with Clack is still being figured out as well.

```lisp
(defroutes tmp
  (GET "/hello"      ()       "hello")
  (GET "/nope/world" ()       "Nope World")
  ;; curl http://localhost:3000/fullthing
  ;; passes the entire clack environment
  (GET "/fullthing"  env      (format nil "~a" env))
  ;; curl http://localhost:3000/nope?x=10
  (GET "/nope"       (x)      (format nil x))
  ;; curl http://localhost:3000/long?x=10&y=1&z=20
  (GET "/long"       (x y z)  (format nil "~a"
                                      (+ (parse-integer x)
                                         (parse-integer y)
                                         (parse-integer z)))))
```

For a small example project please see the example directory

## License

MIT

