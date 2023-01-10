(ns lisp1.5
  (:require  [clojure.tools.trace :as trace])
  (:gen-class))

(trace/trace-ns 'lisp1.5)

;; Equivalent to clojure assoc
(defn pairlis
  "Binds the pairs formed by x and y elements into the environment a"
  [x y a]
  (if (empty? x)
    a
    (cons [(first x) (first y)] (pairlis (rest x) (rest y) a))))

;;(pairlis [:a :b :c] [:u :v :w]
;;         [[:d :x] [:e :y]])
;; => ([:a :u] [:b :v] [:c :w] [:d :x] [:e :y])

;; Equivalent to a clojure get
(defn assoc-lisp [x a]
  (assert (not-empty a))
  (if (= x (ffirst a))
    (first a)
    (assoc-lisp x (rest a))))

(assoc-lisp :b [[:a [:m :n]]
                [:b '(car x)]
                [:c '(quote m)]
                [:c '(rest x)]])

(defn atom-lisp [e]
  (and (not (coll? e))
       (not (fn? e))))

(declare eval-lisp)
(declare apply-lisp)

;; Similar to clojure cond
(defn evcon
  [c a]
  (if (= :nil (eval-lisp (ffirst c) a))
    (evcon (rest c) a)
    (eval-lisp (first (rest (first c))) a)))


;; evaluate a list
(defn evlis
  [m a]
  (if (empty? m) nil
      (cons (eval-lisp (first m) a) (evlis (rest m) a))))

;; similar to eval
(defn eval-lisp
  [e a]
  (cond
    (number? e) e
    (atom-lisp e) (second (assoc-lisp e a))
    (atom-lisp (first e)) (cond
                            (= (first e) :quote) (second e)
                            (= (first e) :cond) (evcon (rest e) a)
                            :else (apply-lisp (first e) (evlis (rest e) a) a))
    :else (apply-lisp (first e) (evlis (rest e) a) a)))

(defn apply-lisp
  ;; f is function
  ;; x is args
  ;; a is var environment
  [f x a]
  (cond
    (atom-lisp f) (cond
                    (= f :car) (ffirst x)
                    (= f :cdr) (rest (first x))
                    (= f :cons) [(first x) (first (rest x))]
                    (= f :atom) (atom-lisp (first x))
                    (= f :eq) (= (first x) (second x)))
    (= (first f) :lambda) (eval-lisp (first (rest (rest f)))
                                     (pairlis (second f) x a))
    (= (first f) :label) (apply-lisp (first (rest (rest f)))
                                     x
                                     (cons (cons (second f)
                                                 (nth f 3)
                                                 )
                                           a))))

(def var-env [[:a [:m :n]]
              [:b '(car x)]
              [:n :nil]
              [:c '(quote m)]
              [:c '(rest x)]])

(eval-lisp :b var-env)
(eval-lisp [:quote :b] var-env)

(evcon [[:n :c] [:b :a]] var-env)
(eval-lisp [:cond [:n :c] [:b :a]] var-env)

(eval-lisp :a var-env)
(eval-lisp [:car :a] var-env)
(eval-lisp [:cdr :a] var-env)
(eval-lisp [:cons [:quote 4] [:quote [8]]] var-env)
(eval-lisp [:cons [:quote 4] [:quote [8]]] [])
(eval-lisp [:atom [:cons [:quote 4] [:quote [8]]]] var-env)
(eval-lisp [:atom [:quote 4]] var-env)
(eval-lisp [:eq [:quote 4] [:quote 4]] var-env)
(eval-lisp [:eq [:quote 4] [:quote 7]] var-env)
(eval-lisp [[:lambda [:x :y] [:cons :y :x]] :a :b] var-env)

;; Labels not working yet.
(eval-lisp [[:label :my-f [:x :y] [:cons :y :x]]
            [:my-f :a :b]] var-env)

(defn -main
  [& args]
  (let [program [[:lambda [:x :y] [:cons :y :x]] :a :b]
        var-env [[:a [:m :n]]
                 [:b '(car x)]
                 [:n :nil]
                 [:c '(quote m)]
                 [:c '(rest x)]]]
    (println "Eval\n" program "\nin environment\n" var-env "\n=>\n" (eval-lisp program var-env))))


(eval-lisp (read-string "[:cons 5 8]") [])
(eval-lisp (read-string "[:cons [:quote 4] [:quote [8]]]") [])
(eval-lisp (read-string "[:cons [:quote 4] [:quote 8]]") [])
