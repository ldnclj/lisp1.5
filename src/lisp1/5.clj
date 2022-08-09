(ns lisp1.5
  (:require  [clojure.tools.trace :as trace])
  (:gen-class))

(trace/trace-ns 'lisp1.5)

;; Equivalent to clojure assoc
(defn pairlis
  [x y a]
  (if (empty? x)
    a
    (cons [(first x) (first y)] (pairlis (rest x) (rest y) a))))


(pairlis [:a :b :c] [:u :v :w] [[:d :x] [:e :y]])
;; => ([:a :u] [:b :v] [:c :w] [:d :x] [:e :y])


;; Equivalent to a get
(defn assoc-lisp [x a]
  (if (= x (ffirst a))
    (first a)
    (assoc-lisp x (rest a))))

(assoc-lisp :b [[:a [:m :n]] [:b '(car x)] [:c '(quote m)] [:c '(rest x)]])

(defn atom-lisp [e]
  (and (not (coll? e))
       (not (fn? e))))

(declare eval-lisp)

;; Similar to clojure cond
(defn evcon
  [c a]
  (if (= :nil (eval-lisp (ffirst c) a))
    (evcon (rest c) a)
    (eval-lisp (first (rest (first c))) a)))

(defn eval-lisp
  [e a]
  (cond
    (atom-lisp e) (second (assoc-lisp e a))
    (atom-lisp (first e)) (cond
                            (= (first e) :quote) (second e)
                            (= (first e) :cond) (evcon (rest e) a)
                            ;:else (apply-lisp (firs))
                            )
    ))


(def var-env [[:a [:m :n]] [:b '(car x)] [:n :nil] [:c '(quote m)] [:c '(rest x)]])

(eval-lisp :b var-env)
(eval-lisp [:quote :b] var-env)
(evcon [[:n :c] [:b :a]] var-env)
(eval-lisp [:cond [:n :c] [:b :a]] var-env)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
