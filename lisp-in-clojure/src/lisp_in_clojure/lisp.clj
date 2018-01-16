(ns lisp-in-clojure.lisp
  (:require [clojure.walk :as walk]
            [clojure.core :as clj])
  (:refer-clojure :exclude [eval list cons apply load-file])
  (:import (java.io Writer)))

(declare eval)
(declare get-global)

;;; car - contents of address register
(defrecord Cons [car cdr])

;;; what we need to support for priinting?
;;; 1. we have to support list in a tail position: `(cons 1 (cons 2 nil))` printed as `(1 2)`
;;; 2. we need to support cons cells: `(cons 1 2)` printed as `(1 . 2)`

(defmethod print-method Cons
  [^Cons c ^Writer w]
  (do (.write w "(")
      (loop [^Cons c c]
        (cond
          (nil? c)
          (.write w ")")

          (nil? (.-cdr c))
          (do (.write w (format "%s)"
                                (pr-str (.-car c)))))

          (instance? Cons (.-cdr c))
          (do (.write w (format "%s "
                                (pr-str (.-car c))))
              (recur (.-cdr c)))

          :else
          (.write w (format "%s . %s)"
                            (pr-str (.-car c))
                            (pr-str (.-cdr c))))))))

(println (->Cons 1 (->Cons 2 nil)))
(println (->Cons 1 2))

;;; helpers
(defn cons
  ([v]
   (->Cons v nil))
  ([car cdr]
   (->Cons car cdr)))

(defn car [^Cons c]
  (.-car c))

(defn cdr [^Cons c]
  (.-cdr c))

(defn cons? [x]
  (instance? Cons x))

(defn bind
  "Binds symbol to value as a pair
  prepended to the given environment."
  [env sym val]
  (->Cons (->Cons sym val) env))

(bind nil :k 42)
(-> nil
    (bind :k 42)
    (bind :l 3))

(defn list [& args]
  (reduce (fn [tail head]
            (cons head tail))
          nil
          (reverse args)))

(list 1 2 3)
;; show that this is not the Clojure list
(type (list 1 2 3))

(defn lookup
  "Lookups symbol's value in given environment."
  [env sym]
  (if env
    (let [head (car env)]
      (if (= (car head) sym)
        (cdr head)
        (recur (cdr env) sym)))
    (get-global sym)))

(lookup (bind nil 'k 3) 'k)

(defn new-env
  "Creates a new environment"
  [& args]
  (reduce
   (fn [env [car cdr]]
     (bind env car cdr))
   nil
   (partition 2 args)))

(new-env 'k 1 'l 3)


;;; we need globals still!
;;; this is an internal structure for tracking global vars
(def globals (atom nil))

(defn map-clojure-syms [syms]
  (zipmap
   syms
   (map resolve syms)))

(defn def-global [sym val]
  (swap! globals assoc sym val))

(defn get-global [sym]
  (let [result (get @globals sym ::not-found)]
    (if (identical? result ::not-found)
      ;; Note: `ex-info` is not used a lot in Clojure and it should be
      (throw (ex-info "Global not Found"
                      {:global sym :env (sort (keys @globals))}))
      result)))

(defn map-clojure-syms [syms]
  (zipmap
   syms
   (map resolve syms)))

;;; eval every item in the list and return the new list
;;; Notice that we don't want to do this in a loop
;;; because we don't want to reverse arguments and functions
;;; -> we want to evaluate them in order
;;; We could use lazy lists and maps, etc. but we're using our Cons cell.
(defn eval-list [env lst]
  (when lst
    (cons (eval env (car lst))
          (eval-list env (cdr lst)))))

;; convert cons back to Clojure's list
(defn cons->list [s]
  (if s
    (conj (cons->list (cdr s)) (car s))
    '()))

;; functions in our lisp will be clojure functions so we can use Clojure's apply
(defn apply [f args]
  (clj/apply f (cons->list args)))

(defn eval-apply [env f args]
  (if (nil? f)
    (throw (ex-info "Cannot apply values to nil"
                    {:args args}))
    (let [lst (eval-list env args)]
      (apply f lst))))

;;; So far we covered the basics, so here's our eval

(defn make-lambda [env syms body]
  (fn lambda [& args]
    (let [env (loop [syms syms
                     args args
                     env env]
                (if syms
                  (recur (cdr syms)
                         (next args)
                         (bind env (car syms) (first args)))
                  env))]
      (eval env body))))

(defn eval-do [env form]
  (if (cdr form)
    (do
      (eval env (car form))
      (recur env (cdr form)))
    (eval env (car form))))

;; notice that we lookup things in env we do so in order so any shadowing of symbols works out of the box
(defn eval-sexpr [env sym form]
  (cond

    (= sym 'if)
    (if (eval env (car form))
      (eval env (car (cdr form)))
      (eval env (car (cdr (cdr form)))))

    (= sym 'do)
    (eval-do env form)

    (= sym 'def)
    (let [sym (car form)
          val (eval env (car (cdr form)))]
      (def-global sym val)
      val)

    (= sym 'quote)
    (car form)

    (= sym 'fn)
    (let [args (car form)
          body (cons 'do (cdr form))]
      (make-lambda env args body))

    (= sym 'cond)
    (loop [form form]
      (let [pred-expr (car form)
            body-expr (car (cdr form))
            next-expr (cdr (cdr form))]
        (if form
          (if (eval env env pred-expr)
            (eval env body-expr)
            (recur next-expr))
          nil)))

    (= sym 'let)
    (let [binds (car form)
          body (cdr form)
          env (loop [env env
                     binds binds]
                (if (nil? binds)
                  env
                  (recur (bind env (car binds) (eval env (car (cdr binds))))
                         (cdr (cdr binds)))))]
      (eval-do env body))

    :else
    (eval-apply env (eval env sym) form)))

(defn eval [env form]
  (cond
    (cons? form)
    (if (symbol? (car form))
      (eval-sexpr env (car form) (cdr form))
      (eval-apply env (eval env (car form)) (cdr form)))

    (symbol? form)
    (lookup env form)

    :else form))

(defn clj->lisp [form]
  (walk/postwalk
   (fn [f]
     (if (sequential? f)
       (clj/apply list f)
       f))
   form))

(defmacro eval-forms [& forms]
  `(do (reset-globals)
       (last (for [form# (quote ~forms)]
               (do (println form#)
                   (eval nil (clj->lisp form#)))))))

(defn read-file [file]
  ;; here we call `clj->lisp` so all clojure specific data structures are convered to LISP
  ;; that's why vectors (square brackets) work in lisp_tests.clj
  (clj->lisp (read-string (str "(do " (slurp file) ")"))))

(defn load-file [file]
  (eval nil (read-file file)))

(defn die [msg & args]
  (throw (ex-info msg (clj/apply hash-map args))))

(defn reset-globals []
  (reset! globals (map-clojure-syms
                   ;; we want to make our LISP reasonably simple - no hashmaps for example
                   '#{println + * - / < > <= >= = dec inc car cdr cons cons? symbol? apply
                      load-file nil? vararg read-file die})))

(defn eval-file [file]
  (reset-globals) ; hack but it's simple
  (load-file file))

(comment

  ;;; eval examples
  (eval nil 1)
  (eval nil 'x)
  ;; when the first thing in form is a symbol: this will eventually call `eval-sexpr`
  (eval (new-env 'x 42) '(def foo 42))
  (eval (new-env 'x 42) '(def foo (fn [x] x)))
  (eval (new-env 'x 42) (clj->lisp '(do (def foo (fn [x] (fn [] x)))
                                        (foo 42))))


  (eval-forms
   (def square (fn [x]
                 (* x x)))
   (square 42 42))

  (eval-forms
   (def fib
     (fn [n]
       (if (<= n 1)
         n
         (+ (fib (dec n))
            (fib (- n 2))))))
   (fib 12))

  (eval-file "src/lisp_in_clojure/lisp_tests.clj")

  )
