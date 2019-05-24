(ns design-thoughts.02-case
  "Design Thoughts - Ep2 - The Case Against Case Exprs.
  https://tbaldridge.pivotshare.com/media/design-thoughts-ep2-the-case-against-case-exprs/76465/feature"
  (:require [clojure.set :as set]))


;; value-based dispatch it's what it really is
(defn speak [x]
  (case x
   :cat "meow"
   :dog "bark"
   :fish "bubble"))

(speak :cat)
(speak :dog)
(speak :fish)

;; we now have a problem: "What are the valid inputs for this function?"
;; pseudo-spec
#_(speak #{:cat :dog :fish} '=> string?)
;; the problem is that we now duplicate everything from our fn implementation

;; this type of system doesn't support machine-level introspection


;; => let's do it other way instead:
(def options {:cat "meow"
              :dog "bark"
              :fish "bubble"
              :cow "moo"})
(defn speak [x]
  (let [val (get options x ::not-found)]
    (assert (not (identical? val ::not-found))
            "Invalid option")))
;; now we can use `options` in "spec" - nothing is duplicated
#_(speak (keys options) '=> (vals options))

(def options-invert (set/map-invert options))

(defn who-said? [word]
  (options-invert word))

(who-said? "moo")

;; now you can also create a generative test for speak

;; also much better error reporting!
;; this is very helpful in all sorts of system
(defn speak [x]
  (let [val (get options x ::not-found)]
    (assert (not (identical? val ::not-found))
            (str "Invalid option " x "; valid options: " (set (keys options))))))
#_(speak 42)


;; What if you need this solution to be really fast?
(defn make-expr [options]
  `(fn [x#]
     (case x#
       ~@(mapcat identity options))))
(mapcat identity options)
(make-expr options)
;; we can either define `make-expr` as macro or use `eval` (nothing wrong with that!)
(def speak (eval (make-expr options)))
;; we can even invert!
(def speak-invert (eval (make-expr options-invert)))
;; we can also wrap this in delay (if you're not sure it's going to be used that often)
(def speak-delayed (delay (eval (make-expr options-invert))))
(@speak-delayed "meow")

