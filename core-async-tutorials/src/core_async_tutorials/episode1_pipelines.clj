(ns core-async-tutorials.episode1-pipelines
  "Examples from the first Core.Async episode - Pipelines.
  See https://www.youtube.com/watch?v=k6zbfb84yIM"
  (:require [clojure.core.async :as async :refer [<!! >!!]]))

;;; We wan't to define a pipeline as pairs of <processes count, operation>
;;; c - input channel (if we used ">" we'd expect the last argument to be output channel
;;

;; this implementation just chain channels together - it's not really parallel
;; although it's pretty efficient
(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    ;; we can built pipeline fairly easily with reduce
    (reduce
     ;; prev-c is previous channel, n number of processes, f is actual function
     (fn [prev-c [n f]]
       (async/map< f prev-c))
     c ; start with input channel
     p ; go over each of the pairs
)))


;; let's try another version - still only one process
;; Tip: look at the code of `async/merge`
(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    ;; we can built pipeline fairly easily with reduce
    (reduce
     ;; prev-c is previous channel, n number of processes, f is actual function
     (fn [prev-c [n f]]
       (-> (for [_ (range n)]
             (async/map< f prev-c))
           ;; merge takes a sequence of channels and returns a channel containing
           ;; all the values from input channel
           async/merge))
     c ; start with input channel
     p ; go over each of the pairs
     )))


;; look at async/pipe - spins out a separate process!
#_(async/pipe in out)

(defn to-proc< [in]
  (let [out (async/chan 1)]
    (async/pipe in out)
    out))

(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    ;; we can built pipeline fairly easily with reduce
    (reduce
     ;; prev-c is previous channel, n number of processes, f is actual function
     (fn [prev-c [n f]]
       (-> (for [_ (range n)]
             (-> (async/map< f prev-c)
                 to-proc<))
           ;; merge takes a sequence of channels and returns a channel containing
           ;; all the values from input channel
           async/merge))
     c ; start with input channel
     p ; go over each of the pairs
     )))

(let [c (async/chan 10)]
  (>!! c 42)
  (<!! (pipeline< [4 inc
                   1 inc
                   2 dec
                   3 str]
                  c)))
;;=> "43"

