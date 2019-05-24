(ns design-thoughts.core)

(conj #{} 2 3 4 5)

(map vector [1 2 3 4] (range 30 40) [:a :b :c :d])

;; datomic example
(comment
  (d/transact db data)
  (d/entity db id)
  ;; we could invent `ipartial` which would apply arguments from the end but it's kinda hairy
  ;; (if entity took db as the last argument)
  (map (ipartial d/entity db) [id1 id2 id3])
  
  ;; BUT instead of using `ipartial` we can just use ->>
  (->> (get-companies db)
       first
       (map (partial get-users db)))

  ;; don't use `partial` too much because you'll confuse your users
  (def get-my-users (partial get-users config my-company))
  (get-my-users 43)

  )


