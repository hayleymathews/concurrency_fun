(ns philosophers.util)

(defn swap-when! 
    "If (pred current-value-of-atom) is true, atomically swaps the value
    of the atom to become (apply f current-value-of-atom args). Note that
    both pred and f may be called multiple times and thus should be free
    of side effects. Returns the value that was swapped in if the
    predicate was true, nil otherwise."
    [a pref f & args]
    (loop []
      (let [old @a]
        (if (pred old)
          (let [new (apply f old args)]
            (if (compare-and-set! a old new)
              new
              (recur)))
        nil))))