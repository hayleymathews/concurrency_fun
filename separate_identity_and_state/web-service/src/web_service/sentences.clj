(ns web-service.sentences
    (:require [clojure.string :refer [trim]]))
  
(defn sentence-split [text]
    (map trim (re-seq #"[^\.!\?:;]+[\.!\?:;]*" text)))
  
(defn is-sentence? [text]
    (re-matches #"^.*[\.!\?:;]$" text))
  
(defn sentence-join [x y]
    (if (is-sentence? x) y (str x " " y)))
  
(defn strings->sentences [strings]
    (filter is-sentence? 
        (reductions sentence-join 
            (mapcat sentence-split strings))))