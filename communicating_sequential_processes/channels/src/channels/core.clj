(ns channels.core
  (:require [clojure.core.async :as async :refer :all
   :exclude [map into reduce merge partition partition-by take]]))

(defn readall!! [ch]
  (loop [coll []]
    (if-let [x (<!! ch)]
      (recur (conj coll x))
      coll)))

(defn writeall!! [ch coll]
  (doseq [x coll]
    (>!! ch x))
    (close! ch))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


