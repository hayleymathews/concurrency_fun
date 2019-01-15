(ns polling.core
  (:gen-class))


(defn poll-fn [interval action]
  (let [seconds (* interval 1000)]
    (go (while true
      (action)
      (<! (timeout seconds))))))

(defmacro poll [interval & body]
  `(let [seconds# (* ~interval 1000)]
    (go (while true
      (do ~@body)
      (<! (timeout seconds#))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
