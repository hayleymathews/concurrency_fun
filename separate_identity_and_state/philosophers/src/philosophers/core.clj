(ns philosophers.core
  (:gen-class))

(def philosophers (into [] (repeatedly 5 #(ref :thinking))))

(defn think []
  (Thread/sleep (rand 1000)))

(defn eat []
  (Thread/sleep (rand 1000)))

(defn now []
  (quot (System/currentTimeMillis) 1000))

(defn release-chopsticks [philosopher]
  (dosync (ref-set philosopher :thinking)))

(defn claim-chopsticks [philosopher left right]
  (dosync
    (when (and 
            (= (ensure left) :thinking) 
            (= (ensure right) :thinking))
      (ref-set philosopher :eating))))

(defn philosopher-thread [n]
  (Thread.
    #(let [philosopher (philosophers n)
           left (philosophers (mod (- n 1) 5))
           right (philosophers (mod (+ n 1) 5))
          ]
      (while true
        (think)
        (when (claim-chopsticks philosopher left right)
          (println (str "philosopher: " n " begin eating " (now)))
          (eat)
          (println (str "philosopher: " n " done eating " (now)))
          (release-chopsticks philosopher))))))

(defn -main [& args]
  (let [threads (map philosopher-thread (range 5))]
    (doseq [thread threads] (.start thread))
    (doseq [thread threads] (.join thread))))
