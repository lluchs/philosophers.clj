(ns philosophers.core
  (:gen-class))

; Synchronizes output
(def printer (agent nil))
(defn puts [& args] (send printer (fn [_] (apply println args))))

; Maximum sleeping and eating time.
(def max-sleep 10)

(defn philosopher
  "Simulates a single philosopher with the given forks."
  [n fork1 fork2]
  (while true
    (let [thinking-length (rand-int max-sleep)
          eating-length (rand-int max-sleep)]
      (puts "Philosopher" n "thinking for" thinking-length "ms.")
      (Thread/sleep thinking-length)
      (locking fork1
        (puts "Philosopher" n "has his left fork.")
        (locking fork2
          (puts "Philosopher" n "has his right fork.")
          (puts "Philosopher" n "eating for" eating-length "ms.")
          (Thread/sleep eating-length))))))

(defn philosophers-simulation
  "Runs the simulation for n philosophers."
  [n]
  (let [forks (vec (repeatedly n #(Object.)))]
    (doseq [i (range n)]
      (.start (Thread. #(philosopher i
                                     (nth forks i)
                                     (nth forks (mod (inc i) n))))))))

(defn philosophers-simulation-fixed
  "Variant without deadlocks by sorting the forks."
  [n]
  (let [forks (vec (repeatedly n #(Object.)))]
    (doseq [i (range n)]
      (let [fork1 i
            fork2 (mod (inc i) n)]
        (.start (Thread. #(philosopher i
                                       (nth forks (min fork1 fork2))
                                       (nth forks (max fork1 fork2)))))))))

(defn -main [& args]
  (let [sim (condp = (first args)
              "standard" philosophers-simulation
              "fixed"    philosophers-simulation-fixed
              (fn [_] (println "Usage: philosophers <standard or fixed> [number of philosohpers]")))
        n (Integer/parseInt (nth args 1 "5"))]
    (sim n)))
