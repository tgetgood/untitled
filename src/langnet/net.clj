(ns langnet.net
  (use incanter.core incanter.stats incanter.charts)
  (import (java.util Random))
) 

(defn generate-net
  "Generates a new neural net with the given topology.

  A topology is just sequence of layer sizes."
  [& topo]
  (let [r (new Random)
        generate-matricies (fn gm [t] 
                             (if (= 1 (count t))
                               []
                               (let [m (inc (first t))
                                   n (second t)
                                   nums (take (* m n) (repeatedly #(.nextDouble r)))
                                   mat (matrix nums n)]
                                 (conj (gm (rest t)) mat))))
        weights (-> topo generate-matricies reverse vec)]
    {:topology topo :weights weights}))

(defn sigmoid
  [x]
  (/ 1 (+ 1 (exp (* -1 x)))))

(defn run-net
  "Trigger firing action through net (deterministic sigmoid),"
  [net input]
  (reduce #(matrix-map sigmoid (mmult (->> %1 (cons 1) matrix trans) %2)) input (net :weights)))

(run-net (generate-net 3 4 5) (matrix [0 0 0] 3))
