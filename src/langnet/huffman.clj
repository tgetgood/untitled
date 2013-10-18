(ns langnet.huffman
  (use [clojure.set :only [union]]))

;;;;; Basic Huffman tree a la SICP
;;;;; This data structure has been modified to fit your language


(defn make-leaf 
  "Create a new leaf for given symbol with given weight"
  [sym weight]
  {:leaf? true :symbol sym :weight weight})

(defn symbols
  "Extract symbols of node as a set"
  [tree]
  (if (tree :leaf?)
    #{(tree :symbol)}
    (tree :symbols)))

(defn make-tree
  "Combine two binary trees to make a new one by constructing a root node."
  [left right]
  {:leaf? false 
   :left left 
   :right right
   :symbols (union (symbols left) (symbols right))
   :weight (+ (left :weight) (right :weight))})

(defn decode
  "Decode vector of bits given a Huffman tree"
  [bits tree]
  (letfn [(choose-branch [bit tree]
            (cond (= bit 0) (tree :left)
                  (= bit 1) (tree :right)
                  :else (throw (new Exception (str "Expected 0 or 1 (i.e. a bit), but found \"" bit "\" instead!")))))
          (decode-1 [bits branch]
            (if (empty? bits)
                ()
                (let [next-branch (choose-branch (first bits) branch)]
                  (if (next-branch :leaf?)
                      (cons (next-branch :symbol) ; order is important here
                            (decode-1 (rest bits) tree)) 
                      (decode-1 (rest bits) next-branch)))))]
   (decode-1 bits tree))) 

(defn encode
  "Encode message according to the given Huffman tree."
  [message tree]
  (letfn [(encode-symbol [sym branch]
            (cond (branch :leaf?) 
                    ()
                  (contains? (symbols (branch :left)) sym) 
                    (cons 0 (encode-symbol sym (branch :left)))
                  (contains? (symbols (branch :right)) sym)
                    (cons 1 (encode-symbol sym (branch :right)))
                  :else 
                    (throw (new Exception (str "Trying to encode unknown symbol: \"" sym "\".")))))]
            
    (if (empty? message)
      []
      (concat (vec (encode-symbol (first message) tree))
              (encode (rest message) tree)))))

(defn generate-huffman-tree
  "Generate a huffman tree from a list of symbol-frequency pairs"
  [pairs]
  (let [leaves (map #(make-leaf (first %) (second %)) pairs)
        successive-merge (fn successive-merge [leaves]
                           (if (= 1 (count leaves))
                             (first leaves)
                             (let [sorted-leaves (sort #(< (%1 :weight) (%2 :weight)) leaves)]
                               (successive-merge (conj (drop 2 sorted-leaves) (make-tree (second sorted-leaves) (first sorted-leaves)))))))]
    (successive-merge leaves)))


(defn count-symbols
  "Create a map of symbol-frequency pairs from a string

  This is optimised slightly past the point of readability, but it needs to be
  fast..."
  [input]
  (persistent! (reduce #(assoc! %1 %2 ((fnil inc 0) (%1 %2))) (transient {}) input)))

;;;;; Test code

(def test-tree
  (make-tree (make-leaf 'A 4)
             (make-tree
             (make-leaf 'B 2)
                 (make-tree (make-leaf 'D 1)
                            (make-leaf 'C 1)))))

(def test-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(= test-message (encode (decode test-message test-tree) test-tree))

(generate-huffman-tree (count-symbols "aadbabca"))

