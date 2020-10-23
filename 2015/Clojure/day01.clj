
(def input (clojure.string/trim-newline (slurp "../data/01.in")))

(defn par [x y]
  (case y
    \( (+ 1 x)
    \) (- x 1)))

(defn part-a [input]
  (reduce par 0 input))

(defn part-b [input]
  (->> input
       (reductions par 0)
       (take-while #(<= 0 %))
       count))

(part-a input)
(part-b input)
