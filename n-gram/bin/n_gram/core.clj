(ns n-gram.core)



; read in file

(def txt (slurp "the-wonderful-wizard-of-oz.txt"))



; split tokens at whitespace (reg. expr.)

(def words (clojure.string/split txt #"\s"))



; count word frequencies

(def h (frequencies words))



; normalize word frequencies by total count

(def nh (zipmap (map key h) (map #(/ % (count words)) (map val h))))



; sort types (ascending) by token count

(sort-by second nh)





(defn cumsum

   "With one arg x returns lazy cumulative sum sequence y with (= (nth y) (cumsum x))

    With two args t, x returns lazy cumulative sum sequence y with (= (nth y) (+ t (cumsum x)))"

  ([x] (cumsum 0 x))

  ([t x] (if (empty? x) ()           

           (let [y (+ t (first x))]

             (cons y ( lazy-seq (cumsum y (rest x))))))))



(def ai  (cumsum (repeat (- (count words) 1) 1)))



; slow and inefficient but interesting to understand

;(defn combine [lst1 lst2]

;  (mapcat (fn [x] (map #(list % x) lst1)) lst2))



(def wp (interleave  (map #(nth words (- % 1)) ai) (map #(nth words %) ai)))



(defn pairs [x y] 

 (if (not= (count x) 0) 

   (cons (list (first x) (first y)) (lazy-seq (pairs (rest x) (rest y))))

   (list)))



(sort-by second (frequencies (pairs (map #(nth words (- % 1)) ai) (map #(nth words %) ai))))