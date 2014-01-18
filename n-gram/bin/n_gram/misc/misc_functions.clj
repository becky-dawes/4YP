(ns n-gram.misc.misc-functions)



(defn cumsum

   "With one arg x returns lazy cumulative sum sequence y with (= (nth y) (cumsum x))

    With two args t, x returns lazy cumulative sum sequence y with (= (nth y) (+ t (cumsum x)))"

  ([x] (cumsum 0 x))

  ([t x] (if (empty? x) ()           

           (let [y (+ t (first x))]

             (cons y ( lazy-seq (cumsum y (rest x))))))))

(defn get-count "Returns 0 if word not found, otherwise returns count"[func args] 
  (if (nil? (func args)) 0 (func args)))

(def get-count-memo (memoize get-count))

(defmacro generate-variable "Generates a variable with the given name and value" [name value]
  (let [new-value [value]]
     (do `(def ~(symbol (str (symbol name))) 
            ~@new-value)))) 

(defn sum [x] (reduce + x))

(defn equal [num1 num2 accuracy] (letfn [(bignum [num] (.setScale (BigDecimal. num) accuracy BigDecimal/ROUND_DOWN))]
                                    (= 0 (.compareTo (bignum num1) (bignum num2)))))