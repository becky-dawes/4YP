(ns n-gram.words.good-turing (:require [n-gram.misc.misc-functions :refer :all]
                                       [n-gram.words.file-reader :refer :all]))

(defn generate-counts-of-counts "Generates a map of counts of counts" [input-counts]
  (frequencies (map val input-counts)))

(def generate-counts-of-counts-memo "Memoized generate-counts-of-counts" (memoize generate-counts-of-counts))

;(def counts-of-word-counts "1-gram counts-of-counts map" (generate-counts-of-counts counts-1 1 "counts-of-word-counts"))

;(defn generate-cumulative-sum "Generates sequence of cumulative sum of items in map" [input-map]
;  (cumsum (repeat (count input-map) 1)))
;
;(def generate-cumulative-sum-memo "Memoized generate-cumulative-sum" (memoize generate-cumulative-sum))
;
;(defn generate-sorted-r-sequence "Generates sorted sequence of keys in map" [input-counts] 
; (map key (sort-by key input-counts)))
;
;(def generate-sorted-r-sequence-memo "Memoized generate-sorted-r-sequence" (memoize generate-sorted-r-sequence))




;1-gram maps

(def counts-of-counts-1 "1-gram counts-of-counts map" (generate-counts-of-counts-memo counts-1))

;(def cumsum-counts-of-counts-1 "Cumulative sum of all word counts" 
;  (generate-cumulative-sum-memo counts-of-counts-1))
;
;(def sorted-r-1 "1-gram r sequence" (generate-sorted-r-sequence-memo counts-of-counts-1))
;
;
;
;(def r-counts-keys-1 "1-gram map of counts and indices with counts as keys" 
;  (zipmap sorted-r-1 cumsum-counts-of-counts-1))
;
;(def r-index-keys-1 "1-gram map of counts and indices with indices as keys"
;  (zipmap cumsum-counts-of-counts-1 sorted-r-1))

;2-gram maps

(def counts-of-counts-2 "2-gram counts-of-counts map" (generate-counts-of-counts-memo counts-2))

;(def cumsum-counts-of-counts-2 "Cumulative sum of all pair counts" 
;  (generate-cumulative-sum-memo counts-of-counts-2))
;
;(def sorted-r-2 "2-gram r sequence" (generate-sorted-r-sequence-memo counts-of-counts-2))
;
;(def r-counts-keys-2 "2-gram map of counts and indices with counts as keys" 
;  (zipmap sorted-r-2 cumsum-counts-of-counts-2))
;
;(def r-index-keys-2 "2-gram map of counts and indices with indices as keys"
;  (zipmap cumsum-counts-of-counts-2 sorted-r-2))

;3-gram maps

(def counts-of-counts-3 "3-gram counts-of-counts map" (generate-counts-of-counts-memo counts-3))

;(def cumsum-counts-of-counts-3 "Cumulative sum of all pair counts" 
;  (generate-cumulative-sum-memo counts-of-counts-3))
;
;(def sorted-r-3 "3-gram r sequence" (generate-sorted-r-sequence-memo counts-of-counts-3))
;
;(def r-counts-keys-3 "3-gram map of counts and indices with counts as keys" 
;  (zipmap sorted-r-3 cumsum-counts-of-counts-3))
;
;(def r-index-keys-3 "3-gram map of counts and indices with indices as keys" 
;  (zipmap cumsum-counts-of-counts-3 sorted-r-3))


;Good-Turing methods

(defn r "Returns count of given n-gram" [n-gram n] (get-count-memo 
                                                     (resolve (symbol (str "counts-" n))) n-gram))

(def r-memo "Memoized r" (memoize r))

(defn n-r "Returns count of given count" [freq n] (if (> freq 0)
                                        ((resolve (symbol (str "counts-of-counts-" n))) freq) 
                                          (count (var-get (resolve (symbol (str "counts-" n)))))))

(def n-r-memo "Memoized n-r" (memoize n-r))

;(defn n-r-plus-one "Returns count of next biggest count" [freq n] 
;  (let [n-r-index (get-count-memo (resolve (symbol (str "r-counts-keys-" n))) freq)] 
;    (if (= n-r-index (count (var-get (resolve (symbol (str "counts-of-counts-" n)))))) 
;      (n-r-memo freq n) ((resolve (symbol (str "counts-of-counts-" n))) ((resolve (symbol (str "r-index-keys-" n))) (+ n-r-index 1))))))

(defn n-r-plus-one "Returns count of frequency plus one" [freq n]
  (if (=  (val (apply max-key val (var-get (resolve (symbol (str "counts-" n)))))) freq)
    ((resolve (symbol (str "counts-of-counts-" n))) freq)
  (get-count-memo (resolve (symbol (str "counts-of-counts-" n))) (inc freq))))

(def n-r-plus-one-memo "Memoized n-r-plus-one" (memoize n-r-plus-one))

(defn g-t "Returns Good-Turing count of given word" [n-gram n] 
  (let [freq (if (= (type n-gram) (type "")) (r-memo [n-gram] n) (r-memo n-gram n))]
    (* (+ freq 1) (/ (n-r-plus-one-memo freq n) (n-r-memo freq n)))))

(def g-t-memo "Memoized g-t" (memoize g-t))

(defn g-t-prob "Returns probability of a given n-gram using Good-Turing smoothing" [n-gram] 
  (let [n (if (= (type n-gram) (type "")) (count [n-gram]) (count n-gram))] 
    (if (> n 1)
      (let [g-t-n-minus-1 (g-t-memo (butlast n-gram) (dec n))]
        (if (zero? g-t-n-minus-1)
          "Inf"
          (float (/ (g-t-memo n-gram n) g-t-n-minus-1)) ))
      (float (/ (g-t-memo n-gram n) (count (var-get (resolve (symbol (str "counts-" n))))))))))
;<- don't think this is right
  
(def g-t-prob-memo "Memoized g-t-prob" (memoize g-t-prob))