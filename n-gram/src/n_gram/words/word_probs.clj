(ns n-gram.words.word-probs (:require [n-gram.misc.misc-functions :refer :all]
                                      [n-gram.words.file-reader :refer :all]
                                      [clojure.string :refer [lower-case]]))
  
(def M "Size of vocabulary" (count counts-1))

(def alpha1 "alpha for 1-gram" 1)

(def alpha2 "alpha for 2-gram" 1)

(def alpha3 "alpha for 3-gram" 1)

(defn p1 "1-gram probability" ([word1] (float (/ (+ (get-count-memo counts-1 [word1]) alpha1)
                                                 (+ N (* M alpha1)))))
 ([word1 word-counts] (float (/ (+ (get-count-memo word-counts [word1]) alpha1)
                                                (+ N (* M alpha1))))))

(def p1-memo "Memoized p1" (memoize p1))

(defn p2 "2-gram probability" ([word1 word2] (float (/ (/ (+ 
        (get-count-memo counts-2 [word1 word2]) alpha2) 
                                  (+ (- N 1) (* M M alpha2))) (p1-memo word1))))
  ([word1 word2 pair-counts] (float (/ (/ (+ 
        (get-count-memo pair-counts [word1 word2]) alpha2) 
                                  (+ (- N 1) (* M M alpha2))) (p1-memo word1)))))

(def p2-memo "memoized p2" (memoize p2))

(defn p3 "3-gram probability" ([word1 word2 word3]
  (float (/ (/ (+ (get-count-memo counts-3 [word1 word2 word3]) alpha3)
               (+ (- N 2) (* M M M alpha3))) (p2-memo word1 word2))))
  ([word1 word2 word3 trio-counts]
  (float (/ (/ (+ (get-count-memo trio-counts [word1 word2 word3]) alpha3)
               (+ (- N 2) (* M M M alpha3))) (p2-memo word1 word2)))))

(def p3-memo "memoized p3" (memoize p3))

(defn p "Returns n-gram probability depending on number of input words"
  ([word1] (let [word1 (lower-case word1)] (p1-memo word1)))
  ([word1 word2] (let [word1 (lower-case word1) word2 (lower-case word2)]
                   (p2-memo word1 word2)))
  ([word1 word2 word3] (let [word1 (lower-case word1) 
                             word2 (lower-case word2) 
                             word3 (lower-case word3)] (p3-memo word1 word2 word3))))

(def p-memo (memoize p))