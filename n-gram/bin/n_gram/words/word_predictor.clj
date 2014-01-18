(ns n-gram.words.word-predictor (:require [n-gram.words.file-reader :refer :all]
                                          [n-gram.words.word-maker :refer :all]
                                          [n-gram.misc.misc-functions :refer :all]
                                          [clojure.string :refer [lower-case]]))



;(def N "number of words in text" n-gram.core/total-word-count)

;(def K 1000000)



(defn find-pair "Finds all word pairs starting with given word" [w] (zipmap (map key counts-2)
                                           (map #(if (= (first (key %)) w) (val %) 0.0) counts-2)))

(def find-pair-memo "Memoized find-pair" (memoize find-pair))

(defn find-trio "Finds all word trios starting with given word pair" 
  [w1 w2] (zipmap (map key counts-3) (map #(if (= (first (key %)) w1) 
                                            (if (= (second (key %)) w2) (val %) 0.0) 0.0) counts-3)))

(def find-trio-memo "Memoized find-trio" (memoize find-trio))

(defn next-word "Predicts next word in sequence" 
  ([word1] (let [word1 (if (= 0 (get-count-memo counts-1 [(lower-case word1)]))
                         unknown (lower-case word1))]
             (last (key (apply max-key val (find-pair-memo word1))))))
  ([word1 word2] (let [word1 (if (= 0 (get-count-memo counts-1 [(lower-case word1)]))
                               unknown (lower-case word1)) word2 
                       (if (= 0 (get-count-memo counts-1 [(lower-case word2)])) 
                         unknown (lower-case word2))] 
                   (last (key (apply max-key val (find-trio-memo word1 word2)))))))

(def next-word-memo "Memoized next-word" (memoize next-word))

;(defn get-last-two "Returns last two words of sequence"
  ;[theWords] [ (last (butlast theWords))  (last theWords)])

;(def get-last-two-memo "Memoized get-last-two" (memoize get-last-two))

(defn loop-next-words "Predicts certain length of text"
  [word1 word2 n] (let [the-next-word (next-word-memo word1 word2) ](if (< 0 n)  
                                (cons word2 
                                          (loop-next-words word2 the-next-word 
                                             (- n 1))) 
                    (cons word2 [the-next-word]))))

(def loop-next-words-memo "Memoized loop-next-words" (memoize loop-next-words))

(defn join-words "Joins input words together into one string"
  [theWords] (if (< 0 (count theWords)) (let [word (str (first theWords) " ")]
                                          (str word (join-words (rest theWords)))) 
                                             (str (first theWords))))

(def join-words-memo "Memoized join-words" (memoize join-words))

(defn predict-text "Predicts a certain length of text based on context"
  [context n] (if (< 0 n) (str (join-words-memo context)
                               (join-words-memo (loop-next-words (last context)
                                                                 (next-word-memo (last (butlast context)) (last context)) (- n 2))))))

(def predict-text-memo "Memoized predict-text" (memoize predict-text))