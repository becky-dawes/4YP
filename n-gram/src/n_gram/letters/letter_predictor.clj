(ns n-gram.letters.letter-predictor (:require [n-gram.misc.misc-functions :refer :all]
                                              [n-gram.letters.file-reader :refer :all]
                                              [n-gram.letters.letter-maker :refer :all]))

(defn find-letter-pair "Finds all letter pairs starting with given letter" [letter] 
  (zipmap (map key counts-ASCII-2) (map #(if (= (str(first (key %))) letter)
                                           (val %) 0.0) counts-ASCII-2)))

(def find-letter-pair-memo "Memoized find-letter-pair" (memoize find-letter-pair))

(defn find-letter-trio "Finds all letter trios starting with given letter pair" 
  [l1 l2] (zipmap (map key counts-ASCII-3) (map #(if (= (str(first (key %))) l1) 
                          (if (= (str(second (key %))) l2) (val %) 0.0) 0.0) counts-ASCII-3)))

(def find-letter-trio-memo "Memoized find-letter-trio" (memoize find-letter-trio))


(defn next-letter "Predicts next letter in sequence" 
  [letters] (let [letters (clojure.string/lower-case letters)] 
              (if (= (count letters) 1) (key (apply max-key val (find-letter-pair-memo letters)))
                           (key (apply max-key val (find-letter-trio-memo (str (first letters)) 
                                                                 (str (second letters))))))))

(def next-letter-memo "Memoized next-letter" (memoize next-letter))