(ns n-gram.letters.file-reader (:require [n-gram.misc.misc-functions :refer :all]
                                         [n-gram.letters.letter-maker :refer :all]
                                         [n-gram.words.file-reader :refer [formattedText]]))

(def all-chars "Map of all lower case characters, numbers and space" (map #(str %) (map char (concat (range 32 33 )(range 48 58) (range 97 123)  ))))

(def all-char-pairs "Map of all possible pairs of lower case characters, numbers and space" (map #(reduce str %) (selections all-chars 2)))

(def all-char-trios "Map of all possible trios of lower case characters, numbers and space" (map #(reduce str %) (selections all-chars 3)))

(def N-letter "number of letters in text" (count formattedText))


(def letters "Sequence of all letters in text" (make-letter-groups-memo formattedText 1))

(println "Getting letter counts")

; count word frequencies

(def counts-ASCII-1 "Frequencies of each distinct letter in text" (frequencies letters))

(def all-char-counts (update-counts-map-memo all-chars counts-ASCII-1))

(println "getting letter pairs")

;(defn makeLetterPairs "Creates a sequence of all the letter pairs from the input"
 ; [theLetters] (if (> (count theLetters) 2)
  ;   (cons (str (first theLetters) (second theLetters))(lazy-seq(makeLetterPairs (rest theLetters))))
   ;  (cons (str (first theLetters) (second theLetters)) "")))


  


(def letter-pairs "Sequence of all pairs of letters in text" (make-letter-groups-memo formattedText 2))

(println "Getting letter pair counts")

(def counts-ASCII-2 "Frequencies of each distinct letter pair in text" (frequencies letter-pairs))

(def all-char-pair-counts (update-counts-map-memo all-char-pairs counts-ASCII-2))

(def letter-trios "Sequence of all trios of letters in text" (make-letter-groups-memo formattedText 3))

(def counts-ASCII-3 "Frequencies of each distinct letter trio in text" (frequencies letter-trios))

(def all-char-trio-counts (update-counts-map-memo all-char-trios counts-ASCII-3))