(ns n-gram.letters.file-reader (:require [n-gram.misc.misc-functions :refer :all]
                                         [n-gram.letters.letter-maker :refer :all]
                                         [n-gram.words.file-reader :refer [formattedText]]))

(def N-letter "number of letters in text" (count formattedText))

(def letters "Sequence of all letters in text" (make-letter-groups-memo formattedText 1))

(println "Getting letter counts")

; count word frequencies

(def counts-ASCII-1 "Frequencies of each distinct letter in text" (frequencies letters))

(println "getting letter pairs")

;(defn makeLetterPairs "Creates a sequence of all the letter pairs from the input"
 ; [theLetters] (if (> (count theLetters) 2)
  ;   (cons (str (first theLetters) (second theLetters))(lazy-seq(makeLetterPairs (rest theLetters))))
   ;  (cons (str (first theLetters) (second theLetters)) "")))


  


(def letter-pairs "Sequence of all pairs of letters in text" (make-letter-groups-memo formattedText 2))

(println "Getting letter pair counts")

(def counts-ASCII-2 "Frequencies of each distinct letter pair in text" (frequencies letter-pairs))

(def letter-trios "Sequence of all trios of letters in text" (make-letter-groups-memo formattedText 3))

(def counts-ASCII-3 "Frequencies of each distinct letter trio in text" (frequencies letter-trios))