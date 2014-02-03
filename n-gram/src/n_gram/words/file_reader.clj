(ns n-gram.words.file-reader (:require [n-gram.misc.misc-functions :refer :all]
                                       [n-gram.words.word-maker :refer :all]))

(use 'clojure.java.io)

(def file-name "Name of file from which text is read" (str "the-wonderful-wizard-of-oz-chapter-1.txt")) 

(def lines "All lines in file" (with-open [rdr (reader file-name)] 
             (doall (line-seq rdr))))



(println "Formatting text")

;Remove all punctuation (except apostrophes) and convert to lower case

(defn format-text [text] "Text with all punctuation (except apostrophes) 
removed and converted to lower case" 
  (clojure.string/lower-case (clojure.string/replace text #"[\p{P}&&[^'][\n]]" "")))


(def format-text-memo "Memoized format-text" (memoize format-text))


(def formattedText "Formatted text" (format-text-memo lines))

; split tokens at whitespace (reg. expr.)



(def raw-words-vector "Vector of all words in text" (split-words-memo formattedText))

(def unique-words "Sequence of all unqiue words in text" (distinct raw-words-vector))





(def replaced-lines "Text with first occurrences of words replaced with <unk>" 
  (replace-first-word-memo lines unique-words))

(def formatted-new-text "New formatted text" (format-text-memo replaced-lines))

(def words-vector "Vector of new words" (split-words-memo formatted-new-text))



;(def words (make-words-memo words-vector))

(def words (make-words-memo words-vector))


(def N "Count of all words in text" (count words))

(println (str N " words"))

(println "Finding word frequencies")

; count word frequencies

(def counts-1 "Frequencies of each distinct word in text" (frequencies words))

;(def n-gram-count-maps (zipmap [1] ["counts-1"]))

(println (str (count counts-1) " distinct words"))

(def word-vals "Map of word frequency values" (map val counts-1))

(def word-keys "Map of word frequency keys" (map key counts-1))


; normalize word frequencies by total count

;(def freqs-1 "Normalised frequencies of all words" (zipmap word-keys (map #(/ % total-word-count) word-vals)))





; Cumulative sum of all words

;(def ai  (cumsum (repeat (- (count words) 1) 1)))



; slow and inefficient but interesting to understand

;(defn combine [lst1 lst2]

;  (mapcat (fn [x] (map #(list % x) lst1)) lst2))



;(def wp (interleave  (map #(nth words (- % 1)) ai) (map #(nth words %) ai)))



;(defn pairs [x y] 

 ;(if (not= (count x) 0) 

  ; (cons (list (first x) (first y)) (lazy-seq (pairs (rest x) (rest y))))

   ;(list)))
   
   (println "Making pairs")



;(def pairFreq (frequencies (pairs (map #(nth words (- % 1)) ai) (map #(nth words %) ai))))

; Create sequence of all word pairs

;(def pairs "Sequence of all pairs of words in text" (make-pairs-memo words-vector))

(def pairs "Sequence of all pairs of words in text" (make-pairs-memo words-vector))

(println "Finding pair frequencies")

; Find frequency of each word pair

(def counts-2 "Map of frequencies of all pairs of words in text" (frequencies pairs))

;(def n-gram-count-maps (assoc n-gram-count-maps 2 "counts-2"))

; Normalise pair frequencies

(def total-pair-count "Count of all pairs of words in text"(count pairs))

(def pair-vals "Map of pair frequency values" (map val counts-2))

(def pair-keys "Map of pair frequency keys" (map key counts-2))

;(def freqs-2 "Map of normalised frequencies of all pairs of words in text" (zipmap pair-keys (map #(/ % total-pair-count) pair-vals)))




(println (str (count counts-2) " distinct pairs"))

;(defn p1 "Returns unigram probability of input word"
 ; ([word1] (float (nh word1))))
         
;(defn p2 "Returns bigram probability of input words" 
 ; ([word1 word2] (float (/ (pairFreqNorm [word1 word2]) (nh word1)))))

(println "Making trios")



; Create sequence of all word trios

(def trios "Sequence of all trios of words in text" (make-trios-memo words-vector))

(println "Finding trio frequencies")

; Find frequency of each word trio

(def counts-3 "Frequencies of all trios of words in text" (frequencies trios))

;(def n-gram-count-maps (assoc n-gram-count-maps 3 "counts-3"))

; Normalise trio frequencies

(def trio-vals "Map of trio frequency values" (map val counts-3))

(def trio-keys "Map of trio frequency keys" (map key counts-3))

(def total-trio-count "Count of all trios of words in text" (count trios))

;(def freqs-3 "Map of normalised frequencies of all trios of words in text" (zipmap trio-keys (map #(/ % total-trio-count) trio-vals)))

(println (str (count counts-3) " distinct trios"))

