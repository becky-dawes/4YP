(ns n-gram.core (:require [n-gram.words.file-reader :refer :all]
                          [n-gram.letters.file-reader :refer :all]
                          ;[n-gram.words.word-maker :refer :all] 
                          [n-gram.letters.letter-maker :refer :all]
                          ;[n-gram.words.word-predictor :refer :all]
                          [n-gram.letters.letter-predictor :refer :all]
                          ;[n-gram.words.word-probs :refer :all]
                          [n-gram.letters.letter-probs :refer :all]
                          ;[n-gram.words.good-turing :refer :all]
                          [n-gram.misc.misc-functions :refer :all]
                          ;[n-gram.words.interpolation :refer :all]
                          ;[n-gram.letters.hierarchical-dirichlet :refer :all]
                          ;[n-gram.misc.suffix :refer :all]
                          [clojure.inspector :refer :all]))



;(def txt (slurp "the-wonderful-wizard-of-oz.txt"))

(println "Reading file")


;(defn p3 "Returns trigram probability of input words"
 ; ([word1 word2 word3] (float (/ (trioFreqNorm [word1 word2 word3]) 
  ;         (pairFreqNorm [word1 word2])))))
         









;(def counts-of-counts-map "Map of counts-of-counts map names for different n" (zipmap [0] [""]))

;(defn generate-counts-of-counts "Generates a map of counts of counts" [input-counts n map-name] ((def counts-of-counts-map (assoc counts-of-counts-map n map-name)) frequencies (map val input-counts)))



;(defn g-t-prob "Returns Good-Turing probability of given word" [word] (float (/ (g-t-memo word) N)))

;(def g-t-prob-memo "Memoized g-t-prob" (memoize g-t-prob))

;Convert to lower case



;(def formattedTextASCII "Text  converted to lower case" 
 ; (clojure.string/lower-case lines))



;(println "Getting letters")



;(def letters-vector "Vector of all words in text" (split-at 1 formattedTextASCII))

;(defn makeLetters "Creates a sequence of all the letters from the input"
 ; [theLetters] (if (> (count theLetters) 1)
  ;   (cons(str (first theLetters))(lazy-seq(makeLetters (rest theLetters))))
   ;  (cons (str (first theLetters)) "")))










; find frequencies of frequencies for single words and sort

;(def word-count-freqs "Frequencies of frequencies for single words" (sort-by key (frequencies (map val counts-1))))

;(def word-count-freqs "Frequencies of frequencies for single words" (frequencies (map val counts-1)))

;(defn find-word-count "Find frequency of word in training corpus" [word] (get-count counts-1 word))

;(def find-word-count-memo "Memoized find-word-count" (memoize find-word-count))

;(defn find-count-freq "Find number of words with given frequency" [freq] (if (> freq 0) (word-count-freqs freq) (count counts-1)))

;(def find-count-freq-memo "Memoized find-count-freq" (memoize find-count-freq))

;(def cumsum-word-count-freqs "Cumulative sum of all normalised word frequencies"(cumsum (repeat (count word-count-freqs) 1)))

; hash map of frequencies and their counts with frequencies as key for single words

;(def word-count-freqs-map-key-freqs "Map of frequencies of frequencies and indexes with frequencies as keys" (zipmap (map key(sort-by key word-count-freqs)) cumsum-word-count-freqs ))

; hash map of frequencies and their with counts as key for single words

;(def word-count-freqs-map-key-counts "Map of frequencies of frequencies and indexes with indexes as keys" (zipmap cumsum-word-count-freqs (map key (sort-by key word-count-freqs)  )))


;;(defn find-freq-index "Find index of frequency in sorted array" [freq] (if (nil? (word-count-freqs-map-key-freqs freq)) 0 (word-count-freqs-map-key-freqs freq)))

;(def find-freq-index-memo "Memoized find-freq-index" (memoize find-freq-index))

;(defn find-count-from-index "Find number of words with frequency given index" [index] (if (> index (count word-count-freqs-map-key-counts)) (find-count-from-index (- index 1)) (word-count-freqs-map-key-counts index)))

;(def find-count-from-index-memo "Memoized find-count-from-index" (memoize find-count-from-index))

;(defn find-count-next-freq "Find number of words with next biggest frequency" [freq] (find-count-freq-memo (find-count-from-index-memo (+ (find-freq-index-memo freq) 1))))

;(def find-count-next-freq-memo "Memoized find-count-next-freq" (memoize find-count-next-freq))

; add one for highest freq?

;(defn good-turing-freq "Returns Good-Turing frequency of word" [word] (float (/ (* (+ (find-word-count-memo word) 1) (/ (find-count-next-freq-memo (find-word-count-memo word)) (find-count-freq-memo (find-word-count-memo word)))) N)))



;(def word-count-freqs-keys (map key word-count-freqs))

;(def word-count-freqs-vals (map val word-count-freqs))

; convert frequencies of frequencies to a hash map

;(def word-count-freqs-map (zipmap word-count-freqs-keys word-count-freqs-vals))

; find frequencies of frequencies for pairs and sort

;(def pair-count-freqs "Frequencies of frequencies of all word pairs"(sort-by key (frequencies (map val freqs-2))))

; convert frequencies of freqquencies to a hash map

;(def pair-count-freqs-map (zipmap (map key pair-count-freqs) (map val pair-count-freqs)))

; cumulative sum of all frequencies of frequencies for single words


; cumulative sum of all frequencies of frequencies for pairs

;(def cumsum-pair-freqs (cumsum (repeat (count pair-count-freqs) 1)))

; hash map of frequencies and their counts with frequencies as key for pairs

;(def pair-freqs-counts-key-freqs (zipmap (map key pair-count-freqs) cumsum-pair-freqs ))

; hash map of frequencies and their with counts as key for pairs

;(def pair-freqs-counts-key-counts (zipmap cumsum-pair-freqs (map key pair-count-freqs)  ))

;(defn good-turing-freq "Returns Good-Turing frequency depending on number of input words"
; ([word1] (if (< (word-count-freqs-map-key-freqs (counts-1 word1)) (count word-count-freqs))
 ;                (* (+ (counts-1 word1) 1) (/ (word-count-freqs-map-key-counts (+ (word-count-freqs-map-key-freqs (counts-1 word1) 1)))) 
  ;      (word-count-freqs-map (counts-1 word1) )))
           
   ;       (counts-1 word1))
  
 ;([word1 word2] (if (< (pair-freqs-counts-key-freqs (pair-freqs [word1 word2])) (count pair-count-freqs))
  ;              (* (+ (pair-freqs [word1 word2]) 1) (/ (pair-freqs-counts-key-counts (+ (pair-freqs-counts-key-freqs (pair-freqs [word1 word2]) 1)))) 
   ;    (pair-count-freqs-map (pair-freqs [word1 word2]) )))
           
  ;       (pair-freqs [word1 word2]))
 ;)

;(defn alpha [word1 word2] (/ (good-turing-freq word1 word2) (nh word1)))




 ; (defn find-pair [w] (map #(if (= (first (key %)) w) (val %) 0.0) pairFreqNorm))
  
  ;(defn sum-pairs [word1] (reduce + (find-pair word1)))

;(defn discount [word1] (- 1 
                          
 ;                         ))

         
         