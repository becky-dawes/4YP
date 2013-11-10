(ns n-gram.core)

(use 'clojure.java.io)

; read in file

;(def txt (slurp "the-wonderful-wizard-of-oz.txt"))

(println "Reading file")

(def file-name "Name of file from which text is read" (str "the-wonderful-wizard-of-oz.txt"))



(def lines "All lines in file" (with-open [rdr (reader file-name)] 
             (doall (line-seq rdr))))

(println "Formatting text")

;Remove all punctuation (except apostrophes) and convert to lower case

(def formattedText "Text with all punctuation (except apostrophes) removed and converted to lower case" (clojure.string/lower-case (clojure.string/replace lines #"[\p{P}&&[^'][\n]]" " ")))

; split tokens at whitespace (reg. expr.)

(def words "Vector of all words in text" (clojure.string/split formattedText #"\s+"))

(println (str (count words) " words"))

(println "Finding word frequencies")

; count word frequencies

(def word-freq "Frequencies of each distinct word in text" (frequencies words))

(println (str (count word-freq) " distinct words"))

(def word-vals "Map of word frequency values" (map val word-freq))

(def word-keys "Map of word frequency keys" (map key word-freq))

(def word-count "Count of all words in text" (count words))

; normalize word frequencies by total count

(def word-freq-norm "Normalised frequencies of all words" (zipmap word-keys (map #(/ % word-count) word-vals)))

; find frequencies of frequencies for single words and sort

;(def freq-word-freqs "Frequencies of frequencies for single words" (sort-by key (frequencies (map val nh))))

;(def freq-word-freqs-keys (map key freq-word-freqs))

;(def freq-word-freqs-vals (ap val freq-word-freqs))

; convert frequencies of frequencies to a hash map

;(def freq-word-freqs-map (zipmap freq-word-freqs-keys freq-word-freqs-vals))


(defn cumsum

   "With one arg x returns lazy cumulative sum sequence y with (= (nth y) (cumsum x))

    With two args t, x returns lazy cumulative sum sequence y with (= (nth y) (+ t (cumsum x)))"

  ([x] (cumsum 0 x))

  ([t x] (if (empty? x) ()           

           (let [y (+ t (first x))]

             (cons y ( lazy-seq (cumsum y (rest x))))))))

; Cumulative sum of all words

(def ai  (cumsum (repeat (- (count words) 1) 1)))



; slow and inefficient but interesting to understand

;(defn combine [lst1 lst2]

;  (mapcat (fn [x] (map #(list % x) lst1)) lst2))



;(def wp (interleave  (map #(nth words (- % 1)) ai) (map #(nth words %) ai)))



;(defn pairs [x y] 

 ;(if (not= (count x) 0) 

  ; (cons (list (first x) (first y)) (lazy-seq (pairs (rest x) (rest y))))

   ;(list)))
   
   (println "Making pairs")

(defn makePairs "Creates a sequence of all the word pairs from the input"
  ([theWords] (if (> (count theWords) 2)
     (cons(take 2 theWords)(lazy-seq(makePairs (rest theWords)))))))


;(def pairFreq (frequencies (pairs (map #(nth words (- % 1)) ai) (map #(nth words %) ai))))

; Create sequence of all word pairs

(def pairs "Sequence of all pairs of words in text" (makePairs words))

(println "Finding pair frequencies")

; Find frequency of each word pair

(def pair-freq "Map of frequencies of all pairs of words in text" (frequencies pairs))

; Normalise pair frequencies

(def pairs-count "Count of all pairs of words in text"(count pairs))

(def pair-vals "Map of pair frequency values" (map val pair-freq))

(def pair-keys "Map of pair frequency keys" (map key pair-freq))

(def pair-freq-norm "Map of normalised frequencies of all pairs of words in text" (zipmap pair-keys (map #(/ % pairs-count) pair-vals)))

; find frequencies of frequencies for pairs and sort

;(def pair-freqs "Frequencies of frequencies of all word pairs"(sort-by key (frequencies (map val pairFreqNorm))))

; convert frequencies of freqquencies to a hash map

;(def pair-freqs-map (zipmap (map key pair-freqs) (map val pair-freqs)))


(println (str (count pair-freq) " distinct pairs"))

;(defn p1 "Returns unigram probability of input word"
 ; ([word1] (float (nh word1))))
         
;(defn p2 "Returns bigram probability of input words" 
 ; ([word1 word2] (float (/ (pairFreqNorm [word1 word2]) (nh word1)))))

(println "Making trios")

(defn makeTrios "Creates a sequence of the word trios from the input"
  ([theWords] (if (> (count theWords) 3)
     (cons(take 3 theWords)(lazy-seq(makeTrios (rest theWords)))))))

; Create sequence of all word trios

(def trios "Sequence of all trios of words in text" (makeTrios words))

(println "Finding trio frequencies")

; Find frequency of each word trio

(def trio-freq "Frequencies of all trios of words in text" (frequencies trios))

; Normalise trio frequencies

(def trio-vals "Map of trio frequency values" (map val trio-freq))

(def trio-keys "Map of trio frequency keys" (map key trio-freq))

(def trio-count "Count of all trios of words in text" (count trios))

(def trio-freq-norm "Map of normalised frequencies of all trios of words in text" (zipmap trio-keys (map #(/ % trio-count) trio-vals)))

(println (str (count trio-freq) " distinct trios"))


;(defn p3 "Returns trigram probability of input words"
 ; ([word1 word2 word3] (float (/ (trioFreqNorm [word1 word2 word3]) 
  ;         (pairFreqNorm [word1 word2])))))
         


; cumulative sum of all frequencies of frequencies for single words

;(def cumsum-nh-freqs "Cumulative sum of all normalised word frequencies"(cumsum (repeat (count nh-freqs) 1)))

; hash map of frequencies and their counts with frequencies as key for single words

;(def nh-freqs-counts-key-freqs (zipmap (map key nh-freqs) cumsum-nh-freqs ));\

; hash map of frequencies and their with counts as key for single words

;(def nh-freqs-counts-key-counts (zipmap cumsum-nh-freqs (map key nh-freqs)  ))

; cumulative sum of all frequencies of frequencies for pairs

;(def cumsum-pair-freqs (cumsum (repeat (count pair-freqs) 1)))

; hash map of frequencies and their counts with frequencies as key for pairs

;(def pair-freqs-counts-key-freqs (zipmap (map key pair-freqs) cumsum-pair-freqs ))

; hash map of frequencies and their with counts as key for pairs

;(def pair-freqs-counts-key-counts (zipmap cumsum-pair-freqs (map key pair-freqs)  ))

;(defn good-turing-freq "Returns Good-Turing frequency depending on number of input words"
 ; ([word1] (if (< (nh-freqs-counts-key-freqs (nh word1)) (count nh-freqs))
  ;                (* (+ (nh word1) 1) (/ (nh-freqs-counts-key-counts (+ (nh-freqs-counts-key-freqs (nh word1) 1)))) 
   ;      (nh-freqs-map (nh word1) )))
           
    ;       (nh word1))
  
  ;([word1 word2] (if (< (pair-freqs-counts-key-freqs (pairFreqNorm [word1 word2])) (count pair-freqs))
   ;               (* (+ (pairFreqNorm [word1 word2]) 1) (/ (pair-freqs-counts-key-counts (+ (pair-freqs-counts-key-freqs (pairFreqNorm [word1 word2]) 1)))) 
    ;     (pair-freqs-map (pairFreqNorm [word1 word2]) )))
           
     ;      (pairFreqNorm [word1 word2]))
  ;)

;(defn alpha [word1 word2] (/ (good-turing-freq word1 word2) (nh word1)))




 ; (defn find-pair [w] (map #(if (= (first (key %)) w) (val %) 0.0) pairFreqNorm))
  
  ;(defn sum-pairs [word1] (reduce + (find-pair word1)))

;(defn discount [word1] (- 1 
                          
 ;                         ))

(def N "number of words in text"word-count)

(def K 1000000)

(def M "Size of vocabulary" (+ N K))

(def alpha1 "alpha for 1-gram" 1)

(def alpha2 "alpha for 2-gram" 1)

(def alpha3 "alpha for 3-gram" 1)

(defn get-count "Returns 0 if word not found, otherwise returns count"[func args] (if (nil? (func args)) 0 (func args)))

(defn p1 "1-gram probability" [word1] (float (/ (+ (get-count word-freq words) alpha1) (+ N (* M alpha1)))))

(def p1-memo "Memoized p1" (memoize p1))

(defn p2 "2-gram probabilityt" [word1 word2] (float (/ (/ (+ (get-count pair-freq [word1 word2]) alpha2) (+ (- N 1) (* M M alpha2))) (p1-memo word1))))

(def p2-memo "memoized p2" (memoize p2))

(defn p3 "3-gram probability" [word1 word2 word3] (float (/ (/ (+ (get-count trio-freq [word1 word2 word3]) alpha3) (+ (- N 2) (* M M M alpha3))) (p2-memo word1 word2))))

(defn p "Returns n-gram probability depending on number of input words"
  ([word1] (p1 word1))
  ([word1 word2] (p2 word1 word2))
  ([word1 word2 word3] (p3 word1 word2 word3)))


         
         