(ns n-gram.core)

(use 'clojure.java.io)

; read in file

;(def txt (slurp "the-wonderful-wizard-of-oz.txt"))

(println "Reading file")

(def file-name "Name of file from which text is read" (str "the-wonderful-wizard-of-oz.txt")) ;"the-wonderful-wizard-of-oz.txt"



(def lines "All lines in file" (with-open [rdr (reader file-name)] 
             (doall (line-seq rdr))))

(println "Formatting text")

;Remove all punctuation (except apostrophes) and convert to lower case

(def formattedText "Text with all punctuation (except apostrophes) removed and converted to lower case" 
  (clojure.string/lower-case (clojure.string/replace lines #"[\p{P}&&[^'][\n]]" "")))

; split tokens at whitespace (reg. expr.)

(def words-vector "Vector of all words in text" (clojure.string/split formattedText #"\s+"))

(defn makeWords "Creates a sequence of all the words from the input"
  [theWords] (if (> (count theWords) 1)
     (cons(take 1 theWords)(lazy-seq(makeWords (rest theWords))))
     (take 1 theWords)))

(def words (makeWords words-vector))

(println (str (count words) " words"))

(println "Finding word frequencies")

; count word frequencies

(def counts-1 "Frequencies of each distinct word in text" (frequencies words))

;(def n-gram-count-maps (zipmap [1] ["counts-1"]))

(println (str (count counts-1) " distinct words"))

(def word-vals "Map of word frequency values" (map val counts-1))

(def word-keys "Map of word frequency keys" (map key counts-1))

(def total-word-count "Count of all words in text" (count words))

; normalize word frequencies by total count

;(def freqs-1 "Normalised frequencies of all words" (zipmap word-keys (map #(/ % total-word-count) word-vals)))




(defn cumsum

   "With one arg x returns lazy cumulative sum sequence y with (= (nth y) (cumsum x))

    With two args t, x returns lazy cumulative sum sequence y with (= (nth y) (+ t (cumsum x)))"

  ([x] (cumsum 0 x))

  ([t x] (if (empty? x) ()           

           (let [y (+ t (first x))]

             (cons y ( lazy-seq (cumsum y (rest x))))))))

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

(defn makePairs "Creates a sequence of all the word pairs from the input"
  [theWords] (if (> (count theWords) 2)
     (cons(take 2 theWords)(lazy-seq(makePairs (rest theWords))))
     (take 2 theWords)))


;(def pairFreq (frequencies (pairs (map #(nth words (- % 1)) ai) (map #(nth words %) ai))))

; Create sequence of all word pairs

(def pairs "Sequence of all pairs of words in text" (makePairs words-vector))

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

(defn makeTrios "Creates a sequence of the word trios from the input"
  [theWords] (if (> (count theWords) 3)
     (cons(take 3 theWords)(lazy-seq(makeTrios (rest theWords))))
     (take 3 theWords)))

; Create sequence of all word trios

(def trios "Sequence of all trios of words in text" (makeTrios words-vector))

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


;(defn p3 "Returns trigram probability of input words"
 ; ([word1 word2 word3] (float (/ (trioFreqNorm [word1 word2 word3]) 
  ;         (pairFreqNorm [word1 word2])))))
         



(def N "number of words in text" total-word-count)

;(def K 1000000)

(def M "Size of vocabulary" (count counts-1))

(def alpha1 "alpha for 1-gram" 1)

(def alpha2 "alpha for 2-gram" 1)

(def alpha3 "alpha for 3-gram" 1)

(defn get-count "Returns 0 if word not found, otherwise returns count"[func args] (if (nil? (func args)) 0 (func args)))

(defn p1 "1-gram probability" [word1] (float (/ (+ (get-count counts-1 words) alpha1) (+ N (* M alpha1)))))

(def p1-memo "Memoized p1" (memoize p1))

(defn p2 "2-gram probability" [word1 word2] (float (/ (/ (+ (get-count counts-2 [word1 word2]) alpha2) (+ (- N 1) (* M M alpha2))) (p1-memo word1))))

(def p2-memo "memoized p2" (memoize p2))

(defn p3 "3-gram probability" [word1 word2 word3]
  (float (/ (/ (+ (get-count counts-3 [word1 word2 word3]) alpha3) (+ (- N 2) (* M M M alpha3))) (p2-memo word1 word2))))

(def p3-memo "memoized p3" (memoize p3))

(defn p "Returns n-gram probability depending on number of input words"
  ([word1] (p1-memo word1))
  ([word1 word2] (p2-memo word1 word2))
  ([word1 word2 word3] (p3-memo word1 word2 word3)))

(defn find-pair "Finds all word pairs starting with given word" [w] (zipmap (map key counts-2) (map #(if (= (first (key %)) w) (val %) 0.0) counts-2)))

(defn find-trio "Finds all word word trios starting with given word pair" [w1 w2] (zipmap (map key counts-3) (map #(if (= (first (key %)) w1) (if (= (second (key %)) w2) (val %) 0.0) 0.0) counts-3)))

(defn next-word "Predicts next word in sequence" 
  ([word1] (key (apply max-key val (find-pair word1))))
  ([word1 word2] (key (apply max-key val (find-trio word1 word2)))))

;(def counts-of-counts-map "Map of counts-of-counts map names for different n" (zipmap [0] [""]))

;(defn generate-counts-of-counts "Generates a map of counts of counts" [input-counts n map-name] ((def counts-of-counts-map (assoc counts-of-counts-map n map-name)) frequencies (map val input-counts)))

(defn generate-counts-of-counts "Generates a map of counts of counts" [input-counts] (frequencies (map val input-counts)))

;(def counts-of-word-counts "1-gram counts-of-counts map" (generate-counts-of-counts counts-1 1 "counts-of-word-counts"))

(defn generate-cumulative-sum "Generates sequence of cumulative sum of items in map" [input-map] (cumsum (repeat (count input-map) 1)))

(defn generate-n-r-map "Generates sorted counts-of-counts map" [input-counts] (map key (sort-by key input-counts)))


;1-gram maps

(def counts-of-counts-1 "1-gram counts-of-counts map" (generate-counts-of-counts counts-1))

(def cumsum-counts-of-counts-1 "Cumulative sum of all normalised word frequencies" (generate-cumulative-sum counts-of-counts-1))

(def n-r-1 "1-gram counts-of-counts map" (generate-n-r-map counts-of-counts-1))

(def n-r-counts-keys-1 "1-gram map of counts-of-counts and indices with counts as keys" (zipmap n-r-1 cumsum-counts-of-counts-1))

(def n-r-index-keys-1 "1-gram map of counts-of-counts and indices with indices as keys" (zipmap cumsum-counts-of-counts-1 n-r-1))

;2-gram maps

(def counts-of-counts-2 "2-gram counts-of-counts map" (generate-counts-of-counts counts-2))

(def cumsum-counts-of-counts-2 "Cumulative sum of all normalised pair frequencies" (generate-cumulative-sum counts-of-counts-2))

(def n-r-2 "2-gram counts-of-counts map" (generate-n-r-map counts-of-counts-2))

(def n-r-counts-keys-2 "2-gram map of counts-of-counts and indices with counts as keys" (zipmap n-r-2 cumsum-counts-of-counts-2))

(def n-r-index-keys-2 "2-gram map of counts-of-counts and indices with indices as keys" (zipmap cumsum-counts-of-counts-2 n-r-2))

;3-gram maps

(def counts-of-counts-3 "3-gram counts-of-counts map" (generate-counts-of-counts counts-3))

(def cumsum-counts-of-counts-3 "Cumulative sum of all normalised pair frequencies" (generate-cumulative-sum counts-of-counts-3))

(def n-r-3 "3-gram counts-of-counts map" (generate-n-r-map counts-of-counts-3))

(def n-r-counts-keys-3 "3-gram map of counts-of-counts and indices with counts as keys" (zipmap n-r-3 cumsum-counts-of-counts-3))

(def n-r-index-keys-3 "3-gram map of counts-of-counts and indices with indices as keys" (zipmap cumsum-counts-of-counts-3 n-r-3))


;Good-Turing methods

(defn r "Returns count of given n-gram" [n-gram n] (get-count (resolve (symbol (str "counts-" n))) n-gram))

(def r-memo "Memoized r" (memoize r))

(defn n-r "Returns count of given count" [freq n] (if (> freq 0) ((resolve (symbol (str "counts-of-counts-" n))) freq) 
                                                    (count (var-get (resolve (symbol (str "counts-" n)))))))

(def n-r-memo "Memoized n-r" (memoize n-r))

(defn n-r-plus-one "Returns count of next biggest count" [freq n] 
  (let [n-r-index (get-count (resolve (symbol (str "n-r-counts-keys-" n))) freq)] 
    (if (= n-r-index (count (var-get (resolve (symbol (str "counts-of-counts-" n)))))) 
      (n-r-memo freq n) ((resolve (symbol (str "counts-of-counts-" n))) ((resolve (symbol (str "n-r-index-keys-" n))) (+ n-r-index 1))))))

(def n-r-plus-one-memo "Memoized n-r-plus-one" (memoize n-r-plus-one))

(defn g-t "Returns Good-Turing count of given word" [n-gram n] 
  (let [freq (if (= (type n-gram) (type "")) (r-memo [n-gram] n) (r-memo n-gram n))] (* (+ freq 1) (/ (n-r-plus-one-memo freq n) (n-r-memo freq n)))))

(def g-t-memo "Memoized g-t" (memoize g-t))

(defn g-t-prob "Returns probability of a given n-gram using Good-Turing smoothing" [n-gram] 
  (let [n (if (= (type n-gram) (type "")) (count [n-gram]) (count n-gram))] 
    (if (> n 1) (float (/ (g-t-memo n-gram n) (g-t-memo (butlast n-gram) (- n 1)))) 
      (float (/ (g-t-memo n-gram n) (count (var-get (resolve (symbol (str "counts-" n)))))))))) ;<- don't think this is right
  
(def g-t-prob-memo "Memoized g-t-prob" (memoize g-t-prob))

;(defn g-t-prob "Returns Good-Turing probability of given word" [word] (float (/ (g-t-memo word) N)))

;(def g-t-prob-memo "Memoized g-t-prob" (memoize g-t-prob))

;Convert to lower case



;(def formattedTextASCII "Text  converted to lower case" 
 ; (clojure.string/lower-case lines))



(println "Getting letters")

(defn make-letter-groups "Creates a sequence of all letter groups of size n from input"
  [theLetters n] (if (> (count theLetters) n)
                   (cons (clojure.string/join (take n theLetters)) (lazy-seq (make-letter-groups (rest theLetters) n)))
                   (cons (clojure.string/join (take n theLetters)) "")))

;(def letters-vector "Vector of all words in text" (split-at 1 formattedTextASCII))

;(defn makeLetters "Creates a sequence of all the letters from the input"
 ; [theLetters] (if (> (count theLetters) 1)
  ;   (cons(str (first theLetters))(lazy-seq(makeLetters (rest theLetters))))
   ;  (cons (str (first theLetters)) "")))

(def letters (make-letter-groups formattedText 1))

(println "Getting letter counts")

; count word frequencies

(def counts-ASCII-1 "Frequencies of each distinct letter in text" (frequencies letters))

(println "getting letter pairs")

;(defn makeLetterPairs "Creates a sequence of all the letter pairs from the input"
 ; [theLetters] (if (> (count theLetters) 2)
  ;   (cons (str (first theLetters) (second theLetters))(lazy-seq(makeLetterPairs (rest theLetters))))
   ;  (cons (str (first theLetters) (second theLetters)) "")))


  


(def letter-pairs "Sequence of all pairs of letters in text" (make-letter-groups formattedText 2))

(println "Getting letter pair counts")

(def counts-ASCII-2 "Frequencies of each distinct letter pair in text" (frequencies letter-pairs))

(defn p1-letter [theLetter] (float (/ (counts-ASCII-1 theLetter) (count letters))))

(def p1-letter-memo (memoize p1-letter))

(defn p2-letter [theLetters] (float (/ (counts-ASCII-2 theLetters) (counts-ASCII-1 (str (first theLetters))))))



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

         
         