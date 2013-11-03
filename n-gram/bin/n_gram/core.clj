(ns n-gram.core)


; read in file

(def txt (slurp "the-wonderful-wizard-of-oz.txt"))

;Remove all punctuation (except apostrophes) and convert to lower case

(def formattedText (clojure.string/lower-case (clojure.string/replace txt #"[\p{P}&&[^'][\n]]" " ")))

; split tokens at whitespace (reg. expr.)

(def words (clojure.string/split formattedText #"\s+"))



; count word frequencies

(def h (frequencies words))



; normalize word frequencies by total count

(def nh (zipmap (map key h) (map #(/ % (count words)) (map val h))))


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

(defn makePairs "Creates a sequence of all the word pairs from the input"
  ([theWords] (if (> (count theWords) 2)
     (cons(take 2 theWords)(lazy-seq(makePairs (rest theWords)))))))


;(def pairFreq (frequencies (pairs (map #(nth words (- % 1)) ai) (map #(nth words %) ai))))

; Create sequence of all word pairs

(def pairs (makePairs words))

; Find frequency of each word pair

(def pairFreq (frequencies pairs))

; Normalise pair frequencies

(def pairFreqNorm (zipmap (map key pairFreq) (map #(/ % (count pairs)) (map val pairFreq))))

(defn p1 "Returns unigram probability of input word"
  ([word1] (float (nh word1))))
         
(defn p2 "Returns bigram probability of input words" 
  ([word1 word2] (float (/ (pairFreqNorm [word1 word2]) (nh word1)))))


(defn makeTrios "Creates a sequence of the word trios from the input"
  ([theWords] (if (> (count theWords) 3)
     (cons(take 3 theWords)(lazy-seq(makeTrios (rest theWords)))))))

; Create sequence of all word trios

(def trios (makeTrios words))

; Find frequency of each word trio

(def trioFreq (frequencies trios))

; Normalise trio frequencies

(def trioFreqNorm (zipmap (map key trioFreq) (map #(/ % (count trios)) (map val trioFreq))))



(defn p3 "Returns trigram probability of input words"
  ([word1 word2 word3] (float (/ (trioFreqNorm [word1 word2 word3]) 
           (pairFreqNorm [word1 word2])))))
         
(defn p "Returns n-gram probability depending on number of input words"
  ([word1] (p1 word1))
  ([word1 word2] (p2 word1 word2))
  ([word1 word2 word3] (p3 word1 word2 word3)))

         
         