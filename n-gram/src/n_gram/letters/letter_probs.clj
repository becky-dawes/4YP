(ns n-gram.letters.letter-probs (:require [n-gram.misc.misc-functions :refer :all]
                                          [n-gram.letters.file-reader :refer :all]))



(def M-letter "Size of letter vocabulary" 256)

(def alpha1-letter "alpha for 1-gram letters" 1)

(def alpha2-letter "alpha for 2-gram letters" 1)

(def alpha3-letter "alpha for 3-gram letters" 1)


(defn p1-letter "1-gram probability for letters" [theLetter] 
  (let [theLetter (clojure.string/lower-case theLetter)] 
                (float (/ (+ (get-count counts-ASCII-1 theLetter) alpha1-letter) 
                          (+ N-letter (* M-letter alpha1-letter))))))

(def p1-letter-memo "Memoized p1-letter" (memoize p1-letter))

(defn p2-letter "2-gram probability for letters" [theLetters] 
  (let [theLetters (clojure.string/lower-case theLetters)] 
                  (float (/ (/ (+ (get-count counts-ASCII-2 theLetters) alpha2-letter)
                            (+ (- N-letter 1) (* M-letter M-letter alpha2-letter))) 
                            (p1-letter-memo (str (first theLetters)))))))

(def p2-letter-memo "Memoized p2-letter" (memoize p2-letter))

(defn p3-letter "3-gram probability for letters" [theLetters] 
  (let [theLetters (clojure.string/lower-case theLetters)] 
                  (float (/ (/ (+ (get-count counts-ASCII-3 theLetters) alpha3-letter)
                            (+ (- N-letter 2) (* M-letter M-letter M-letter alpha3-letter))) 
                            (p2-letter-memo (str (first theLetters) (second theLetters)))))))

(defn p-letter "Returns n-gram probability depending on number of input letters"
  [theLetters] (let [n (count theLetters)] 
                 (cond (= 1 n) (p1-letter-memo theLetters) 
                       (= 2 n) (p2-letter-memo theLetters)
                       (= 3 n) (p2-letter-memo theLetters)
                       :else (println "Please enter a letter string of length 3 or less"))))

(def p-letter-memo "Memoized p-letter" (memoize p-letter))