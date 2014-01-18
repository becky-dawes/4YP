(ns n-gram.words.interpolation (:require [n-gram.misc.misc-functions :refer :all]
                                        [n-gram.words.file-reader :refer :all]
                                        [n-gram.words.word-maker :refer :all]
                                        [n-gram.words.word-probs :refer :all]))

;(defn loop-block-creator [the-words word-start words-per-block block-name n]
;  (let [var-name (str block-name n)] (if (> n 0) (do (generate-variable (clojure.string/join `(~@var-name)) (subvec the-words word-start (+ word-start words-per-block))) 
;                                                   (loop-block-creator the-words (+ word-start words-per-block) words-per-block block-name (- n 1))))))

;(defn split-text-into-blocks "Splits training corpus into n blocks" [the-words n] 
;  (let [word-count (count the-words) words-per-block (/ word-count n)] (loop-block-creator the-words 0 words-per-block "interpolation-block-" n)))
    
(def number-of-blocks 2)

(def words-per-block (/ N number-of-blocks))

(def interpolation-block-1 (subvec raw-words-vector 0 words-per-block))

(def interpolation-block-2 (subvec raw-words-vector words-per-block (* 2 words-per-block)))

;(def interpolation-block-3 (subvec raw-words-vector (+ (* 2 words-per-block) 1) (* 3 words-per-block)))
;
;(def interpolation-block-4 (subvec raw-words-vector (+ (* 3 words-per-block) 1) (* 4 words-per-block)))
;
;(def interpolation-block-5 (subvec raw-words-vector (+ (* 4 words-per-block) 1) (* 5 words-per-block)))
;
;(def interpolation-block-6 (subvec raw-words-vector (+ (* 5 words-per-block) 1) (* 6 words-per-block)))
;
;(def interpolation-block-7 (subvec raw-words-vector (+ (* 6 words-per-block) 1) (* 7 words-per-block)))
;
;(def interpolation-block-8 (subvec raw-words-vector (+ (* 7 words-per-block) 1) (* 8 words-per-block)))
;
;(def interpolation-block-9 (subvec raw-words-vector (+ (* 8 words-per-block) 1) (* 9 words-per-block)))
;
;(def interpolation-block-10 (subvec raw-words-vector (+ (* 9 words-per-block) 1) N))

(defn p-ML [word-i words-given] (let [n (count words-given)]
                                   (cond (= 1 n) (p-memo word-i (first words-given)) 
                       (= 2 n) (p-memo word-i (first words-given) (second words-given))
                       
                       :else (println "Please supply a second argument with 2 or fewer words"))))

(defn lambda-interp [word-i words-given] 0.7)

(defn p-interp ([word-i words-given] (let [n (count words-given) lam (lambda-interp word-i words-given)]
                                       (cond (= 1 n) (+ (* lam (p-ML word-i words-given)) (* (- 1 lam) (p-interp word-i)))
                                             (= 2 n) (+ (* lam (p-ML word-i words-given)) (* (- 1 lam) (p-interp word-i (subvec words-given 0 1))))
                                             :else (println "Please supply a second argument with 2 or fewer words"))))
  ([word-i] (/ 1 M)))