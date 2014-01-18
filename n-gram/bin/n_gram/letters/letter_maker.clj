(ns n-gram.letters.letter-maker (:require [n-gram.misc.misc-functions :refer :all]))

(defn make-letter-groups "Creates a sequence of all letter groups of size n from input"
  [theLetters n] (if (> (count theLetters) n)
                   (cons (clojure.string/join (take n theLetters))
                         (lazy-seq (make-letter-groups (rest theLetters) n)))
                   (cons (clojure.string/join (take n theLetters)) "")))

(def make-letter-groups-memo "Memoized make-letter-groups" (memoize make-letter-groups))

