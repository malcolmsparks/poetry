(ns poetry.core
  (:require [clojure.java.io :as io]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; Syllables Rules:
;; A syllable is the sound of a vowel (a, e, i, o, u) that's created when pronouncing a word.
;; The number of times that you hear the sound of a vowel (a, e, i, o, u) in a word is equal to the number of syllables the word has.
;;
;; How To Find Syllables:
;;
;; Count the number of vowels (a, e, i, o, u, and sometimes y) in the word.
;; Subtract any silent vowels (like the silent 'e' at the end of a word).
;; Subtract 1 vowel from every diphthong.
;; A diphthong is when two volwels make only 1 sound (oi, oy, ou, ow, au, aw, oo, ...).
;; The number you are left with should be the number of vowels in the word.
;; How To Divide A Word Into Syllables:
;; Divide off any compound words, prefixes, suffixes, and root words that have vowels.
;; sports/car, house/boat, un/happy, pre/paid, re/write, farm/er, hope/less
;; Divide between two middle consonants
;; hap/pens, bas/ket, let/ter, sup/per, din/ner
;; Never split up consonant digraphs as they really represent only one sound ("th", "sh", "ph", "th", "ch", and "wh").
;; Usually divide before a single consonant.
;; o/pen, i/tem, e/vil, re/port.
;; The only exceptions are those times when the first syllable has an obvious short sound, as in "cab/in".
;; Divide before an "-le" syllable.
;; a/ble, fum/ble, rub/ble, mum/ble
;; The only exceptions are "ckle" words like "tick/le".

(def vowels #{\a \e \u \i \o \y})
(defn count-vowels [s]
  (count (filter vowels (seq (.toLowerCase s))))
  )

(defn remove-silent-e [word]
  (if (= (last word) \e)
    (apply str (butlast word))
    word))

(def split-into-words (partial re-seq #"\S+"))

(defn count-syllables [s]
  (->> s
       split-into-words
       (map (comp count-vowels remove-silent-e))
       (apply +)
       ))

(defn generate-poem [structure]
  (println "--------------------")
  (let [buckets (group-by count-syllables (line-seq (io/reader "src/poetry/proverbs.txt")))]
    (dorun (map (comp println rand-nth buckets) structure))
    )
  (println "-------END---------"))

(defn generate-haiku [] (generate-poem [5 7 5]))
(defn generate-limerick [] (generate-poem [8 8 5 5 8]))

(defn generate-shakespearean-sonnets []
  (filter #(= % works-of-shakespeare)
          (partition 400000
                     (map rand-nth
                          (repeat
                           (conj
                            (map char
                                 (range (int \A) (inc (int \Z)))) \space))))))




(generate-limerick)

