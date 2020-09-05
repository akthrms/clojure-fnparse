(ns core
  (:import [fnparse.core ParseSuccess])
  (:require [fnparse.core :refer :all]))

(def number (fp-map (fp-regex "\\d*") #(Integer/parseInt % 10)))

(def operator (fp-char "+-"))

(declare expression)

(def parenthesis (fp-lazy (fn []
                           (let [parser (fp-seq (fp-token "(") expression (fp-token ")"))]
                             (fp-map parser #(second %))))))

(def p-atom (fp-choice number parenthesis))

(def expression (fp-map (fp-seq p-atom (fp-many (fp-seq operator p-atom)))
                        #(concat (vector (first %)) (reduce concat [] (second %)))))

(defn -main [target & _]
  (let [result (expression target 0)]
    (if (= (type result) ParseSuccess)
      (println (:result result))
      (println (str (+ (:new-position result) 1) "文字目でパース失敗")))))
