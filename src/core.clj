(ns core
  (:import [fnparse.core ParseSuccess])
  (:require [fnparse.core :refer :all]))

(def number (p-map (p-regex "\\d*") #(Integer/parseInt % 10)))

(def operator (p-char "+-"))

(declare expression)

(def parenthesis (p-lazy (fn []
                           (let [parser (p-seq (p-token "(") expression (p-token ")"))]
                             (p-map parser #(second %))))))

(def p-atom (p-choice number parenthesis))

(def expression (p-map (p-seq p-atom (p-many (p-seq operator p-atom)))
                       #(concat (vector (first %)) (reduce concat [] (second %)))))

(defn -main [target & _]
  (let [result (expression target 0)]
    (if (= (type result) ParseSuccess)
      (println (:result result))
      (println (str (+ (:new-position result) 1) "文字目でパース失敗")))))
