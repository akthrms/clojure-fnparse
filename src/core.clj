(ns core
  (:import [fnparse.core ParseSuccess])
  (:require [fnparse.core :refer :all]))

(def fp-number (fp-map (fp-regex "\\d*") #(Integer/parseInt % 10)))

(def fp-operator (fp-char "+-"))

(declare fp-expression)

(def fp-parenthesis (fp-lazy (fn []
                               (let [parser (fp-seq (fp-token "(") fp-expression (fp-token ")"))]
                                 (fp-map parser #(second %))))))

(def fp-atom (fp-choice fp-number fp-parenthesis))

(def fp-expression (fp-map (fp-seq fp-atom (fp-many (fp-seq fp-operator fp-atom)))
                           #(concat (vector (first %)) (reduce concat [] (second %)))))

(defn -main [target & _]
  (let [result (fp-expression target 0)]
    (if (= (type result) ParseSuccess)
      (println (:result result))
      (println (str (+ (:new-position result) 1) "文字目でパース失敗")))))
