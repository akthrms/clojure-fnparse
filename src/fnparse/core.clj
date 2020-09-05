(ns fnparse.core)

(defrecord ParseSuccess [result new-position])

(defrecord ParseFailure [result new-position])

(defn- substring [string start end]
  (let [len (count string)]
    (.substring string start (if (< len end) len end))))

(defn fp-token [string]
  (let [len (count string)]
    (fn [target position]
      (let [end (+ position len)]
        (if (= (substring target position end) string)
          (->ParseSuccess string end)
          (->ParseFailure nil position))))))

(defn fp-many [parser]
  (fn [target position]
    (loop [result []
           position' position]
      (let [result' (parser target position')]
        (if (= (type result') ParseSuccess)
          (recur (conj result (:result result')) (:new-position result'))
          (->ParseSuccess result position'))))))

(defn fp-choice [& parsers]
  (fn [target position]
    (let [successes (for [parser parsers
                          :let [result (parser target position)]
                          :when (= (type result) ParseSuccess)]
                      result)]
      (if (< 0 (count successes))
        (first successes)
        (->ParseFailure nil position)))))

(defn fp-seq [& parsers]
  (fn [target position]
    (let [position' (atom position)
          successes (for [parser parsers
                          :let [result (parser target @position')]
                          :while (= (type result) ParseSuccess)]
                      (do
                        (reset! position' (:new-position result))
                        result))]
      (if (= (count parsers) (count successes))
        (->ParseSuccess (map :result successes) (last (map :new-position successes)))
        (->ParseFailure nil position)))))

(defn fp-option [parser]
  (fn [target position]
    (let [result (parser target position)]
      (if (= (type result) ParseSuccess)
        result
        (->ParseSuccess nil position)))))

(defn fp-regex [regexp]
  (fn [target position]
    (let [regexp' (if (= (.substring regexp 0 1) "^") regexp (str "^" regexp))
          target' (.substring target position)
          matches (re-find (re-pattern regexp') target')
          result (if (coll? matches) (first matches) matches)]
      (if (not-empty result)
        (->ParseSuccess result (+ position (count result)))
        (->ParseFailure nil position)))))

(defn fp-lazy [func]
  (fn [target position]
    (let [parser (func)]
      (parser target position))))

(defn fp-map [parser func]
  (fn [target position]
    (let [result (parser target position)]
      (if (= (type result) ParseSuccess)
        (->ParseSuccess (func (:result result)) (:new-position result))
        (->ParseFailure nil position)))))

(defn- find-first [func coll]
  (first (drop-while (complement func) coll)))

(defn fp-char [string]
  (fn [target position]
    (let [result (substring target position (+ position 1))]
      (if (find-first #(= (first result) (second %)) (map-indexed list string))
        (->ParseSuccess result (+ position 1))
        (->ParseFailure nil position)))))
