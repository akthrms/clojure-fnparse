(ns fnparse.core)

(defrecord ParseSuccess [result new-position])

(defrecord ParseFailure [result new-position])

(defn substring [s start end]
  (let [len (count s)]
    (.substring s start (if (< len end) len end))))

(defn parse-hoge [target position]
  (if (= (substring target position (+ position 4)) "hoge")
    (->ParseSuccess "hoge" (+ position 4))
    (->ParseFailure nil position)))

(defn token [s]
  (let [len (count s)]
    (fn [target position]
      (let [end (+ position len)]
        (if (= (substring target position end) s)
          (->ParseSuccess s end)
          (->ParseFailure nil position))))))

(defn parse-hoge-many [target position]
  (loop [result []
         position' position]
    (let [end (+ position' 4)]
      (if (= (substring target position' end) "hoge")
        (recur (conj result "hoge") end)
        (->ParseSuccess result position')))))

(defn many [parser]
  (fn [target position]
    (loop [result []
           position' position]
      (let [result' (parser target position')]
        (if (= (type result') ParseSuccess)
          (recur (conj result (:result result')) (:new-position result'))
          (->ParseSuccess result position'))))))

(defn parse-foo-or-bar [target position]
  (case (substring target position (+ position 3))
    "foo" (->ParseSuccess "foo" (+ position 3))
    "bar" (->ParseSuccess "bar" (+ position 3))
    (->ParseFailure nil position)))

(defn choice [& parsers]
  (fn [target position]
    (let [successes (for [parser parsers
                          :let [result (parser target position)]
                          :when (= (type result) ParseSuccess)]
                      result)]
      (if (< 0 (count successes))
        (first successes)
        (->ParseFailure nil position)))))

(defn seqs [& parsers]
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

(defn option [parser]
  (fn [target position]
    (let [result (parser target position)]
      (if (= (type result) ParseSuccess)
        result
        (->ParseSuccess nil position)))))

(defn regex [regexp]
  (fn [target position]
    (let [target' (.substring target position)
          matches (re-find (re-pattern regexp) target')
          result (if (coll? matches) (first matches) matches)]
      (if result
        (->ParseSuccess result (+ position (count result)))
        (->ParseFailure nil position)))))
