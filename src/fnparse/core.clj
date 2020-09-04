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

; (parse-hoge "hoge" 0)
; => #fnparse.core.ParseSuccess{:result "hoge", :new-position 4}
; (parse-hoge "ahoge" 1)
; => #fnparse.core.ParseSuccess{:result "hoge", :new-position 5}
; (parse-hoge "aaa" 0)
; => #fnparse.core.ParseFailure{:result nil, :new-position 0}

(defn token [s]
  (let [len (count s)]
    (fn [target position]
      (let [end (+ position len)]
        (if (= (substring target position end) s)
          (->ParseSuccess s end)
          (->ParseFailure nil position))))))

; ((token "foobar") "foobar" 0)
; => #fnparse.core.ParseSuccess{:result "foobar", :new-position 6}
; ((token "foobar") "foobar" 1)
; => #fnparse.core.ParseFailure{:result nil, :new-position 1}

(defn parse-hoge-many [target position]
  (loop [result []
         position' position]
    (let [end (+ position' 4)]
      (if (= (substring target position' end) "hoge")
        (recur (conj result "hoge") end)
        (->ParseSuccess result position')))))

; (parse-hoge-many "hogehoge" 0)
; => #fnparse.core.ParseSuccess{:result ["hoge" "hoge"], :new-position 8}
; (parse-hoge-many "ahogehoge" 1)
; => #fnparse.core.ParseSuccess{:result ["hoge" "hoge"], :new-position 9}
; (parse-hoge-many "aaaaaa" 0)
; => #fnparse.core.ParseSuccess{:result [], :new-position 0}

(defn many [parser]
  (fn [target position]
    (loop [result []
           position' position]
      (let [result' (parser target position')]
        (if (= (type result') ParseSuccess)
          (recur (conj result (:result result')) (:new-position result'))
          (->ParseSuccess result position'))))))

; ((many (token "hoge")) "hogehoge" 0)
; => #fnparse.core.ParseSuccess{:result ["hoge" "hoge"], :new-position 8}
; ((many (token "hoge")) "" 0)
; => #fnparse.core.ParseSuccess{:result [], :new-position 0}
; ((many (token "foobar")) "foo" 0)
; => #fnparse.core.ParseSuccess{:result [], :new-position 0}

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

; (def parse (many (choice (token "hoge") (token "fuga"))))
; (parse "" 0)
; => #fnparse.core.ParseSuccess{:result [], :new-position 0}
; (parse "hogehoge" 0)
; => #fnparse.core.ParseSuccess{:result ["hoge" "hoge"], :new-position 8}
; (parse "fugahoge" 0)
; => #fnparse.core.ParseSuccess{:result ["fuga" "hoge"], :new-position 8}
; (parse "fugafoo" 0)
; => #fnparse.core.ParseSuccess{:result ["fuga"], :new-position 4}

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

; (def parse (seqs (token "foo") (choice (token "bar") (token "baz"))))
; (parse "foobar" 0)
; => #fnparse.core.ParseSuccess{:result ("foo" "bar"), :new-position 6}
; (parse "foobaz" 0)
; => #fnparse.core.ParseSuccess{:result ("foo" "baz"), :new-position 6}
; (parse "foo" 0)
; => #fnparse.core.ParseFailure{:result nil, :new-position 0}

(defn option [parser]
  (fn [target position]
    (let [result (parser target position)]
      (if (= (type result) ParseSuccess)
        result
        (->ParseSuccess nil position)))))

; (def parse (option (token "hoge")))
; (parse "hoge" 0)
; => #fnparse.core.ParseSuccess{:result "hoge", :new-position 4}
; (parse "fuga" 0)
; => #fnparse.core.ParseSuccess{:result nil, :new-position 0}
