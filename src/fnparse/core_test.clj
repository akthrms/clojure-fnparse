(ns fnparse.core-test
  (:require [clojure.test :as t]
            [fnparse.core :refer :all]))

(t/deftest token
  (let [parse (fp-token "foobar")]
    (t/is (= (parse "foobar" 0) (->ParseSuccess "foobar" 6)))
    (t/is (= (parse "foobar" 1) (->ParseFailure 1)))))

(t/deftest many
  (let [parse (fp-many (fp-token "hoge"))]
    (t/is (= (parse "hogehoge" 0) (->ParseSuccess ["hoge" "hoge"] 8)))
    (t/is (= (parse "" 0) (->ParseSuccess [] 0))))
  (let [parse (fp-many (fp-token "foobar"))]
    (t/is (= (parse "foo" 0) (->ParseSuccess [] 0)))))

(t/deftest choice
  (let [parse (fp-many (fp-choice (fp-token "hoge") (fp-token "fuga")))]
    (t/is (= (parse "" 0)) (->ParseSuccess [] 0))
    (t/is (= (parse "hogehoge" 0) (->ParseSuccess ["hoge" "hoge"] 8)))
    (t/is (= (parse "fugahoge" 0) (->ParseSuccess ["fuga" "hoge"] 8)))
    (t/is (= (parse "fugafoo" 0) (->ParseSuccess ["fuga"] 4)))))

(t/deftest seq
  (let [parse (fp-seq (fp-token "foo") (fp-choice (fp-token "bar") (fp-token "baz")))]
    (t/is (= (parse "foobar" 0) (->ParseSuccess ["foo" "bar"] 6)))
    (t/is (= (parse "foobaz" 0) (->ParseSuccess ["foo" "baz"] 6)))
    (t/is (= (parse "foo" 0) (->ParseFailure 0)))))

(t/deftest option
  (let [parse (fp-option (fp-token "hoge"))]
    (t/is (= (parse "hoge" 0) (->ParseSuccess "hoge" 4)))
    (t/is (= (parse "fuga" 0) (->ParseSuccess nil 0)))))

(t/deftest regex
  (let [parse (fp-regex "hoge")]
    (t/is (= (parse "hoge" 0) (->ParseSuccess "hoge" 4))))
  (let [parse (fp-regex "([1-9][0-9]*)")]
    (t/is (= (parse "2014" 0) (->ParseSuccess "2014" 4)))
    (t/is (= (parse "01" 0) (->ParseFailure 0)))))

(t/deftest map
  (let [parse (fp-map (fp-token "hello") #(str % "という文字をパースできたよ"))]
    (t/is (= (parse "hello" 0) (->ParseSuccess "helloという文字をパースできたよ" 5)))
    (t/is (= (parse "foobar" 0) (->ParseFailure 0)))))

(t/deftest char
  (let [parse (fp-char "abcdef")]
    (t/is (= (parse "a" 0) (->ParseSuccess "a" 1)))
    (t/is (= (parse "b" 0) (->ParseSuccess "b" 1)))
    (t/is (= (parse "g" 0) (->ParseFailure 0)))
    (t/is (= (parse "" 0) (->ParseFailure 0)))))

(t/run-tests)
