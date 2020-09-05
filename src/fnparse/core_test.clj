(ns fnparse.core-test
  (:require [clojure.test :as t]
            [fnparse.core :refer :all]))

(t/deftest token
  (let [parse (p-token "foobar")]
    (t/is (= (parse "foobar" 0) (->ParseSuccess "foobar" 6)))
    (t/is (= (parse "foobar" 1) (->ParseFailure nil 1)))))

(t/deftest many
  (let [parse (p-many (p-token "hoge"))]
    (t/is (= (parse "hogehoge" 0) (->ParseSuccess ["hoge" "hoge"] 8)))
    (t/is (= (parse "" 0) (->ParseSuccess [] 0))))
  (let [parse (p-many (p-token "foobar"))]
    (t/is (= (parse "foo" 0) (->ParseSuccess [] 0)))))

(t/deftest choice
  (let [parse (p-many (p-choice (p-token "hoge") (p-token "fuga")))]
    (t/is (= (parse "" 0)) (->ParseSuccess [] 0))
    (t/is (= (parse "hogehoge" 0) (->ParseSuccess ["hoge" "hoge"] 8)))
    (t/is (= (parse "fugahoge" 0) (->ParseSuccess ["fuga" "hoge"] 8)))
    (t/is (= (parse "fugafoo" 0) (->ParseSuccess ["fuga"] 4)))))

(t/deftest seq
  (let [parse (p-seq (p-token "foo") (p-choice (p-token "bar") (p-token "baz")))]
    (t/is (= (parse "foobar" 0) (->ParseSuccess ["foo" "bar"] 6)))
    (t/is (= (parse "foobaz" 0) (->ParseSuccess ["foo" "baz"] 6)))
    (t/is (= (parse "foo" 0) (->ParseFailure nil 0)))))

(t/deftest option
  (let [parse (p-option (p-token "hoge"))]
    (t/is (= (parse "hoge" 0) (->ParseSuccess "hoge" 4)))
    (t/is (= (parse "fuga" 0) (->ParseSuccess nil 0)))))

(t/deftest regex
  (let [parse (p-regex "hoge")]
    (t/is (= (parse "hoge" 0) (->ParseSuccess "hoge" 4))))
  (let [parse (p-regex "([1-9][0-9]*)")]
    (t/is (= (parse "2014" 0) (->ParseSuccess "2014" 4)))
    (t/is (= (parse "01" 0) (->ParseFailure nil 0)))))

(t/deftest map
  (let [parse (p-map (p-token "hello") #(str % "という文字をパースできたよ"))]
    (t/is (= (parse "hello" 0) (->ParseSuccess "helloという文字をパースできたよ" 5)))
    (t/is (= (parse "foobar" 0) (->ParseFailure nil 0)))))

(t/deftest char
  (let [parse (p-char "abcdef")]
    (t/is (= (parse "a" 0) (->ParseSuccess "a" 1)))
    (t/is (= (parse "b" 0) (->ParseSuccess "b" 1)))
    (t/is (= (parse "g" 0) (->ParseFailure nil 0)))
    (t/is (= (parse "" 0) (->ParseFailure nil 0)))))

(t/run-tests)
