(ns fnparse.core-test
  (:require [clojure.test :as t]
            [fnparse.core :as f]))

(t/deftest parse-hoge
  (t/is (= (f/parse-hoge "hoge" 0) (f/->ParseSuccess "hoge" 4)))
  (t/is (= (f/parse-hoge "ahoge" 1) (f/->ParseSuccess "hoge" 5)))
  (t/is (= (f/parse-hoge "aaa" 0) (f/->ParseFailure nil 0))))

(t/deftest token
  (let [parse (f/token "foobar")]
    (t/is (= (parse "foobar" 0) (f/->ParseSuccess "foobar" 6)))
    (t/is (= (parse "foobar" 1) (f/->ParseFailure nil 1)))))

(t/deftest parse-hoge-many
  (t/is (= (f/parse-hoge-many "hogehoge" 0) (f/->ParseSuccess ["hoge" "hoge"] 8)))
  (t/is (= (f/parse-hoge-many "ahogehoge" 1) (f/->ParseSuccess ["hoge" "hoge"] 9)))
  (t/is (= (f/parse-hoge-many "aaaaaa" 0) (f/->ParseSuccess [] 0))))

(t/deftest many
  (let [parse (f/many (f/token "hoge"))]
    (t/is (= (parse "hogehoge" 0) (f/->ParseSuccess ["hoge" "hoge"] 8)))
    (t/is (= (parse "" 0) (f/->ParseSuccess [] 0))))
  (let [parse (f/many (f/token "foobar"))]
    (t/is (= (parse "foo" 0) (f/->ParseSuccess [] 0)))))

(t/deftest parse-foo-or-bar
  (t/is (= (f/parse-foo-or-bar "foo" 0) (f/->ParseSuccess "foo" 3)))
  (t/is (= (f/parse-foo-or-bar "bar" 0) (f/->ParseSuccess "bar" 3)))
  (t/is (= (f/parse-foo-or-bar "hoge" 0) (f/->ParseFailure nil 0))))

(t/deftest choice
  (let [parse (f/many (f/choice (f/token "hoge") (f/token "fuga")))]
    (t/is (= (parse "" 0)) (f/->ParseSuccess [] 0))
    (t/is (= (parse "hogehoge" 0) (f/->ParseSuccess ["hoge" "hoge"] 8)))
    (t/is (= (parse "fugahoge" 0) (f/->ParseSuccess ["fuga" "hoge"] 8)))
    (t/is (= (parse "fugafoo" 0) (f/->ParseSuccess ["fuga"] 4)))))

(t/deftest seqs
  (let [parse (f/seqs (f/token "foo") (f/choice (f/token "bar") (f/token "baz")))]
    (t/is (= (parse "foobar" 0) (f/->ParseSuccess ["foo" "bar"] 6)))
    (t/is (= (parse "foobaz" 0) (f/->ParseSuccess ["foo" "baz"] 6)))
    (t/is (= (parse "foo" 0) (f/->ParseFailure nil 0)))))

(t/deftest option
  (let [parse (f/option (f/token "hoge"))]
    (t/is (= (parse "hoge" 0) (f/->ParseSuccess "hoge" 4)))
    (t/is (= (parse "fuga" 0) (f/->ParseSuccess nil 0)))))

(t/run-tests)