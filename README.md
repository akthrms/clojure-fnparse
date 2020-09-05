# clojure-fnparse

Clojure製のパーサコンビネータ

## 機能

- p-token
- p-many
- p-choice
- p-seq
- p-option
- p-regex
- p-lazy
- p-map
- p-char

## 使用例

```
$ clj -m core "1+2-(3+1-(4))"
(1 + 2 - (3 + 1 - (4)))
```

## 参考

- [JavaScriptでパーサコンビネータのコンセプトを理解する(「正規表現だけに頼ってはいけない」の続き)](https://anatoo.hatenablog.com/entry/2015/04/26/220026)
- [fnparse.js](https://github.com/anatoo/fnparse.js)
