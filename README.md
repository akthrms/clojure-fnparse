# clojure-fnparse

Clojure製のパーサコンビネータ

## 機能

- token
- many
- choice
- seq
- option
- regex
- lazy
- map
- char

## 使用例

```
$ clj -m core "1+2-(3+1-(4))"
(1 + 2 - (3 + 1 - (4)))
```

## 参考

- [JavaScriptでパーサコンビネータのコンセプトを理解する(「正規表現だけに頼ってはいけない」の続き)](https://anatoo.hatenablog.com/entry/2015/04/26/220026)
- [fnparse.js](https://github.com/anatoo/fnparse.js)
