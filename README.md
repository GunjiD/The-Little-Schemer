# 用語解説

参考URL、文献  

* http://www.haun.org/kent/lisp1/2.html

## 第1の戒律

アトムのリスト ___lat___ を再帰せしときは、2つの質問、  
___(null? lat)___ と ___else___ を行うべし。  
数 ___n___ を再帰せしときは、2つの質問、  
___(zero? n)___ と ___else___ を行うべし。  
S 式のリスト ___l___ を再帰せしときは、3つの質問、  
___(null? l)___ 、 ___(atom? (car l))___ 、 ___else___ を行うべし。  

## 第2の戒律

リストを作るには ___cons___ を用いるべし。

## 第3の戒律

リストを作らんとせしときは、最初の要素になるものを  
記述し、しかる後にそれを自然なる再帰に ___cons___ すべし。

## 第4の戒律

再帰のときは少なくとも1つの引数を変えるべし。  
アトムのリスト ___lat___ を再帰せしときは、  
___(cdr lat)___ を用いるべし。  
数 ___n___ を再帰せしときは、
___(sub1)___ を用いるべし。  
S 式のリスト ___l___ を再帰せしときは、  
___(null? l)___ も ___(atom? (car l))___ も真でないならば、  
___(car l)___ と ___(cdr l)___ を用いるべし。  
必ず最終条件に向かって変化すべし。  
変化せし引数は、必ず最終条件でテストすべし。すなわち、  
___cdr___ を用いるときは、最後に ___null?___ で、  
___sub1___ を用いるときは、最後に ___zero?___ でテストすべし。  

## 第5の戒律

＋ で値を作らんとせしときは、行を終えるときに常に値として 0 を用うべし。  
なんとなれば、 0 を加うるは加算の値を変えぬからなり。  
× で値を作らんとせしときは、行を終えるときに常に値として 1 を用うべし。  
なんとなれば、 1 を掛けるは乗算の値を変えぬからなり。  
___cons___ で値を作らんとせしときは、行を終えるときに常に値として ___()___ を考えるべし。  

## 第6の戒律

関数が正しいときのみ簡約化せよ。  

## 第7の戒律

性質を同じくするすべての構成部分について再帰すべし。すなわち、  

* リストのすべての部分リストについて。
* 算術式のすべての部分式について。

## 第8の戒律

表現から抽象化するに際し、補助関数を使用すべし。  
___(define value)___ を参照。  

## 第9の戒律

新しき関数においては共通のパターンを抽象化すべし。  

## 第10の戒律

同時に2つ以上の値を集める際には関数を作るべし。  

## atom(アトム)

数値などのように、データ型としてそれ以上分解できないもの  
 シンボル（要調査）以外のアトムを評価するとそれ自身が評価結果となる  

```
	┏ アトム──┬ 数値
	┃           ├ 文字列
	┃           ├ シンボル
	┃           └ nil
	┗ リスト
```

## S式

lispの構文そのもの  
「道はnilを生ず、nilはアトムを生じ、アトムはS式を生じ、S式は万物を生ず」  


```
(例)
(hogehoge)
(cdr '(sushi))
```

## タップ

数のリストのこと。

```
(1 2 3 4 5)
```

## Car

リストの先頭要素を求める

## Cdr

先頭の要素を除いたリストを求める

## Cons

先頭に要素を加えたリストを作る

## Cond

条件分岐。上から順に実行される

```
(cond
  ((hogehoge) fugafuga)
  ((higehige) mogemoge)
  (else #f))
```

## 感想・メモ
6/18  
最初は全然わからなかったが、数遊びの最後のあたりで再帰がなんとなく書けるようになってきた。  
6/20  
かなり複雑になってきた。この本を1度だけで読み終えてはいけない理由が少しわかった気がする。 
6/20  
7章集合論で草。  
