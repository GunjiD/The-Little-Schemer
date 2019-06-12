# 用語解説

参考URL、文献  

* http://www.haun.org/kent/lisp1/2.html
* [初めての人のためのLISP](https://www.amazon.co.jp/dp/4798119415/ref=cm_sw_r_tw_dp_U_x_FW7.CbVTHZ064)

## 第一の戒律

（仮）いかなる関数を表現するときも最初の質問はすべて ___null?___ にすべし。

## 第二の戒律

リストを作るには ___cons___ を用いるべし。

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

## Car

リストの先頭要素を求める

## Cdr

先頭の要素を除いたリストを求める

## Cons

先頭に要素を加えたリストを作る

## Cond

条件分岐

```
(cond
  ((hogehoge) fugafuga)
  ((higehige) mogemoge)
  (else #f))
```