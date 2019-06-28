;test
;(looking 'caviar '(6 2 4 caviar 5 7 3))
;#t
;(looking 'caviar '(6 2 grits caviar 5 7 3))
;#f

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;test
;(pick 6 '(6 2 4 caviar 5 7 3))
;7
;(pick 7 '(6 2 4 caviar 5 7 3))
;3

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
    (lambda (a lat)
          (keep-looking a (pick 1 lat) lat)))

;sorn は Symbol Or Number の略
;再帰できないパターンがある。
;例）(looking 'caviar '(7 1 2 caviar 5 6 3))
;これは常に looking, looking, looking, ... を繰り返す

;ある引数に対してはゴールに到達しない関数でもっと短いもの
(define eternity
  (lambda (x)
    (eternity x)))
;これは部分関数である


(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define third
  (lambda (p)
    (car (cdr (cdr p)))))
(define build
  (lambda (a1 a2)
    (cons a1
	  (cons a2 '()))))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))
;test
;(shift '((a b) c))
;
;(a (b c))
;(shift '((a b) (c d)))
;
;(shift '(a (b (c d))))

;atom?
(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

;a-pair? 2つのアトムからなるリストなら #t
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define align
  (lambda (para)
    (cond
      ((atom? para) para)
      ((a-pair? (first para))
       (align (shift para)))
      (else (build (first para)
		   (align (second para)))))))
;align は7の戒律に反している

(define length*
  (lambda (para)
    (cond
      ((atom? para) 1)
      (else
	(+ (length* (first para))
	   (length* (second para)))))))

(define weight*
  (lambda (para)
    (cond
      ((atom? para) 1)
      (else
	(+ (* (weight* (first para)) 2)
	   (weight* (second para)))))))
;test
;(weight* '(a (b c)))
;5
;(weight* '((a b) c))
;7

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
		  (revrel(cdr rel)))))))

;7章の revpair を使う。 shuffle と revpair は第一要素がペアのときにペアの要素を交換
(define shuffle
  (lambda (para)
    (cond
      ((atom? para) para)
      ((a-pair? (first para))
       (shuffle (revpair para)))
      (else
	(build (first para)
	       (shuffle (second para)))))))

;test
;(shuffle '(a (b c)))
;(a (b c))
;(shuffle '(a b))
;(a b)
;引数が ((a b) (c d))のとき para は ((c d) (a b))
;これは (revpair para) が ((a b) (c d)) のときの (shuffle (revpair para)) の値を知る必要があるということ
;このことから shuffle は全関数ではないないということがわかる。なぜなら、ペアの要素を再び交換しているが、元に戻っているしまうためである

;次の関数は全関数か
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
	(cond
	  ((even? n) (C (/ n 2)))
	  (else
	    (C (+ 1 (* 3 n)))))))))
;0に対しては値を持たないが、それ以外の引数に対して全関数であるかどうかは不明（未解決問題）
;Lothar Collatz（ローター・コラッツ）コラッツ問題
;n が偶数の場合 n/2 
;n が奇数の場合 n*3+1
;どんな初期値から初めても有限回の操作のうちに必ず1に到達するという主張をコラッツ予想という

(define A
  (lambda (n m)
    (cond
      ((zero? n) (+ 1 m))
      ((zero? m) (A (- 1 n) 1))
      (else
	(A (- 1 n)
	   (A n (- 1 m)))))))
;test
;(A 1 0)
;2
;(A 1 1)
;3
;(A 2 2)
;7

;Wilhelm Friedrich Ackermann(ヴィルヘルム・アッカーマン)
;アッカーマン関数。詳しくはググる。ざっくりいうと与える数が大きくなると爆発的に計算量が大きくなるという特徴がある。



;ある関数がすべての引数に対して値を返すかどうかを教えてくれる関数を書けるだろうか
;これは全関数である。常に #t or #f を返す
(define will-stop?
  (lambda (f)
    ...))
;例 f が length のとき (wiil-stop? f) の値は何か
;(length l) の l が () のとき #t （になるはず）
;(willstop? (etetnity '())) は値を返さない
;これは #f であることを示している

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
	 (eternity '()))))

;(will-stop? last-try) が #f だとすると (and #f ...) は常に #f である
;なので (and #f (eternity '()))
;(last-try '()) は停止する。
;つづいて #t になる値について論じる
;結論 : will-stop? は正確に記述できるが我々の言語では定義できない最初の関数である
;Alan Mathieson Turing(アラン・マシスン・チューリング)
;Kurt Gödel(クルト・ゲーデル)
;上記2名を参照することで理解できる（要参考資料）

;ここからは紙で行う

;適用順 Y コンビネータ

(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
	   ((mk-lenght mk-length) x))))))

;なぜ Y が動作するのかはもう一度この章を読めばわかる
(define Y
  (lambda (f) (f f))
  (lambda (f)
    (le (lambda (x) ((f f) x)))))































