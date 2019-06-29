;entry(エントリ)とはリストのペアであり、その第一のリストが集合であるもの。さらに2つのリストは同じ長さでなければならない
;例
'((appetizer entree beverage) (patee boeuf vin))
'((appetizer entree beverage) (beer beer beer))
'((beverage dessert) ((food is) (number one with us)))

;(define new-entry build)

;ペアの表現を作るときと、ペアの表現から部分を取り出すときに使う。
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

(define lookup-in-entry
  (lambda (name entry entry-f)
  (lookup-in-entry-help name
			(first entry)
			(second entry)
			entry-f)))

(define lookup-in-entry-help
  (lambda (name names value entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name)
       (car value))
      (else
	(lookup-in-entry-help name
			      (cdr names)
			      (cdr value)
			      entry-f)))))

;(lookup-in-entry 'entree '((appertizer entree beverage) (food tastes good)))

;table(テーブル、環境)とは entry のリストだとする
;例
'(((appetizer entree beverage) (pate boeuf vin)) ((beverage dessert) ((food is) (number one with us))))

;entry と table を引数としてとり、そのエントリを古いテーブルの先頭に付け加えて新しいテーブルを作る関数
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
			     (car table)
			     (lambda (name)
			       (lookup-in-table name
						(cdr table)
						table-f)))))))
;test
;(lookup-in-table 'entree '(((entree dessert) (spaghetti spumoni)) ((appetizer entree beverage) (food tasted good))) (lambda (n) '()))
;spaghetti

(lambda (name)
  (lookup-in-table name
		   (cdr table)table-f))

;これは最初の entry に名前がなかったときに実行される関数


(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

;与えられたS式に対して正しいアクション（関数）を生成する関数
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1?) *const)
      ((eq? e 'sub1?) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
	 ((eq? (car e) 'quote) *quote)
	 ((eq? (car e) 'lambda) *lambda)
	 ((eq? (car e) 'cond) *cond)
	 (else *application)))
      (else *application))))

;関数 value は Scheme における eval と同じ働きをする
(define value
  (lambda (e)
    (meaning e '())))
(define meaning
  (lambda (e)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive) e))))

(define *quote
  (lambda (e table)
    (text-of)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

;非基本関数と基本関数の違いは
;基本関数は何をするものか知っている関数
;非基本関数は、引数と関数本体によって定義される関数
;非基本関数を使うときには、仮引数と関数本体を覚えて置く必要がある

;これらをリストで表現する

(define *lambda
  (lambda (e table)
    (build 'non-primitive)
    (cons table (cdr e))))

;(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))

(define  table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))
(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

;(*cond '(cond (coffee klatsch) (t party)) '(((coffee) t) ((klatsch party) (5 (6)))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
	(cons (meaning (car args) table)
	      (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))
;apply は実装されているので applys
(define applys
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))





















































