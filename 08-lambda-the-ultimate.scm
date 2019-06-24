;第5章最後の rember

#|
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((oequal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))
|#

;rember-f? test? a l
(define rember-f?
  (lambda (test? a l)
    (cond
      ((null? '()))
      (else
	(cond
	  ((test? (car l) a) (cdr l))
	  )))))

;test
(rember-f? '= '5 '(6 2 5 3))
;result
(rember-f? 'eq? 'jelly '(jelly beans are good))
;result
(rember-f? 'equal? '(pop corn) '(lemonade (pop corn) and (cake)))
;result

#|
(lambda (a)
  (lambda (x)
    (eq? x a)))
;これは引数としてaを渡されると、関数
(lambda (x)
  (eq? x a))
;を返す。これをカリー化という。
|#
;上記の関数に名前をつけて定義する

(define eq?-c
  (lambda (a)
  (lambda (x)
    (eq? x a))))

;test
((eq?-c 'tuna) 'tuna)
;result #t

(define eq?-salada (eq?-c 'salada))
;test
(eq?-salada 'salada)
;reult #t

;eq?-salada 定義しなくてもこれで済む
((eq?-c 'tuna) 'tuna)

;引数test?をとりremberのeq?をtest?置き換えた関数を返す
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
	 ((null? l) '())
	 ((test? (car l) a) (cdr l))
	 (else (cons (car l) ((rember-f test?) a (cdr l))))))))
;test
((rember-f eq?) 'tuna '(shrimp salada and tuna salada))
;result (shrimp salada and salada)

;;insertL-f
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
	((null? l) '())
	((test? (car l) old)
	 (cons new (cons old (cdr l))))
	(else (cons (car l) 
		    ((insertL-f test?) new old
				       (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
	((null? l) '())
	((test? (car l) old)
	 (cons old (cons new (cdr l))))
	(else (cons (car l)
		    ((insertR-f test?) new old
				       (cdr l))))))))
#|
;3つの引数をとり
;第2の引数を第3の引数に cons した結果に
;第1の引数を cons する
;seqL
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
;seqR
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
;seq が seqL のときは insertL を返し
;seq が seqR のときは insertR を返す
;insert-g を定義する
|#

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
	((null? l) '())
	((eq? (car l) old)
	 (seq new old (cdr l)))
	(else (cons (car l)
		    ((insert-g seq) new old
				    (cdr l))))))))

;(define insertL (insert-g seqL))
;(define insertR (insert-g seqR))

(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

#|
(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old)
       (cons new (cdr l)))
      (else (cons (car l)
		  (subst new old (cdr l)))))))
|#

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
;test (yyy 'sausage '(pizza with sausage and bacon))
;result (pizza with and bacon)

;6章の value 参照
#|
(define value
  (lambda (nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) (quote 'o+))
     (o+ (value (1st-sub-exp))
	 (value (2nd-sub-exp))))
    ((eq? (operator nexp) (quote 'x))
     (x (value (value (1st-sub-exp)))
	(value (value (2nd-sub-exp)))))
    (else
      (exponent (1st-sub-exp nexp)
		(2nd-sub-exp nexp))))))
(define 1st-sub-exp
  (lambda (aexp)
    ((car aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    ((car (cdr aexp)))))
(define operator
  (lambda (aexp)
    (car aexp)))
|#

(define 1st-sub-exp
  (lambda (aexp)
    ((car aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    ((car (cdr aexp)))))
(define operator
  (lambda (aexp)
    (car aexp)))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) 'o+)
      ((eq? x (quote -)) 'o-)
      (else exponent))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
	((atom-to-function
	   (operator nexp))
	 (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp)))))))











