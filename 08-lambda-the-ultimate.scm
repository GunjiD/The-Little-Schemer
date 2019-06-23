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
	(cons (car l) ((insertL-f test?) new old (cdr l)))))))

;;insertRを定義する
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons old
                (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old
                                   (cdr lat)))))))))

















