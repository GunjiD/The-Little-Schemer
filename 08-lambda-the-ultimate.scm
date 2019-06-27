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

(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
	((atom-to-function
	   (operator nexp))
	 (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp)))))))

;test
;result
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else 
	  (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;test ((multirember-f eq?) 'tuna '(shrimp salada tuna salada and tuna))
;result (shrimp salada salada and)

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
		  (multiremberT test? (cdr lat)))))))

;test (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
;result (shrimp salad salad and)

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
		       (lambda (newlat seen)
			 (col newlat
			      (cons (car lat) seen)))))
      (else
	(multirember&co a (cdr lat)
			(lambda (newlat seen)
			  (col (cons (car lat) newlat) seen)))))))

(define a-fried
  (lambda (x y)
    (null? y)))

;test
;(multirember&co 'tuna '() a-fried)
;#t
;(multirember&co 'tuna '(strawberries tuna and swordfish) a-fried)
;#f

;col は collector（収集子）の短縮形。収集子は continuation（継続）とも呼ばれる

#|
(define new-fried
  (lambda (newlat seen)
    (col newlat
	 (cons (car lat) seen))))
|#

;この定義を他の方法で書く
(define new-fried
  (lambda (newlat seen)
    (a-fried newlat
	 (cons 'tuna seen))))
;質問(multirember&co 'tuna '(and tuna) a-fried)
;#f

(define latest-fried
  (lambda (newlat seen)
    (a-fried (cons 'and newlat) seen)))

(define last-fried
  (lambda (x y)
    (length x)))

;いちごとメカジキの料理ってあるんだろうか

#|
;multiinsertLR
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons (new 
	       (cons oldL
		     (multiinsertLR new oldL oldR (cdr lat))))))
      ((eq? (car lat) oldR)
       (cons (oldR
	       (cons new
		     (multiinsertLR new oldL oldR (cdr lat))))))
      (else (cons ((car lat)
		   (multiinsertLR new oldL oldR (cdr lat))))))))
|#

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) 
       (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
			 (lambda (newlat L R)
			   (col (cons new
				      (cons oldL newlat))
				(+ 1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
				L (+ 1 R)))))
      (else
	(multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
			     (col (cons (car lat) newlat)
				  L R)))))))
;収集子関数を書く
(define col_newlat
  (lambda (newlat L R)newlat))
(define col_L
  (lambda (newlat L R) L))
(define col_R
  (lambda (newlat L R) R))

#|
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col_R)
2
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col_newlat)
(chips salty and salty fish or salty fish and chips salty)
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col_L)
2
|#

;even?はデフォルトで使えるので定義しない

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((even? (car l))
	  (cons (car l)
		(evens-only* (cdr l))))
	 (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
		  (evens-only* (cdr l)))))))

;test (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
;result ((2 8) 10 (() 6) 2)

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
	 ((even? (car l))
	  (evens-only*&co (cdr l)
			  (lambda (newl p s)
			    (col (cons (car l) newl)
				 (* (car l) p) s))))
	 (else (evens-only*&co (cdr l)
			      (lambda (newl p s)
				(col newl
				     p (+ (car l) s)))))))
      (else (evens-only*&co (car l)
			    (lambda (al ap as)
			      (evens-only*&co (cdr l)
					      (lambda (dl dp ds)
						(col (cons al dl)
						     (* ap dp)
						     (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product
		newl))))

;test 
;(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
;(38 1920 (2 8) 10 (() 6) 2)
