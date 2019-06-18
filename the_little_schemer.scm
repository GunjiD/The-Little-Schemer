(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
;;member?を定義する
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) nil)
      (else (or (eq? (car lat) a)
		(member? a (cdr lat)))))))
;;remberを定義する
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
	    (rember a (cdr lat)))))))
;;firstsを定義する
;;なぜfirsts (cdr l)なのか -> 一度に一つのS式しか見ないため。残りを見るには cdr で先頭以外の要素を取り出す必要がある
(define firsts
  (lambda (l)
    (cond
      ((null? l) quote ())
      (else (cons (car (car l))
		  (firsts (cdr l)))))))
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
;;insertLを定義する
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
	      ((eq? (car lat) old)
	       (cons new lat))
	      (else (cons (car lat)
		    (insertL new old
			     (cdr lat)))))))))
;;substを定義する lat の中の最初に出てきた old を new に置き換える
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
	      ((eq? (car lat) old)
	       (cons new (cdr lat)))
	      (else (cons (car lat)
			  (subst new old
				 (cdr lat)))))))))
;;subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
	      ((or (eq? (car lat) o1) (eq? (car lat) o2))
	       (cons new (cdr lat)))
	      (else (cons (car lat)
			  (subst2 new o1 o2
			  (cdr lat2)))))))))
;;multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
	(cond
	  ((eq? (car lat) a)
	   (multirember a (cdr lat)))
	  (else (cons (car lat)
		      (multirember a
				   (cdr lat)))))))))
;;multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else 
	(cond
	  ((eq?  (car lat) old)
	   (cons old
	     (cons new
	       (multiinsertR new old
	         (cdr lat)))))
	   (else (cons (car lat)
		       (multiinsertR new old
				(cdr lat)))))))))
;;multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
	(cond
	  ((eq? (car lat) old)
	   (cons new
		 (cons old
		       (multiinsertL new old
				     (cdr lat)))))
	  (else (cons (car lat)
		      (multiinsertL new old
				    (cdr lat)))))))))
;;multisubst old を new で置換
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
	(cond
	  ((eq? (car lat) old)
	   (cons new
		 (multisubst new old
			     (cdr lat))))
	  (else (cons (car lat)
		      (multisubst new old
				  (cdr lat)))))))))
;;add1
(define add1
  (lambda (n)
    (+ n 1)))
;;sub1
(define sub1
  (lambda (n)
    (- n 1)))
;;o+ 加算の定義
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
;;o- 減算の定義
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
;;addtup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))
;;x 掛け算
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))
;;tup+
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote ()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
	(cons (o+ (car tup1) (car tup2))
	      (tup+
		(cdr tup1) (cdr tup2)))))))

;;o> 
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))
;;o<
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))
;;o=
(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))
;;exponentべき乗の計算
(define exponent
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x (exponent n (sub1 m)))))))
;;qo/割り算
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))
;;olength アトムの数
(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (olength (cdr lat)))))))
;;pick
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
;;rempick 引数 n を取り除く
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
		  (rempick (sub1 n)
			   (cdr lat)))))))
;;no-nums ラットから数字を取り出す
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
	      ((number?(car lat))
	       (no-nums (cdr lat)))
	      (else (cons (car lat)
			  (no-nums 
			    (cdr lat)))))))))
;;all-nums ラットからタップを取り出す
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
	      ((number? (car lat))
	       (cons (car lat)
		     (all-nums (cdr lat))))
	      (else (all-nums (cdr lat))))))))
;;eqan? 2つの引数が同じatomのとき、#tを返す
(define eqan?
  (lambda (a1 a2)
  (cond
    ((and (number? a1) (number? a2))
     (o= a1 a2))
    ((or (number? a1) (number? a2))
     #f)
    (else (eq? a1 a2)))))
;;(print (eqan? 1 1))
;;(print (eqan? 'atom 'atom))
;;(print (eqan? '1 'atom))
;;occur latの中にaが何回現れたかを数える関数
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
	(cond
	  ((eq? (car lat) a)
	   (add1 (occur a (cdr lat))))
	  (else (occur a (cdr lat))))))))
;;(print (occur 'pop '(pop up pop))
;;nが1のとき真
(define one?
  (lambda (n)
      (= n 1)))
;;(print (one? 1))
;;(print (one? 2))
;;one? を rempick に組み込んで使ってみる
;;(print (rempick '3 '(lemon meringure salty pie)))
;;rember*はlからaを取り除く
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
	 ((eq? (car l) a)
	  (rember* a (cdr l)))
	 (else (cons (car l)
		     (rember* a (cdr l))))))
       (else (cons (rember* a (car l))
		   (rember* a (cdr l)))))))
;;(print (rember* 'cup '((coffee cup ((tea) cup) (and (hick)) cup))))
;;(print (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))
;;(lat? l)か。 答えは#f
;;(print (lat? '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))
;;(car l)はアトムか。 いいえ
;;(print (car '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))
;;insertR*はoldがどこにあってもnewを挿入する
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
	 ((eq? (car l) old)
	  (cons old 
		(cons new
		      (insertR* new old
				(cdr l)))))
	 (else (cons (car l)
		     (insertR* new old
			       (cdr l))))))
      (else (cons (insertR* new old
			     (car l))
		   (insertR* new old
			     (cdr l)))))))
;;(print  (insertR* 'roast 'chuck  '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))

;;(print (multiinsertR 'topping 'fudge '(ice cream with fudge topping for fudge)))
;;(print (no-nums '(5 pears 6 prunes 9 dates)))
;;(print (all-nums '(5 pears 6 prunes 9 dates)))
