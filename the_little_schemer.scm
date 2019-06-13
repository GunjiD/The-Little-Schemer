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
