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
