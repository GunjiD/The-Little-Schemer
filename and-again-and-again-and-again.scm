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























