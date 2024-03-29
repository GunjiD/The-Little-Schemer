(define member?
(lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      ((set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '())
(set? '(apple 3 pear 4 9 apple 3 4))

#|
;最初の makeset
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
		  (makeset (cdr lat)))))))
(makeset '(apple peach pear peach plum apple lemon))
;result () <- 本の結果と違うので2周目読むときに直す
|#

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

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
		  (makeset
		    (multirember (car lat)
				 (cdr lat))))))))

(makeset '(apple peach pear peach plum apple lemon))

;subset? t1がt2の部分集合であれば #t なければ #f
#|
(define subset?
  (lambda (set1 set2)
    (cond
    ((null? set1) #t)
    ((member? (car set1) set2)
     (subset? (cdr set1) set2))
    (else #f))))
;test
(subset? '(5 chicken wings) '(5 hamburgers 2 pices fried chicken and light duckling wings))
;result #t
(subset? '(4 pounds of horseradish)  '(four pounds chicken and5ounces horseradish))
;resunt #f
|#
;andを使ってさらに短く書く
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
		 (subset? (cdr set1) set2))))))

;eqset? equal set 等しい集合の略（多分）
(define eqset?
  (lambda (set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1))))
;test
(eqset? '(5 chicken wings) '(5 chicken wings))
;result #t
(eqset? '(5 chicken wings) '(chicken wings 5))
;result #t
(eqset? '(5 chicken wings) '(chicken wings))
;result #f

;intersect? set1のアトムがset2に含まれているか
#|
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      (else
	(cond
	  ((member? (car set1) set2) #t)
	  (else (intersect?
		  (cdr set1) set2)))))))
|#
#|
;もっと短く書く
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect?
	      (cdr set1) set2)))))
|#
;orを使って書く
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
		(intersect?
		  (cdr set1) set2))))))
;test
(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
;result #t

;intersect 一致したアトムをリストで返す
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
	     (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
;test
(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
;result (and macaroni)

;union 和集合
(define union
  (lambda (set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2)
     (union (cdr set1) set2))
    (else 
      (cons (car set1)
	    (union (cdr set1) set2))))))
;test
(union '(stewed tomatoes and macaroni) '(macaroni and cheese))
;result (and macaroni)

;xxx set1に含まれているがset2には含まれないすべてのアトムを返す。差集合
(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (xxx (cdr set1) set2))
      (else
	(cons (car set1)
	      (xxx (cdr set1) set2))))))
;test
(xxx '(stewed tomatoes and macaroni) '(macaroni and cheese))
;result (stewed tomatoes)

;intersectall
(define intersectall
  (lambda (l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else
      (intersect (car l-set)
		 (intersectall (cdr l-set)))))))
;test
(intersectall '((a b c) (c a d e) (e f g h a b)))
;result (a)
(intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with lost of apples)))
; result (6 and)

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
;test
(a-pair? '(pair pair))
;result #t
(a-pair? '(pair pair pair))
;result #f

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

;l は (apples peaches pumpkin pie) rel（レル）ではない。ここでレルは関係(relation)を表す
;relの例 ((4 3) (4 2) (7 6) (6 2) (3 4))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))
;有限関数か？ 各ペアの要素が全ての第一要素と一致しない
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;test
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;result #t
(fun? '((b 4) (b 0) (b 9) (e 5) (g 4)))
;result #f

;revrel reverse rel
#|
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build
		    (second (car rel))
		    (first (car rel)))
		  (revrel (cdr rel)))))))
|#

;revpair 関数を使うとどう記述できるか
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
		  (revrel(cdr rel)))))))


;test
(revrel '((8 a) (pumpkin pie) (got sick)))
;result ((a 8) (pie pumpkin) (sick got))

;全単射。群論を読もう！
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
;test
(one-to-one? '((grape raisin) (plum prune) (stewed prune)))
;result  
(one-to-one? '((chocolate chip) (doughy cookie)))
;result 




















