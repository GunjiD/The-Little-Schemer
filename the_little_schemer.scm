(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))
;; S式とはリスト構造である
(quote 'atom)
;; carの書き方
(print(car '(a b c)))
;;((a b c) x y z)のcdr
(print '((a b c) x y z))
;;car,cdrは引数として空でないリストをとる
;;consは引数として、最初にS式をとり２番めに任意のリストをとる
;;(cons a b) 答えはありません。なぜでしょう
;;lat?を定義する
;;S式の最後まで、どのS式もアトムかどうか尋ねる。もしリストが見つかれば直ちに#fを返す
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
;;第1の戒律まで進んだ
