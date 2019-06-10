(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))
;; S式とはリスト構造である
(quote 'atom)
;; carの書き方
(car '(a b c))
;;掟の前まで終わった
