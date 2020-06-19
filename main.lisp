;; add-element: 左のリストでindex番目の右のリストを吐き出す
(defun add-element (xs ys index)
  (let ((d (/ (* (nth index ys) -1) (nth index xs))))
    (mapcar #'+ (mapcar (lambda (x) (* x d))  xs) ys)))

(defun min-index (xs)
  (position
    (apply 'min xs)
    xs))

(defun all-but-last (l) (reverse (cdr (reverse l))))

(defun show-log (sts op)
  (let ((i 0))
    (dolist (st sts)
      (progn
        (format t "st~D: ~A~%" i st)
        (incf i)
        )
      )
    )
  (format t "op:  ~A~%~%" op)
  )

(defun compute (sts-op)
  (let ((sts (first (all-but-last sts-op))) (op (first (last sts-op))))
    (let ((index (min-index op)))
      (let ((index-row (min-index (mapcar #'(lambda (xs) (if (> (nth index xs)  0) (/ (first (last xs)) (nth index xs)) 1000)) sts))))
        (show-log sts op)
        (let ((i 0))
          (let ((new-sts-op
                 (list
                   (mapcar #'(lambda (st) (if (/= index-row i)
                                            (progn
                                              (incf i)
                                              (add-element (nth index-row sts) st index))
                                            (progn
                                              (incf i)
                                              (mapcar #'(lambda (x) (/ x (nth index st))) st))
                                            )) sts)
                   (add-element (nth index-row sts) op index)
                   )
                 ))
            (if (every #'(lambda (x) (>= x 0)) (all-but-last op))
              new-sts-op
              (compute new-sts-op))
            )
          )
        )
      )
    )
  )

;; 制約: op -> 最小化, スラック変数導入済み, すべての変数は0以上
;; Constraint: op -> Minimize, require stack variables, all variables are greater than or equal to 0
(defun main ()
  (let ((st1 '( 1  1  8 4 1 0 9))
        (st2 '( 4 -3 -2 -3 0 1 8))
        (op  '(-5 -2 -3 -1 0 0 0)))
    (let ((sts (list st1 st2)))
      (let ((sts-op (compute (list sts op))))
        )
      )
    )
  )

(main)
