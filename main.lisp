;; add-element: xsのindex番目のピボットでysを掃き出す
(defun eliminate (xs ys index)
  (let ((d (/ (* (nth index ys) -1) (nth index xs))))
    (mapcar #'+
            (mapcar (lambda (x) (* x d))  xs)
            ys)))

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
        (incf i))))
  (format t "op:  ~A~%~%" op))


(defun show-log2 (sts op index index-row)
  (let ((i 0))
    (dolist (st sts)
      (format t "st~D: " i)
      (let ((j 0))
        (dolist (val st)
          (if (and (= j index) (= i index-row))
            (format t "~c[31m~D~c[0m " #\ESC (nth index (nth index-row sts)) #\ESC)
            (format t "~D " val)
            )
          (incf j)
          )
        (format t "~%")
        )

      (incf i)))
  (format t "op:  ")
  (dolist (val op)
    (format t "~D " val)
    )
  (format t "~%~%")
  )


(defun compute (sts-op)
  (let ((sts (first (all-but-last sts-op))) (op (first (last sts-op))))
    (let ((index (min-index (all-but-last op))))
      (let ((index-row
              (min-index (mapcar #'(lambda (xs)
                                     (if (> (nth index xs)  0)
                                       (/ (first (last xs)) (nth index xs))
                                       100000))
                                 sts))))
        (show-log2 sts op index index-row)
        (let ((i 0))
          (let ((new-sts-op (list
                              (mapcar #'(lambda (st) (if (/= index-row i)
                                                       (progn
                                                         (incf i)
                                                         (eliminate (nth index-row sts) st index))
                                                       (progn
                                                         (incf i)
                                                         (mapcar #'(lambda (x) (/ x (nth index st))) st))
                                                       )) sts)
                              (eliminate (nth index-row sts) op index))))
            (if (every #'(lambda (x) (>= x 0)) (all-but-last op))
              new-sts-op
              (compute new-sts-op))))))))

;;;; 制約: op(-z) -> 最大化, スラック変数導入済み, すべての変数は0以上
;;;; Constraint: op(-z) -> Maximize, require slack variables, all variables are greater than or equal to 0
(defun main ()
  (let ((st1 '(  5  5  5  7  7  7 -1  0  0  0  137))
        (st2 '(  8  0  0  6  0  0  0  1  0  0  6))
        (st3 '(  0  7  0  0  9  0  0  0  1  0 10))
        (st4 '(  0  0  6  0  0  7  0  0  0  1  5))
        (op  '(  3  2  1  7 -1  2  0  0  0  0 -140)))
    (let ((sts (list st1 st2 st3 st4)))
      (compute (list sts op)))))

(main)
