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

(defun compute (sts-op)
  (let ((sts (first (all-but-last sts-op))) (op (first (last sts-op))))
    (let ((index (min-index op)))
      (let ((index-row
              (min-index (mapcar #'(lambda (xs)
                                     (if (> (nth index xs)  0)
                                       (/ (first (last xs)) (nth index xs))
                                       1000))
                                 sts))))
        (show-log sts op)
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
  (let ((st1 '(  4   3  2  1  0  8))
        (st2 '( -3  1  -1  0  1  3))
        (op  '( -8 -5  -6  0  0  0)))
    (let ((sts (list st1 st2)))
      (compute (list sts op)))))

(main)
