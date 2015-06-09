(defun contains (cell generation)
  (if (member cell generation) t nil)
  )

(contains '100 '(2 3 4))
(contains '2 '(2 3 4))


(defun neighbors (cell)
  (let (  (x (pop cell)) (y (pop cell))  )
    (list  (list (- x 1) (- y 1))  (list x (- y 1))  (list (+ x 1) (- y 1))
           (list (- x 1) y)                          (list (+ x 1)    y   )
           (list (- x 1) (+ y 1))  (list x (+ y 1))  (list (+ x 1) (+ y 1))
           )
    )
  )

(neighbors '(10 20))


(defun count-alive-neighbors (cell generation)
  (require 'cl-lib)
  (setq neig (neighbors cell))

  (length
   (cl-remove-if-not (lambda (e) (contains e generation)) neig)
   )
  )


(count-alive-neighbors '(10 11) '( (10 10) (11 10) (12 10)))
