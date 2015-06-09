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

(defun should-die (cell generation)
  (not (equal (count-alive-neighbors cell generation) 2))
  )

(eq (should-die '(5 5) '()) 't)                        ; it dies for loneliness
(eq (should-die '(5 5) '( (4 4) (5 4))) nil)           ; it doesn't die
(eq(should-die '(5 5) '( (4 4) (5 4) (6 4))) 't)       ; it dies for suffocation
