(defun contains (cell generation)
  (if (member cell generation) t nil)
  )

(eq (contains '100 '(2 3 4)) 'nil)
(eq (contains '2 '(2 3 4)) 't)


(defun neighbors (cell)
  (let (  (x (pop cell)) (y (pop cell))  )
    (list  (list (- x 1) (- y 1))  (list x (- y 1))  (list (+ x 1) (- y 1))
           (list (- x 1) y)                          (list (+ x 1)    y   )
           (list (- x 1) (+ y 1))  (list x (+ y 1))  (list (+ x 1) (+ y 1))
           )
    )
  )

(equal
 (neighbors '(10 20))
 '( (9 19) (10 19) (11 19) (9 20) (11 20) (9 21) (10 21) (11 21))
 )


(defun count-alive-neighbors (cell generation)
  (require 'cl-lib)
  (setq neig (neighbors cell))

  (length
   (cl-remove-if-not (lambda (e) (contains e generation)) neig)
   )
  )


(eq (count-alive-neighbors '(10 11) '( (10 10) (11 10) (12 10)))
    2)

(defun should-die (cell generation)
  (not (equal (count-alive-neighbors cell generation) 2))
  )

(eq (should-die '(5 5) '()) 't)                        ; it dies for loneliness
(eq (should-die '(5 5) '( (4 4) (5 4))) nil)           ; it doesn't die if surrounded by 2 cells
(eq(should-die '(5 5) '( (4 4) (5 4) (6 4))) 't)       ; it dies for suffocation


(defun should-get-to-life (cell generation)
  (equal (count-alive-neighbors cell generation) 2)
  )

(eq (should-get-to-life '(5 5) '()) nil)
(eq (should-get-to-life '(5 5) '( (5 4) (6 4))) t)
