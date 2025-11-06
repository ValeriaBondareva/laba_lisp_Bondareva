(defun %extract-min-pair (pairs test)
  (cond
    ((null pairs) (error "extract-min: empty list"))
    ((null (cdr pairs)) (values (car pairs) nil))
    (t
     (multiple-value-bind (m rest) (%extract-min-pair (cdr pairs) test)
       (let ((x (car pairs)))
         (if (funcall test (car x) (car m))
             (values x (cons m rest))
             (values m (cons x rest))))))))

(defun selection-sort
    (lst &key (key #'identity) (test #'<))
 
  (labels ((decorate (xs)
             (mapcar (lambda (x) (cons (funcall key x) x)) xs))
           (sel (pairs)
             (if (null pairs)
                 nil
                 (multiple-value-bind (m rest) (%extract-min-pair pairs test)
                   (cons (cdr m) (sel rest))))))
    (sel (decorate lst))))

(defun add-prev-fn (&key transform)
  (let* ((use-transform (and transform))
         (tr (or transform #'identity))
     
         (have-prev nil)
         (prev-original nil)
         (prev-transformed nil))
    (lambda (x)
      (let* ((cur-transformed (if use-transform
                                  (funcall tr x)
                                  x))
             (res (cons cur-transformed
                        (if have-prev
                            prev-transformed
                            nil))))
        (setf have-prev t
              prev-original x
              prev-transformed cur-transformed)
        res))))

(defun check-sort (name fn input expected)
  (format t "~:[FAILED~;passed~]  ~a~%"
          (equal (funcall fn input) expected)
          name))

(defun test-selection-sort ()
 
  (check-sort "sel test 1: empty"
              (lambda (xs) (selection-sort xs))
              '()
              '())

  (check-sort "sel test 2: nums+dups"
              (lambda (xs) (selection-sort xs))
              '(3 2 1 3 2)
              '(1 2 2 3 3))

  (check-sort "sel test 3: already-sorted"
              (lambda (xs) (selection-sort xs))
              '(0 1 2 3 4)
              '(0 1 2 3 4))

  (check-sort "sel test 4: :key length"
              (lambda (xs) (selection-sort xs :key #'length :test #'<))
              '("dddd" "a" "bbb" "cc")
              '("a" "cc" "bbb" "dddd"))

  (check-sort "sel test 5: symbols lex order"
              (lambda (xs) (selection-sort xs
                                           :key  (lambda (s) (symbol-name s))
                                           :test #'string<))
              '(c a bb b)
              '(a b bb c)))
(defun test-add-prev-fn ()
  (check-sort "add-prev test 1: plain"
              (lambda (xs) (mapcar (add-prev-fn) xs))
              '(1 2 3)
              '((1 . NIL) (2 . 1) (3 . 2)))

  (check-sort "add-prev test 2: transform 1+"
              (lambda (xs) (mapcar (add-prev-fn :transform #'1+) xs))
              '(1 2 3)
              '((2 . NIL) (3 . 2) (4 . 3)))

  (check-sort "add-prev test 3: negatives"
              (lambda (xs) (mapcar (add-prev-fn) xs))
              '(-5 -1 -2)
              '((-5 . NIL) (-1 . -5) (-2 . -1)))

  (check-sort "add-prev test 4: symbols->strings upcase"
              (lambda (xs)
                (mapcar (add-prev-fn
                         :transform (lambda (s) (string-upcase (symbol-name s))))
                        xs))
              '(a bb c)
              '(("A" . NIL) ("BB" . "A") ("C" . "BB")))

  (check-sort "add-prev test 5: single element"
              (lambda (xs)
                (mapcar (add-prev-fn :transform (lambda (x) (* 2 x))) xs))
              '(7)
              '((14 . NIL))))
