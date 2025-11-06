<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 4</b><br/>
 "Функції вищого порядку та замикання"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студентка</b>: Бондарева Валерія Романівна КВ-22</p>
 <p align="right"><b>Рік</b>: 2025</p
## Загальне завдання

Завдання складається з двох частин:

1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
   роботи 3 з такими змінами:
   - використати функції вищого порядку для роботи з послідовностями (де/якщо
   це доречно, в разі, якщо функції вищого порядку не були використані при
   реалізації л.р. No3);
    - додати до інтерфейсу функції (та використання в реалізації) два ключових
    параметра: key та test , що працюють аналогічно до того, як працюють
    параметри з такими назвами в функціях, що працюють з послідовностями (р.
    12). При цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
   варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
   можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини 1

Алгоритм сортування вибором за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
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
 ```

### Тестові набори та утиліти першої частини

```lisp
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
```

### Тестування першої частини

```lisp
CL-USER> (test-add-prev-fn)
passed  add-prev test 1: plain
passed  add-prev test 2: transform 1+
passed  add-prev test 3: negatives
passed  add-prev test 4: symbols->strings upcase
passed  add-prev test 5: single element
NIL
```

## Варіант другої частини 1

Написати функцію add-prev-fn , яка має один ключовий параметр — функцію
transform . add-prev-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен
елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення
поточного елемента, а в комірці CDR знаходиться значення попереднього елемента
списку. Якщо функція transform передана, тоді значення поточного і попереднього
елементів, що потраплять у результат, мають бути змінені згідно transform .
transform має виконатись мінімальну кількість разів.

## Лістинг реалізації другої частини завдання

```lisp
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
```

### Тестові набори та утиліти другої частини

```lisp
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
```

### Тестування другої частини

```lisp
CL-USER> (test-selection-sort)
passed  sel test 1: empty
passed  sel test 2: nums+dups
passed  sel test 3: already-sorted
passed  sel test 4: :key length
passed  sel test 5: symbols lex order
NIL
```
