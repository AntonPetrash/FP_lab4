## МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС

### Звіт з лабораторної роботи 4
 "Функції вищого порядку та замикання"
 дисципліни "Вступ до функціонального програмування"

**Студент**: *Петраш Антон Степанович КВ-13*


**Рік**: *2025*

## Завдання:
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де це
доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями.
При цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

### Варіант 13(1):
Написати функцію add-prev-fn , яка має один ключовий параметр — функцію
transform . add-prev-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен
елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення
поточного елемента, а в комірці CDR знаходиться значення попереднього елемента
списку. Якщо функція transform передана, тоді значення поточного і попереднього
елементів, що потраплять у результат, мають бути змінені згідно transform .
transform має виконатись мінімальну кількість разів.
CL-USER> (mapcar (add-prev-fn) '(1 2 3))
((1 . NIL) (2 . 1) (3 . 2))
CL-USER> (mapcar (add-prev-fn :transform #'1+) '(1 2 3))
((2 . NIL) (3 . 2) (4 . 3))

**Код програми для першого завдання:**
```
(defun bubble-sort-func (lst &key (key #'identity) (test #'<))
  (let ((processed (mapcar (lambda (x) (cons (funcall key x) x)) lst)))
    (labels ((bubble-pass (lst)
               (if (or (null lst) (null (cdr lst))) 
                   (values lst nil)
                   (let ((first (car lst))
                         (second (cadr lst)))
                     (if (funcall test (car second) (car first))
                         (multiple-value-bind (rest flag)
                             (bubble-pass (cons first (cddr lst)))
                           (values (cons second rest) t))
                         (multiple-value-bind (rest flag)
                             (bubble-pass (cdr lst))
                           (values (cons first rest) flag)))))))
      (labels ((sort-helper (lst)
                 (multiple-value-bind (sorted flag)
                     (bubble-pass lst)
                   (if flag
                       (sort-helper sorted)
                       sorted))))
        (mapcar #'cdr (sort-helper processed))))))
        
(defun test-bubble-sort ()
  (format t "Test 1 (default): ~A~%"
          (equal (bubble-sort-func '(3 1 4 1 5 9)) '(1 1 3 4 5 9)))
  (format t "Test 2 (change test): ~A~%"
          (equal (bubble-sort-func '(5 3 8 6 2 7) :test #'>) '(8 7 6 5 3 2)))
  (format t "Test 3 (sort by length): ~A~%"
          (equal (bubble-sort-func '("apple" "qbc" "kiwi") :key #'length) '("qbc" "kiwi" "apple")))
  (format t "Test 4 (empty list): ~A~%"
          (equal (bubble-sort-func '()) '()))
  (format t "Test 5 (single element): ~A~%"
          (equal (bubble-sort-func '(1)) '(1))))

(test-bubble-sort)
```
**Результат тестування коду першого завдання:**
```
Output:

Test 1 (default): T
Test 2 (change test): T
Test 3 (sort by length): T
Test 4 (empty list): T
Test 5 (single element): T
```

**Код програми для другого завдання:**
```
(defun add-prev-fn (&key transform)
  (let ((prev nil))
    (lambda (current)
      (let ((pair (cons current prev)))
        (setf prev current) 
        (if transform
            (cons (funcall transform (car pair))
                  (if (cdr pair)
                      (funcall transform (cdr pair))
                      nil))
            pair)))))

(defun test-add-prev-fn ()
  (format t "Test 1: ~A~%" 
          (equal (mapcar (add-prev-fn) '(1 2 3)) 
                 '((1 . NIL) (2 . 1) (3 . 2))))
  (format t "Test 2: ~A~%" 
          (equal (mapcar (add-prev-fn :transform #'1+) '(1 2 3)) 
                 '((2 . NIL) (3 . 2) (4 . 3))))
  (format t "Test 3: ~A~%" 
          (equal (mapcar (add-prev-fn) '()) 
                 '()))
  (format t "Test 4: ~A~%" 
          (equal (mapcar (add-prev-fn) '(42)) 
                 '((42 . NIL))))
  (format t "Test 5: ~A~%" 
          (equal (mapcar (add-prev-fn :transform #'(lambda (x) (* x x))) '(2 3 4)) 
                 '((4 . NIL) (9 . 4) (16 . 9))))
  (format t "Test 6: ~A~%" 
          (equal (mapcar (add-prev-fn) '(1 "two" 3.0)) 
                 '((1 . NIL) ("two" . 1) (3.0 . "two")))))

(test-add-prev-fn)
```
**Результат тестування коду другого завдання:**
```
Output:

Test 1: T
Test 2: T
Test 3: T
Test 4: T
Test 5: T
Test 6: T
```
