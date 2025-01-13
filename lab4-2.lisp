(defun add-prev-fn (&key transform)
  (let ((prev nil)) ; Локальна змінна для збереження попереднього елемента
    (lambda (current)
      (let ((pair (cons current prev))) ; Створюємо точкову пару
        (setf prev current) ; Оновлюємо попередній елемент
        (if transform
            (cons (funcall transform (car pair))
                  (if (cdr pair)
                      (funcall transform (cdr pair))
                      nil)) ; Якщо CDR — NIL, transform не застосовується
            pair))))) ; Повертаємо пару без перетворення

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
