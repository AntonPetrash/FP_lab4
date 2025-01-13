(defun bubble-sort-func (lst &key (key #'identity) (test #'<))
  (let ((processed (mapcar (lambda (x) (cons (funcall key x) x)) lst))) ; Обчислення ключів
    (labels ((bubble-pass (lst)
               (if (or (null lst) (null (cdr lst))) 
                   (values lst nil) ; Якщо список порожній або один елемент
                   (let ((first (car lst))
                         (second (cadr lst)))
                     (if (funcall test (car second) (car first))
                         ;; Потрібна заміна
                         (multiple-value-bind (rest flag)
                             (bubble-pass (cons first (cddr lst)))
                           (values (cons second rest) t))
                         ;; Без заміни
                         (multiple-value-bind (rest flag)
                             (bubble-pass (cdr lst))
                           (values (cons first rest) flag)))))))
      ;; Основний рекурсивний цикл
      (labels ((sort-helper (lst)
                 (multiple-value-bind (sorted flag)
                     (bubble-pass lst)
                   (if flag
                       (sort-helper sorted)
                       sorted))))
        ;; Повертаємо тільки самі елементи без ключів
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
