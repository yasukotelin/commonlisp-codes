;標準入力関数
(defun read-stdin ()
  (read-from-string (concatenate 'string "(" (read-line) ")")))

;演算処理
(defun calc (ope num1 num2)
  (cond ((eq ope '+) (+ num1 num2))
        ((eq ope '-) (- num1 num2))
        ((eq ope '*) (* num1 num2))
        ((eq ope '/) (/ num1 num2))))

;スタックリスト
(defparameter *stack-list* '())

;逆ポーランドスタック処理
(defun reverse-polish(list)
  (if list
    (cond ((numberp (car list))
           (push (car list) *stack-list*)
           (reverse-polish (cdr list)))
          ((symbolp (car list))
           (let ((num1 (pop *stack-list*))
                 (num2 (pop *stack-list*)))
             (push (calc (car list) num2 num1) *stack-list*)
             (reverse-polish (cdr list))))
          (t (reverse-polish (cdr list))))
    (princ (pop *stack-list*))))

;関数呼び出し
(reverse-polish (read-stdin))
