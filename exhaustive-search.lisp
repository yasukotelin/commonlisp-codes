; standard input function
(defun read-stdin ()
  (read-from-string (concatenate 'string "(" (read-line) ")")))

; Return 'T' when the 'target-num' is created from the 'num-list'.
(defun solvep (num-list target-num)
  (cond ((= target-num 0) t)
        ; If 'num-list' is empty, return 'Nil'.
        (num-list 
          (or (solvep (cdr num-list) (- target-num (car num-list)))
              (solvep (cdr num-list) target-num)))))

(defun main()
  ; Receive input values
  (let* ((n (read-stdin))
         (a (read-stdin))
         (q (read-stdin))
         (m (read-stdin)))
    (dolist (target-num m) 
      (fresh-line)
      (princ (if (solvep a target-num) 
               "yes"
               "no")))))

(main)
