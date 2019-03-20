(defparameter *stack-list* '())

(defun stack-push (num list)
  (append list `(,num)))

(defun stack-pop (list)
  (if list


(setf *stack-list* (stack-push 2 *stack-list*))
(setf *stack-list* (stack-push 3 *stack-list*))
(print *stack-list*)
