(defstruct node
            val
            link)

(defun cons-node(val node)
  (if (node-val node)
    (make-node :val val :link node)
    (make-node :val val :link nil)))

(defun print-list(node)
  (fresh-line)
  (cond 
    ((node-link node)
      (princ (car (node-val node)))
      (print-list (node-link node)))
    (t
      (princ (car (node-val node))))))

(defun reverse-list(node node-num)
  (let* ((re-node (make-node :val nil :link nil)))
    (dotimes (i node-num)
      (setf re-node (cons-node (node-val node) re-node))
      (setf node (node-link node)))
    re-node))

(defun make-single-linked-list()
  (let* ((node (make-node :val nil :link nil)))
    (let* ((input-num (car (read-stdin))))
      (dotimes (i input-num)
        (setf node (cons-node (read-stdin) node)))
      (princ "-- Reverse --")
      (print-list node)
      (setf node (reverse-list node input-num))
    )))

(defun count-node-num(node)
  )

(defun single-linked-list-copy(node)
  )

(defun read-stdin ()
  (read-from-string (concatenate 'string "(" (read-line) ")")))

(defun main()
  (let* ((single-linked-list nil))
    (setf single-linked-list (make-single-linked-list))
    (fresh-line)
    (princ "-- forward --")
    (print-list single-linked-list)
    (fresh-line)
    (princ "-- copied list --")
    ))


(main)
