(defstruct node parent left right)

(defparameter *nodes* nil)

; Standard input function.
(defun read-stdin ()
  (read-from-string (concatenate 'string "(" (read-line) ")")))

; Create empty node in the array.
(defun create-nodes(node-num)
  (setf *nodes* (make-array node-num))
  (dotimes (i (car node-num))
    (setf (aref *nodes* i) (make-node))))

; Create rooted tree.
(defun create-rooted-tree(node-num)
  (dotimes (i (car node-num))
    (let ((input (read-stdin)))
      (when (< 0 (cadr input)) 
        (setf (node-left (aref *nodes* i)) (caddr input))
        (let ((childs (cddr input)))
          (dotimes (j (cadr input))
            (setf (node-parent (aref *nodes* (car childs))) (car input))
            (setf (node-right (aref *nodes* (car childs))) (cadr childs))
            (setf childs (cdr childs))))))))

; Get parent index of the node.
(defun get-parent(node-index)
  (node-parent (aref *nodes* node-index)))

; Get depth of the node.
(defun get-depth(node-index &optional (depth 0))
  ; If parent is exist, continue to search parent.
  (let ((parent (get-parent node-index)))
    (if parent
      (get-depth parent (1+ depth))
      depth)))

; Get the node type is root ,leaf or internal.
(defun get-node-type(node-index)
  (let ((node (aref *nodes* node-index)))
    (cond ((not (node-parent node)) "root")
          ((not (node-left node)) "leaf")
          (t "internal node"))))

; Get child elements of node.
(defun get-childs(node-index)
  (let ((node (aref *nodes* node-index))
        (childs '()))
    (when (node-left node) 
      (setf childs (push (node-left node) childs))
      ; Search right node.
      (labels ((search-right (r)
                             (when r
                               (setf childs (push r childs))
                               (search-right (node-right (aref *nodes* r))))))
        (search-right (node-right (aref *nodes* (node-left node)))))
      (reverse childs))))

; Nil to -1 Converter
(defun parent-converter(parent)
  (if parent
    parent
    -1))

; Convert childs list to string.
(defun childs-converter(childs)
  (let ((str "["))
    (when childs
      (setf str (concatenate 'string str (princ-to-string (car childs))))
      (dolist (n (cdr childs))
        (setf str (concatenate 'string str ", " (princ-to-string n)))))
    (concatenate 'string str "]")))

(defun main()
  (let ((node-num (read-stdin)))
    (create-nodes node-num)
    (create-rooted-tree node-num)
    (dotimes (i (car node-num))
      (let ((index (princ-to-string i))
            (parent (princ-to-string (parent-converter (get-parent i))))
            (depth (princ-to-string (get-depth i)))
            (type (get-node-type i))
            (childs (childs-converter (get-childs i))))
        (fresh-line)
        (princ (concatenate 'string "node " index ": parent = " parent ", depth = " depth ", " type ", " childs))))))

(main)
