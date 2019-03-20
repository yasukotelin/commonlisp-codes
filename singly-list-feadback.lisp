;東さんのコード
;dotimes マクロですが (dotimes (var limit result) s-expr ...) のように書くと、limit回 ループを回したあと、 
;result を評価した値を返します。ですので reverse-singly-list は以下のようにも書けます
(defun reverse-singly-list(singly-list list-num)
  (let* ((cur-node singly-list) (pre-node nil))
    (dotimes (i list-num pre-node)
      (let* ((next-node (node-link cur-node)))
        (setf (node-link cur-node) pre-node)
        (setf pre-node cur-node)
        (setf cur-node next-node)))))


(defun reverse-singly-list(singly-list list-num)
  (let* ((cur-node singly-list) (pre-node nil))
    (dotimes (i list-num)
      (let* ((next-node (node-link cur-node)))
        (setf (node-link cur-node) pre-node)
        (setf pre-node cur-node)
        (setf cur-node next-node)))
    pre-node))
