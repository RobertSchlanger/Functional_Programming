(defun preorder (tree)
	(COND
		((NULL tree) )
		((ATOM tree) (print tree))
		((LISTP tree)
			(preorder (car tree))
			(preorder (car (cdr tree)))
			(preorder (car (cdr (cdr tree))))
			)
		)
	)

(defun postorder (tree)
	(COND
		((NULL tree) )
		((ATOM tree) (print tree))
		((LISTP tree)
			(postorder (car (cdr tree)))
			(postorder (car (cdr (cdr tree))))
			(postorder (car tree))
			)
		)
	)

(defun inorder (tree)
	(COND
		((NULL tree) nil)
		((ATOM tree) (print tree))
		(T
			(inorder (car (cdr tree)))
			(inorder (car tree))
			(inorder (car (cdr (cdr tree))))
			)
		)
	)

(print "Preorder")
(preorder '(a (b (d h) (e i)) (c f g)))

(print "Postorder")
(postorder '(a (b (d h) (e i)) (c f g)))

(print "Inorder")
(inorder '(a (b (d h) (e i)) (c f g)))