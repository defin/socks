(in-package socks)

(defmacro defun-if (test-form name arglist &body body)
  `(progn (define-compiler-macro ,name (&whole form &rest args)
	     (declare (ignore args))
	     (when ,test-form
	       `,form))
	  (defun ,name ,arglist ,@body)))

(defmacro defmethod-if (test-form name arglist &body body)
  `(progn (define-compiler-macro ,name (&whole form &rest args)
	     (declare (ignore args))
	     (when ,test-form
	       `,form))
	  (defmethod ,name ,arglist ,@body)))

(deftype fixed-width-integer (&optional fwi)
  `(complex ,fwi))

(defun fix-integer-width (magnitude width)
  (complex magnitude width))

(defun fixed-width-integer-magnitude (fwi)
  (realpart fwi))

(defun fixed-width-integer-width (fwi)
  (imagpart fwi))

(defmacro and1 (&rest clauses)
  (let ((first (gensym "AND-FIRST")))
    `(let ((,first ,(first clauses)))
       (and (and ,first ,@(rest clauses))
	    ,first))))

(defun base256-string (x &key (width 1))
  (loop with string = (make-array width :element-type 'character)
	;; start at the big end so the result is in network byte order
	for place from (1- width) downto 0
	for i from 0
	do (setf (aref string i) (code-char (ldb (byte 8 (* 8 place)) x)))
	finally (return string)))

(defun base256-integer (string)
  (loop for place from (1- (length string)) downto 0
	for i from 0
	with integer = 0
	do (setf integer (dpb (char-code (aref string i)) (byte 8 (* 8 place)) integer))
	finally (return integer)))

(defstruct (string-builder) (string (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)))

(defmethod build-string ((builder string-builder) (new-character character))
  (with-accessors ((string string-builder-string))
      builder
    (vector-push-extend new-character string)))

(defmethod build-string ((builder string-builder) (new-character integer))
  (with-accessors ((string string-builder-string))
      builder
    (vector-push-extend (code-char new-character) string)))

(defmethod built-string ((builder string-builder))
  (with-accessors ((string string-builder-string))
      builder
    (let ((received-string (copy-seq string)))
      (setf (fill-pointer string) 0)
      received-string)))

