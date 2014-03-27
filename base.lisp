(in-package socks)

(defconstant +reserved-byte+ #x00)

(defparameter *debug-socks* t)

(defun-if *debug-socks*
    debug-socks (stream direction raw &rest fields)
  (when *debug-socks*
    (format stream "~&$$ ~C \"~A\" [ "
	    (case direction
	      (:in #\>)
	      (:out #\<))
	    raw)
    (loop for field in fields by #'cddr
	  for value in (cdr fields) by #'cddr
	  do (format stream "~A ~A " field value))
    (format stream "]~%")))

(defconstant +ipv4-address-bits+ 32)
(defconstant +ipv6-address-bits+ 128)

; probably want to memoize this
(defun pack-message (&rest fields)
  (let ((buffer (make-array (expt 2 16) :element-type 'character :fill-pointer 0 :adjustable nil)))
    (flet ((foo (x)
	     (let ((start (fill-pointer buffer)))
	       (incf (fill-pointer buffer) (length x))
	       (replace buffer x :start1 start))))
      (loop for field in fields
	    do (typecase field
		 (integer (vector-push (code-char field) buffer))
		 (complex (foo (base256-string (realpart field) :width (imagpart field))))
		 (string (foo field))
		 (list (foo (apply #'pack-message field)))
		 (t (error "Don't know how to pack object of type ~A into a message." (type-of field))))
	    finally (return buffer)))))

