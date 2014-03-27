(in-package socks)

(define-condition unknown-reply-code (socks-error socks-command-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "an unknown reply code"
				    socks-address socks-port destination-address destination-port)))))

(define-condition invalid-socks-version (socks-client-error)
  ((version
    :initarg :version
    :reader socks-error-version))
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (format stream "The SOCKS library does not understand SOCKS version ~A, specify :v5, :v4, or :v4a."
		       (socks-error-version condition))))))

