(in-package socks)

(defun socks4-error-string (stream pretty-error socks-address socks-port destination-address destination-port)
  (format stream "SOCKS4 server ~A:~D reported ~A while attempting to connect to ~A~D."
	  socks-address socks-port pretty-error destination-address destination-port))

(define-condition socks4-error (socks-error)
  ())

(define-condition request-rejected-or-failed (socks-command-error socks4-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks4-error-string stream "Request Rejected or Failed"
		       socks-address socks-port destination-address destination-port)))))

(define-condition identd-not-available (socks-authentication-error socks4-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks4-error-string stream "that your Identd is not available"
				    socks-address socks-port destination-address destination-port)))))

(define-condition identd-userid-mismatch (socks-authentication-error socks4-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks4-error-string stream "that your Identd is giving a different userid than the one supplied in your request"
				    socks-address socks-port destination-address destination-port)))))

