(in-package socks)

(defun socks5-error-string (stream pretty-error socks-address socks-port destination-address destination-port)
  (format stream "SOCKS5 server ~A:~D reported ~A while attempting to connect to ~A~D."
	  socks-address socks-port pretty-error destination-address destination-port))

(define-condition socks5-error (socks-error)
  ())

(define-condition general-socks-server-failure (socks5-error socks-command-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "General Server Failure"
				    socks-address socks-port destination-address destination-port)))))

(define-condition connection-not-allowed-by-ruleset (socks5-error socks-command-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Connection Not Allowed by Ruleset"
				    socks-address socks-port destination-address destination-port)))))

(define-condition reqeust-network-unreachable (socks5-error socks-request-remote-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Request Network Unreachable"
				    socks-address socks-port destination-address destination-port)))))

(define-condition request-host-unreachable (socks5-error socks-request-remote-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Request Host Unreachable"
				    socks-address socks-port destination-address destination-port)))))

(define-condition request-connection-refused (socks5-error socks-request-remote-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Request Connection Refused"
				    socks-address socks-port destination-address destination-port)))))

(define-condition request-ttl-expired (socks5-error socks-request-remote-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Request TTL Expired"
				    socks-address socks-port destination-address destination-port)))))

(define-condition command-not-supported (socks5-error unknown-command-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Command Not Supported"
				    socks-address socks-port destination-address destination-port)))))

(define-condition address-type-not-supported (socks5-error socks-command-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Address Type Not Supported"
				    socks-address socks-port destination-address destination-port)))))

(define-condition socks5-authentication-failed (socks-authentication-error socks-v5-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (socks-address socks-port destination-address destination-port)
		 (socks-error-connection condition)
	       (socks5-error-string stream "Authentication Failed"
				    socks-address socks-port destination-address destination-port)))))
