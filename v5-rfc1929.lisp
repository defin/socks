(in-package socks)

(define-paired-tables-from-alist
    code username-password-status
  ((#x00 . :success))
  :default :failure)

(defmethod authenticate ((connection socks5-connection) &key username password &allow-other-keys)
  (with-accessors ((state socks5-connection-state))
      connection
    (send-username-password-request connection username password)
    (receive-username-password-status connection)))

(defmethod send-username-password-request ((connection socks5-connection)
					   (username string)
					   (password string))
  (with-accessors ((version-code version-code)
		   (socket socks5-connection-socket)
		   (supported-methods socks5-connection-supported-methods)
		   (state socks5-connection-state))
      connection
    (let* ((username-length (length username))
	   (password-length (length password))
	   (message (pack-message version-code username-length username password-length password)))
      (send-message socket message)
      (debug-socks t :out message 'ver version-code 'ulen username-length 'uname username
		   'plen password-length 'passwd password)
      (setf state :sent-authentication-request))))

(defmethod read-username-password-status-code ((connection socks5-connection))
  (with-accessors ((socket socks5-connection-socket))
      connection
    (code->username-password-status (socks-read-octet connection))))

(defmethod receive-username-password-status ((connection socks5-connection))
  (with-accessors ((socket socks5-connection-socket)
		   (supported-methods socks5-connection-supported-methods)
		   (state socks5-connection-state))
      connection
    (let ((version (read-version connection))
	  (status (read-username-password-status-code connection)))
      (debug-socks t :in (get-received-string connection) 'ver version 'status status)
      (if (eq status :success)
	  (setf state :authenticated)
	  (progn (setf state :authentication-failed)
		 (error 'socks5-authentication-failed :connection connection)))
      state)))


