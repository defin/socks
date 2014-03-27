(in-package socks)

(excl:def-stream-class socks-simple-stream (excl:socket-simple-stream)
  ((socks-connection
    :accessor socks-simple-stream-socks-connection
    :initarg :socks-connection)))

;'(:remote-host nil :remote-port 80 :socks-address nil :socks-port "23" :username "bar" :password "foo" :version 5)

(defun make-socket-via-socks (&rest keys
			      &key remote-host remote-port socks-address (socks-port *socks-default-port*) username password (version :v5)
			      &allow-other-keys)
  "Accepts all keywords which MAKE-SOCKET accepts and passes them through, using SOCKS-specific keywords to setup the proxy.
If SOCKS-ADDRESS is null, bypass using SOCKS."
  (let* ((keys-not-valid-for-make-socket '(:remote-host :remote-port :socks-address :socks-port :username :password :version))
	 (make-socket-keywords (filter-plist (lambda (x y)
					       (declare (ignore y))
					       (member x keys-not-valid-for-make-socket))
					     keys))
	 ; need to catch errors here and turn them into socks-client-errors of various flavors
	 (socks-stream (if socks-address
			   (apply #'socket::make-socket-stream-internet 'socks-simple-stream
				  :connect :active
				  :remote-host socks-address
				  :remote-port socks-port
				  make-socket-keywords)
			   (apply #'socket:make-socket
				  :remote-host remote-host
				  :remote-port remote-port
				  make-socket-keywords)))
	 (socks-connection (when socks-address (open-socks-connection version socks-stream :username username :password password))))
    (when socks-address
      (setf (socks-simple-stream-socks-connection socks-stream) socks-connection)
      (setf (socks-connection-destination-address socks-connection) remote-host
	    (socks-connection-destination-port socks-connection) remote-port)
      (socks-connect-remote socks-connection remote-host remote-port))
    socks-stream))
