(in-package socks)

(defmethod open-socks-connection ((version symbol) (host string)
				  &key (port *socks-default-port*))
  (declare (ignore port))
  (error 'invalid-socks-version :version version))

(defmethod-if *debug-socks*
    build-received-string ((connection socks-connection) (new-character t))
  (with-accessors ((builder socks-connection-received-string-builder))
      connection
    (when *debug-socks*
      (build-string builder new-character))))

(defmethod-if *debug-socks*
    get-received-string ((connection socks-connection))
  (with-accessors ((builder socks-connection-received-string-builder))
      connection
    (when *debug-socks*
      (built-string builder))))

(defun-if *debug-socks*
    make-received-string-builder ()
  (make-string-builder))

(defun send-message (socket message)
  (write-sequence message socket)
  (force-output socket))

(defmethod socks-read-octet ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket)
		   (received-string-builder socks-connection-received-string-builder))
      connection
    (let ((octet (read-byte socket)))
      (build-received-string connection octet)
      octet)))

(defmethod socks-read-octets ((connection socks-connection) (count integer))
  (with-accessors ((socket socks-connection-socket)
		   (received-string-builder socks-connection-received-string-builder))
      connection
    (loop with string = (make-array count :element-type 'character)
	  for i from 0 below count
	  for octet = (read-byte socket)
	  do (setf (aref string i) (code-char octet))
	     (build-received-string connection octet)
	  finally (return string))))

(defmethod socks-connect-remote ((connection socks-connection) (host t) (port integer))
  (send-request-message connection :connect host port)
  (multiple-value-bind (remote-host remote-port)
      (read-reply-message connection)
    (with-accessors ((state socks-connection-state)
		     (outbound-address socks-connection-outbound-address)
		     (outbound-port socks-connection-outbound-port))
	connection
      (setf outbound-address remote-host
	    outbound-port remote-port)))
`  connection)

(defmethod open-socks-connection ((version t) (host string) &key (port *socks-default-port*) username password &allow-other-keys)
  (let ((socket (socket:make-socket :remote-host host :remote-port port)))
    (open-socks-connection version socket :username username :password password)))

(defmethod read-version ((connection socks-connection))
  (with-accessors ((version-code version-code)
		   (socket socks-connection-socket))
      connection
    (let ((octet (socks-read-octet connection)))
      octet)))

(defmethod read-ipv4-network-order-address ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (socket:ipaddr-to-dotted (base256-integer (socks-read-octets connection 4)))))

(defmethod read-domain-name ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (let ((length (socks-read-octet connection)))
      (socks-read-octets connection length))))

(defmethod read-ipv6-network-order-address ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (socks-read-octets connection 16)))

(defmethod read-network-order-port-number ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (base256-integer (socks-read-octets connection 2))))

(defmethod read-reserved ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (let ((octet (socks-read-octet connection)))
      (when (and octet (= octet +reserved-byte+))
	octet))))

(defmethod read-remote-output-until-eof ((connection socks-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (read-until-drained socket t)))




