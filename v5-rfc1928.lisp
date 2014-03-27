(in-package socks)

(define-paired-tables-from-alist
    code method
  ((#x00 . :No-Authentication-Required)
   (#x01 . :GSSAPI)
   (#x02 . :Username-Password)
   ;; http://www.iana.org/assignments/socks-methods
   (#x03 . :Challenge-Handshake-Authentication-Protocol)
   (#x05 . :Challenge-Response-Authentication-Method)
   (#x06 . :Secure-Sockets-Layer)
   (#x07 . :NDS-Authentication)
   (#x08 . :Multi-Authentication-Framework)
   ;; 09-7f IANA to assign
   ;; 80-fe private methods
   (#xff . :no-acceptable-methods)))

(define-paired-tables-from-alist
    code address-type
  ((#x01 . :ipv4-address)
   (#x03 . :domain-name)
   ;; the address field contains a fully-qualified domain name.  The first
   ;; octet of the address field contains the number of octets of name that
   ;;   follow, there is no terminating NUL octet.
   (#x04 . :ipv6-address)))

(define-paired-tables-from-alist
    code v5-command
  ((#x01 . :connect)
   (#x02 . :bind)
   (#x03 . :udp-associate)))

(define-paired-tables-from-alist
    code v5-reply
  ((#x00 . :succeeded)
   (#x01 . :general-socks-server-failure)
   (#x02 . :connection-not-allowed-by-ruleset)
   (#x03 . :network-unreachable)
   (#x04 . :host-unreachable)
   (#x05 . :connection-refused)
   (#x06 . :ttl-expired)
   (#x07 . :command-not-supported)
   (#x08 . :address-type-not-supported)))

(defconstant +v5-byte+ #x05)

(defstruct (socks5-connection (:include socks-connection)) supported-methods selected-method)

(defmethod version-code ((connection socks5-connection))
  +v5-byte+)

(defmethod open-socks-connection ((version (eql :v5)) (socket excl:socket-simple-stream) &key username password &allow-other-keys)
  (let ((connection (make-socks5-connection
		     :socket socket
		     :supported-methods '(:no-authentication-required
					  :username-password)
		     :state :open)))
    (case (select-authentication-method connection)
      (:username-password
       (when (eq (authenticate connection :username username :password password) :authentication-failed)
	 connection))
      (:no-authentication-required connection)
					;      (:gssapi)
      )))

(defmethod select-authentication-method ((connection socks5-connection))
  (send-method-selection-message connection)
  (read-selected-method connection))

(defmethod send-method-selection-message ((connection socks5-connection))
  (with-accessors ((version-code version-code)
		   (socket socks5-connection-socket)
		   (supported-methods socks5-connection-supported-methods)
		   (state socks5-connection-state))
      connection
    (let ((message (pack-message version-code
				 (length supported-methods)
				 (mapcar #'method->code supported-methods))))
      (send-message socket message)
      (debug-socks t :out message 'ver version-code 'nmethods (length supported-methods) 'methods supported-methods))
    (setf state :selecting-method)))

(defmethod read-selected-method ((connection socks5-connection))
  (with-accessors ((socket socks-connection-socket)
		   (state socks-connection-state)
		   (supported-methods socks5-connection-supported-methods)
		   (selected-method socks5-connection-selected-method))
      connection
    (let ((version (read-version connection))
	  (method (code->method (socks-read-octet connection))))
      (debug-socks t :in (get-received-string connection) 'ver version 'method method)
      (setf state :authentication-method-selected)
      (setf selected-method method)
      method)))

(defmethod send-request-message ((connection socks5-connection)
				 (command symbol)
				 (destination-address string)
				 (destination-port integer))
  (with-accessors ((version-code version-code)
		   (socket socks-connection-socket)
		   (state socks-connection-state))
      connection
    (let* ((address-type (typecase destination-address
			   (integer (case (integer-length destination-address)
				      (+ipv4-address-bits+ :ipv4-address)
				      (+ipv6-address-bits+ :ipv6-address)))
			   (string :domain-name)))
	   (destination-address (typecase destination-address
				  (integer destination-address)
				  (string (or (ignore-errors (socket:dotted-to-ipaddr destination-address))
					      destination-address))))
	   (message (pack-message version-code
				  (v5-command->code command)
				  +reserved-byte+
				  (address-type->code address-type)
				  (length destination-address)
				  destination-address
				  (fix-integer-width destination-port 2))))
	(send-message socket message)
	(setf state :requesting-remote-connection)
	(debug-socks t :out message 'ver version-code 'cmd command 'rsv +reserved-byte+
		     'atyp address-type 'dst.addr destination-address 'dst.port destination-port))))

(defmethod read-address-type ((connection socks5-connection))
  (with-accessors ((socket socks-connection-socket))
      connection
    (code->address-type (socks-read-octet connection))))

(defmethod read-reply-message ((connection socks5-connection))
  (with-accessors ((version-code version-code)
		   (socket socks-connection-socket)
		   (state socks-connection-state))
      connection
    (let* ((version (read-version connection))
	   (reply (code->v5-reply (socks-read-octet connection) :unknown-reply-code))
	   (reserved (read-reserved connection))
	   (address-type (read-address-type connection))
	   (address (case address-type
		      (:ipv4-address (read-ipv4-network-order-address connection))
		      (:domain-name (read-domain-name connection))
		      (:ipv6-address (read-ipv6-network-order-address connection))))
	   (port (read-network-order-port-number connection)))
      (debug-socks t :in (get-received-string connection) 'ver version 'rep reply 'rsv reserved 'atyp address-type 'bind.addr address 'bind.port port)
      (case reply
	(:succeeded 
	 (setf state :remote-connection-ready)
	 (values socket connection port))
	(:general-socks-server-failure (error 'general-socks-server-failure :connection connection))
	(:connection-not-allowed-by-ruleset (error 'connection-not-allowed-by-ruleset :connection connection))
	(:network-unreachable (error 'reqeust-network-unreachable :connection connection))
	(:host-unreachable (error 'request-host-unreachable :connection connection))
	(:connection-refused (error 'request-connection-refused :connection connection))
	(:ttl-expired (error 'request-ttl-expired :connection connection))
	(:command-not-supported (error 'command-not-supported :connection connection))
	(:address-type-not-supported (error 'address-type-not-supported :connection connection))
	(:unknown-reply-code (error 'unknown-reply-code :connection connection))))))
