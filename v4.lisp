(in-package socks)

(define-paired-tables-from-alist
    code v4-command
  ((#x01 . :connect)
   (#x02 . :bind)))

(define-paired-tables-from-alist
    code v4-reply
  (;; this series reported by wikipedia
   (#x5a . :request-granted)
   (#x5b . :request-rejected-or-failed)
   (#x5c . :request-failed-identd-not-available)
   (#x5d . :request-failed-identd-userid-mismatch)
   ;; this series defined by SOCKS4 spec
   (#x90 . :request-granted)
   (#x91 . :request-rejected-or-failed)
   (#x92 . :request-failed-identd-not-available)
   (#x93 . :request-failed-identd-userid-mismatch))
  :default :unknown-reply-code)

(defconstant +v4-byte+ #x04)

(defstruct (socks4-connection (:include socks-connection)) userid)

(defmethod version-code ((connection socks4-connection))
  +v4-byte+)

(defmethod open-socks-connection ((version (eql :v4)) (socket excl:socket-simple-stream) &key &allow-other-keys)
  (let ((connection (make-socks4-connection
		     :socket socket
		     :state :open)))
    connection))

(defmethod send-request-message ((connection socks4-connection)
				 (command symbol)
				 (destination-address string)
				 (destination-port integer))
  (with-accessors ((version-code version-code)
		   (socket socks-connection-socket)
		   (state socks-connection-state)
		   (userid socks4-connection-userid))
      connection
    (let* ((destination-address (typecase destination-address
				  (integer destination-address)
				  (string (or (ignore-errors (socket:dotted-to-ipaddr destination-address))
					      destination-address))))
	   (message (pack-request-message connection
					  (v4-command->code command)
					  destination-port
					  destination-address
					  userid)))
      (send-message socket message)
      (setf state :requesting-remote-connection))))

(defmethod pack-request-message ((connection socks4-connection) command-code destination-port destination-address userid)
  (with-accessors ((version-code version-code))
      connection
    (let ((message (pack-message version-code command-code
				 (fix-integer-width destination-port 2)
				 (fix-integer-width destination-address 4)
				 userid (string #\Null))))
      (debug-socks t :out message 'vn version-code 'cd (code->command command-code)
		   'dstport destination-port 'dstip destination-address 'userid userid)
      message)))

(defmethod read-reply-message ((connection socks4-connection))
  (with-accessors ((version-code version-code)
		   (socket socks-connection-socket)
		   (state socks-connection-state))
      connection
    (let* ((version (read-version connection)) ; supposed to be 0 according to SOCKS4 spec!
	   (reply (code->v4-reply (socks-read-octet connection) :unknown-reply-code))
	   (port (read-network-order-port-number connection))
	   (address (read-ipv4-network-order-address connection)))
      (debug-socks t :in (get-received-string connection) 'vn version 'cd reply 'dstport port 'dstip address)
      (case reply
	(:request-granted
	 (setf state :remote-connection-ready)
	 (values socket connection))
	(:request-rejected-or-failed (error 'request-rejected-or-failed :connection connection))
	(:request-failed-identd-not-available (error 'identd-not-available :connection connection))
	(:request-failed-identd-userid-mismatch (error 'identd-userid-mismatch :connection connection))
	(:unknown-reply-code (error 'unknown-reply-code :connection connection))))))


