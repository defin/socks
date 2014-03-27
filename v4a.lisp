(in-package socks)

(defstruct (socks4a-connection (:include socks-v4-connection)))

(defmethod open-socks-connection ((version (eql :v4a)) (socket excl:socket-simple-stream) &key &allow-other-keys)
  (let ((connection (make-socks4a-connection
		     :socket socket
		     :state :open)))
    connection))

(defmethod pack-request-message ((connection socks4a-connection) command-code destination-port destination-address userid)
  (with-accessors ((version-code version-code))
      connection
    (let ((message (pack-message version-code command-code
				 (fix-integer-width destination-port 2)
				 (fix-integer-width (socket::dotted-to-ipaddr "0.0.0.255") 4)
				 userid (string #\Null)
				 destination-address (string #\Null))))
      (debug-socks t :out message 'vn version-code 'cd (code->command command-code)
		   'dstport destination-port 'dstip destination-address 'userid userid)
      message)))

