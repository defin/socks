(in-package socks)

(defparameter *socks-default-port* 1080)

(defstruct (socks-connection)
  socket (state :uninitialized) (keepalive t)
  socks-address socks-port
  destination-address destination-port
  outbound-address outbound-port
  (time-opened 0) (time-closed 0)
  (received-string-builder (make-received-string-builder)))

(defmethod version-code ((connection socks-connection))
  nil)

(defmethod open-socks-connection :around ((version symbol) (host string)
					  &key (port *socks-default-port*) &allow-other-keys)
  (let ((connection (call-next-method)))
    (setf (socks-connection-time-opened connection) (get-universal-time)
	  (socks-connection-socks-address connection) host
	  (socks-connection-socks-port connection) port)
    connection))

(pushnew :socks *features*)
