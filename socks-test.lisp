(in-package socks)

(defun test-socks-simple-stream ()
  (let ((sss (make-socket-via-socks :remote-host "www.lisp.org" :Remote-port 80 :socks-address "10.5.5.32" :socks-port 10808 :username "df" :password "df")))
    (write-sequence "GET / HTTP/1.0" sss)
    (terpri sss)
    (terpri sss)
    (force-output sss)
    (read-until-drained sss t)))

(defun test-socks-simple-stream-passthrough ()
  (let ((sss (make-socket-via-socks :remote-host "www.lisp.org" :Remote-port 80 :socks-address nil)))
    (write-sequence "GET / HTTP/1.1

" sss)
    (force-output sss)
    (read-until-drained sss t)))

(defun test-socks-http ()
  (drakma:http-request "http://lisp.org/" :socks-address "10.5.5.32" :socks-port 10808 :socks-username "k" :socks-password "k" :user-agent :firefox))


#|
Example:

This is a socks 4 request to connect Fred to 66.102.7.99:80, the server replies with an "OK."

  Client: 0x04 | 0x01 | 0x00 0x50 | 0x42 0x66 0x07 0x63 | 0x46 0x72 0x65 0x64 0x00
  Server: 0x00 | 0x5a | 0x00 0x50 | 0x42 0x66 0x07 0x63
|#
