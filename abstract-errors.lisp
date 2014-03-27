(in-package socks)

(define-condition socks-error (excl:socket-error)
  ((connection
    :initarg :connection
    :reader socks-error-connection)))

(define-condition socks-command-error (socks-error)
  ())

(define-condition socks-request-remote-error (socks-command-error)
  ())

(define-condition socks-connection-error (socks-error)
  ())

(define-condition socks-authentication-error (socks-error)
  ())

(define-condition socks-client-error (socks-error)
  ())

(define-condition unknown-command-error (socks-command-error)
  ())
