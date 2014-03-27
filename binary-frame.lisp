define-binary-frame
  (version :width 1 :reader (read-byte stream) :writer (write-byte 5 stream))
  (command :width 1 :reader )
  (foo :width (byte 2 3) :reader :writer)
