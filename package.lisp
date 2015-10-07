;;;; package.lisp

(defpackage #:vxi11
  (:use #:cl #:iterate)
  (:export
   #:query-command
   #:query-command-double
   #:query-command-long
   #:send-command
   #:send-data
   #:send-data-block
   #:with-open-device
   #:with-open-device-and-checked
   #:query-data-block
   #:convert-to-sequence
   #:convert-to-string
   #:query-data))

