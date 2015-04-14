;;;; package.lisp

(defpackage #:vxi11
  (:use #:cl)
  (:export
   #:query-command
   #:query-command-double
   #:query-command-long
   #:send-command
   #:send-data
   #:send-data-block
   #:with-open-device
   #:with-open-device-and-checked))

