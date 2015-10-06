;;;; vxi11.lisp

(in-package #:vxi11)

;;; "vxi11" goes here. Hacks and glory await!



(defparameter *default-timeout* 1000)
(defparameter *default-buffer-length* 100)

(defun query-command (command &key
			      (ip-address "titanfg.triumf.ca")
			      (instrument-name "inst0")
			      (ret-buf-length 100)
			      (timeout-in-ms *default-timeout*))
  (cffi:with-foreign-objects ((buf :char ret-buf-length))
    (dotimes (i ret-buf-length)
      (setf (cffi:mem-aref buf :char i) 0))
    (with-open-device-and-checked (ip-address link instrument-name)
      (vxi11-send-and-receive link command buf ret-buf-length timeout-in-ms))
    (cffi:foreign-string-to-lisp buf)))

(defun query-command-double (command &optional (ip-address "titanfg.triumf.ca")
					       (timeout-in-ms *default-timeout*))
  (with-open-device (ip-address link)
    (vxi11-obtain-double-value-timeout link command timeout-in-ms)))


(defun query-command-long (command &optional (ip-address "titanfg.triumf.ca")
					     (timeout-in-ms *default-timeout*))
  (with-open-device (ip-address link)
    (vxi11-obtain-long-value-timeout link command timeout-in-ms)))



