;;;; vxi11.lisp

(in-package #:vxi11)

;;; "vxi11" goes here. Hacks and glory await!



(defparameter *default-timeout* 1000)
(defparameter *default-buffer-length* 100)

(defun query-command (command &optional (ip-address "titanfg.triumf.ca")
			      (ret-buf-length 100)
			      (timeout-in-ms *default-timeout*))
  (cffi:with-foreign-objects ((buf :char ret-buf-length))
    (dotimes (i ret-buf-length)
      (setf (cffi:mem-aref buf :char i) 0))
    (with-open-device-and-checked (ip-address link)
      (send-and-receive link command buf ret-buf-length timeout-in-ms))
    (cffi:foreign-string-to-lisp buf)))

(defun query-command-double (command &optional (ip-address "titanfg.triumf.ca")
					       (timeout-in-ms *default-timeout*))
  (with-open-device (ip-address link)
    (obtain-double-value link command timeout-in-ms)))


(defun query-command-long (command &optional (ip-address "titanfg.triumf.ca")
					     (timeout-in-ms *default-timeout*))
  (with-open-device (ip-address link)
    (obtain-long-value link command timeout-in-ms)))



