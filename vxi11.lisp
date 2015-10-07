;;;; vxi11.lisp

(in-package #:vxi11)

;;; "vxi11" goes here. Hacks and glory await!



(defparameter *default-timeout* 2000)
(defparameter *default-buffer-length* 100)


(defun convert-to-string (buf len)
  (declare (ignore len))
  (cffi:foreign-string-to-lisp buf))


(defun convert-to-sequence (buf len)
  (iter
    (with ret-array = (make-array `(,len)))
    (for i from 0 below len)
    (setf (aref ret-array i) (cffi:mem-aref buf :char i))
    (finally (return ret-array))))

(defun query-data (command &key (output-conversion #'convert-to-sequence)
				(ip-address "titanfg.triumf.ca")
				(instrument-name "inst0")
				(ret-buf-length 100)
				(timeout-in-ms *default-timeout*))
  (cffi:with-foreign-objects ((buf :char ret-buf-length))
    (dotimes (i ret-buf-length)
      (setf (cffi:mem-aref buf :char i) 0))
    (with-open-device-and-checked (link ip-address instrument-name)
      (vxi11-send-and-receive link command buf ret-buf-length timeout-in-ms))
    (funcall output-conversion buf ret-buf-length)))

(defun query-command (command &key (ip-address "titanfg.triumf.ca")
				   (instrument-name "inst0")
				   (ret-buf-length 100)
				   (timeout-in-ms *default-timeout*))
  (query-data command
	      :output-conversion #'convert-to-string
	      :ip-address ip-address :instrument-name instrument-name
	      :ret-buf-length ret-buf-length :timeout-in-ms timeout-in-ms))



(defun query-command-double (command &optional (ip-address "titanfg.triumf.ca")
					       (timeout-in-ms *default-timeout*))
  (with-open-device (link ip-address)
    (vxi11-obtain-double-value-timeout link command timeout-in-ms)))


(defun query-command-long (command &optional (ip-address "titanfg.triumf.ca")
					     (timeout-in-ms *default-timeout*))
  (with-open-device (link ip-address)
    (vxi11-obtain-long-value-timeout link command timeout-in-ms)))







(defun send-command (command &key
			      (ip-address "titanfg.triumf.ca")
			      (instrument-name "inst0"))
  (with-open-device-and-checked (link ip-address instrument-name)
    (vxi11-send link command (length command))))
