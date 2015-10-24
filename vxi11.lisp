;;;; vxi11.lisp

(in-package #:vxi11)

;;; "vxi11" goes here. Hacks and glory await!



(defparameter *default-timeout* 2000)
(defparameter *default-buffer-length* 100)


(defparameter *mock* nil)
(defparameter *mock-default-val* "")

(defun find-mock-return (command &optional (default *mock-default-val*))
  (log:info "Returning mock value for command: ~a" command)
  (alexandria:if-let (ret (assoc command *mock* :test #'equal))
    (cdr ret)
    default))







(defun convert-to-string (buf len)
  (declare (ignore len))
  (cffi:foreign-string-to-lisp buf))



(defun convert-to-sequence (buf len)
  (iter
    (with ret-array = (make-array `(,len)))
    (for i from 0 below len)
    (setf (aref ret-array i) (cffi:mem-aref buf :unsigned-char i))
    (finally (return ret-array))))

(defun query-data (command &key (output-conversion #'convert-to-sequence)
				(ip-address "titanfg.triumf.ca")
				(instrument-name "inst0")
				(ret-buf-length 100)
				(timeout-in-ms *default-timeout*))
  (log:debug "send and receive, command ~a" command)
  (if *mock* (return-from query-data (find-mock-return command)))
  
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
  (log:debug "obtain double value, command ~a" command)
  
  (if *mock* (return-from query-command-double
	       (find-mock-return command 0d0)))
  
  (with-open-device (link ip-address)
    (vxi11-obtain-double-value-timeout link command timeout-in-ms)))


(defun query-command-long (command &optional (ip-address "titanfg.triumf.ca")
					     (timeout-in-ms *default-timeout*))
  (log:debug "obtain long value, command: ~a" command)

  (if *mock* (return-from query-command-long
	       (find-mock-return command 0)))
  
  (with-open-device (link ip-address)
    (vxi11-obtain-long-value-timeout link command timeout-in-ms)))



(defun send-command (command &key
			     (ip-address "titanfg.triumf.ca")
			     (instrument-name "inst0"))
  (log:debug "sending command ~a" command)

  (if *mock* (return-from send-command
	       (find-mock-return command 0)))
  
  (with-open-device-and-checked (link ip-address instrument-name)
    (vxi11-send link command (length command))))
