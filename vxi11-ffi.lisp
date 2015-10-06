;;;; vxi11-ffi.lisp

(in-package #:vxi11)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *lib-location*
    (asdf:system-relative-pathname 'vxi11 "vxi11/library/libvxi11.so.1")))


(cffi:define-foreign-library vxiwrapper
  (:unix (:or #.*lib-location*))
  (t (:default "libvxi11.so.1")))


(cffi:use-foreign-library vxiwrapper)

(cffi:defcstruct create-link-resp
  (device-error-code :long)
  (device-link :long)
  (abort-port :unsigned-short)
  (max-recv-size :unsigned-long))

(cffi:defctype vxi11-link (:pointer (:struct create-link-resp)))
(cffi:defctype vxi11-client (:pointer :void))

(cffi:defcstruct clink
  (client vxi11-client)
  (link vxi11-link))



(cffi:defcfun ("vxi11_open_device")
    :int
  (clink (:pointer))
  (ip :string)
  (device :string))

(cffi:defcfun ("vxi11_close_device")
    :int
  (clink (:pointer (:struct clink)))
  (ip :string))


(cffi:defcfun ("vxi11_send_command")
    :int
  (clink (:pointer (:struct clink)))
  (cmd :string))


(cffi:defcfun ("vxi11_send_data")
    :int
  (clink (:pointer (:struct clink)))
  (cmd :string) (len :unsigned-long))


(cffi:defcfun ("vxi11_receive_timeout")
    :long
  (clink (:pointer (:struct clink)))
  (buffer (:pointer :char))
  (len :unsigned-long)
  (timeout :unsigned-long))


(cffi:defcfun ("vxi11_send_data_block")
    :int
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (buffer (:pointer :char))
  (len :unsigned-long))


(cffi:defcfun ("vxi11_receive_data_block")
    :long
  (clink (:pointer (:struct clink)))
  (buffer (:pointer :char))
  (len :unsigned-long)
  (timeout :unsigned-long))


(cffi:defcfun ("vxi11_send_and_receive")
    :long
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (buf (:pointer :char))
  (buf-len :unsigned-long)
  (timeout :unsigned-long))


(cffi:defcfun ("vxi11_obtain_long_value_timeout")
    :long
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (timeout :unsigned-long))


(cffi:defcfun ("vxi11_obtain_double_value_timeout")
    :double
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (timeout :unsigned-long))

(defconstant +NO-ERROR+ 0)




(defmacro with-open-device ((link ip &optional (instrument-name "inst0")) &body body)
  (alexandria:with-gensyms (err-value link-pointer)
    `(cffi:with-foreign-object (,link-pointer :pointer)
       (let ((,err-value (vxi11-open-device ,link-pointer ,ip ,instrument-name)))
	 (if (not (equal ,err-value +NO-ERROR+))
	     (error "could not open connection to: ~a; error: ~a" ,ip ,err-value)))
       (let ((,link (cffi:mem-ref ,link-pointer :pointer)))
	 (unwind-protect (progn ,@body)
	   (let ((,err-value (vxi11-close-device ,link ,ip)))
	     (if (not (equal ,err-value +NO-ERROR+))
		 (error "could not close connection to: ~a; error: ~a" ,ip ,err-value))))))))

(defmacro with-open-device-and-checked ((link ip &optional (instrument-name "inst0")) &body body)
  `(with-open-device (,link ,ip ,instrument-name)
     (let ((ret (progn ,@body)))
       (if (not (equal 0 ret))
	   (error "Call to AFG returned: ~a" ret)
	   ret))))
