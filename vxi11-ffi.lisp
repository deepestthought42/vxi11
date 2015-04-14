;;;; vxi11-ffi.lisp

(in-package #:vxi11)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *lib-location*
    (concatenate 'string
		 (directory-namestring (asdf:system-relative-pathname 'vxi11 ""))
		 "libvxiwrapper.so")))


(cffi:define-foreign-library vxiwrapper
  (:unix (:or #.*lib-location*))
  (t (:default "libvxiwrapper")))


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



(cffi:defcfun ("open_device" open-device)
    :int
  (ip :string)
  (clink (:pointer (:struct clink))))

(cffi:defcfun ("open_device_by_name")
    :int
  (ip :string)
  (clink (:pointer (:struct clink)))
  (device :string))

(cffi:defcfun ("close_device")
    :int
  (ip :string)
  (clink (:pointer (:struct clink))))


(cffi:defcfun ("send_command")
    :int
  (clink (:pointer (:struct clink)))
  (cmd :string))


(cffi:defcfun ("send_data")
    :int
  (clink (:pointer (:struct clink)))
  (cmd :string) (len :unsigned-long))


(cffi:defcfun ("receive")
    :long
  (clink (:pointer (:struct clink)))
  (buffer (:pointer :char))
  (len :unsigned-long)
  (timeout :unsigned-long))


(cffi:defcfun ("send_data_block")
    :int
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (buffer (:pointer :char))
  (len :unsigned-long))


(cffi:defcfun ("receive_data_block")
    :long
  (clink (:pointer (:struct clink)))
  (buffer (:pointer :char))
  (len :unsigned-long)
  (timeout :unsigned-long))


(cffi:defcfun ("send_and_receive")
    :long
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (buf (:pointer :char))
  (buf-len :unsigned-long)
  (timeout :unsigned-long))


(cffi:defcfun ("obtain_long_value")
    :long
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (timeout :unsigned-long))


(cffi:defcfun ("obtain_double_value")
    :double
  (clink (:pointer (:struct clink)))
  (cmd :string)
  (timeout :unsigned-long))

(defconstant +NO-ERROR+ 0)

(defmacro with-open-device ((ip link) &body body)
  `(cffi:with-foreign-object (,link '(:struct clink))
     (if (not (equal (open-device ,ip ,link) +NO-ERROR+))
	 (error "could not open connection to: ~a" ,ip))
     (unwind-protect (progn ,@body)
       (close-device ,ip ,link))))

(defmacro with-open-device-and-checked ((ip link) &body body)
  `(with-open-device (,ip ,link)
     (let ((ret (progn ,@body)))
       (if (not (equal 0 ret))
	   (error "Call to AFG returned: ~a" ret)
	   ret))))
