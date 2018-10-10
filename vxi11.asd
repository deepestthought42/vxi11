;;;; vxi11.asd

(asdf:defsystem #:vxi11
  :serial t
  :description "Lisp wrapper for vxi11 c library"
  :author "Renee Klawitter <klawitterrenee@gmail.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:iterate
               #:cffi
               #:binary-types
	       #:deploy)
  :components ((:file "package")
	       (:file "vxi11-ffi")
               (:file "vxi11")
	       (:file "deploy")))

