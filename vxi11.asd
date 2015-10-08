;;;; vxi11.asd

(asdf:defsystem #:vxi11
  :serial t
  :description "Describe vxi11 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:iterate
               #:cffi
               #:binary-types
	       #:log4cl)
  :components ((:file "package")
	       (:file "vxi11-ffi")
               (:file "vxi11")))

