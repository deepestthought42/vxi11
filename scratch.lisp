(in-package #:vxi11)



(defun test ()
  (let* ((sem (bt-sem:make-semaphore))
	 (thread (bt:make-thread
		 #'(lambda ()
		     (query-command "*IDN?" :ip-address "titangpib1.triumf.ca" :instrument-name "gpib0,8")
		     (bt-sem:signal-semaphore sem)))))
    (if (not (bt-sem:wait-on-semaphore sem :timeout 2))
	(progn
	  (bt:destroy-thread thread)
	  (format t "Killed thread")))))


(test)

(time
 (utils:with-destroyed-after (1 :name "test")
   (values 1 2)))












