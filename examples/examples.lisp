;;;; examples.lisp

(defpackage cl-diffbot-examples
  (:use :cl :cl-diffbot)
  (:export #:article-api-example))

(in-package :cl-diffbot-examples)

(defun article-api-example ()
  "Performs a sample article API request.
   Extrats text from http://diffbot.com/"
  (let ((token "89e24f53a99c5b5126332e2b8b3a2759"))
    (handler-case 
	(let ((data (article-api token "http://diffbot.com/")))
	  (format nil "http://diffbot.com/ text: ~a" (cdr (assoc :text data))))
      (diffbot-server-error (err) err)
      (t () "Unexpected error."))))
