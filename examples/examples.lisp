;;;; examples.lisp

(defpackage cl-diffbot-examples
  (:use :cl :cl-diffbot)
  (:export #:article-api-example))

(in-package :cl-diffbot-examples)

(defun article-api-example ()
  "Performs a sample article API request.
   Extrats text from http://diffbot.com/"
  (let ((token "DIFFBOT_TOKEN"))
    (handler-case 
	(let ((data (article-api token "http://diffbot.com/")))
	  (format nil "http://diffbot.com/ text: ~a" (cdr (assoc :text data))))
      (diffbot-server-error (err) err)
      (t () "Unexpected error."))))
