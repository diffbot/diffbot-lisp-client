;;;; conditions.lisp

(in-package #:cl-diffbot)

(define-condition diffbot-condition (condition)
  ()
  (:documentation "Diffbot conditions superclass."))

(define-condition diffbot-error (diffbot-condition error)
  ()
  (:documentation "Diffbot errors superclass."))

(define-condition parameter-error (diffbot-error simple-condition)
  ()
  (:documentation "Signalled when function is called with malformed parameters."))

(defun parameter-error (format-control &rest format-arguments)
  "Signals an error of type PARAMETER-ERROR with the provided
   format control and arguments."
  (error 'parameter-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition diffbot-server-error (diffbot-error simple-condition)
  ((status-code :initarg :status-code :accessor error-status-code :initform nil)
   (message :initarg :message :accessor error-message :initform nil))
  (:documentation "Error response from the diffbot server."))
