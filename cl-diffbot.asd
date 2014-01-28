;;;; cl-diffbot.asd

(asdf:defsystem #:cl-diffbot
  :serial t
  :description "Official diffbot (http://diffbot.com) bindings."
  :author "Christopher M. van Buren <thegophering@gmail.com>"
  :version "0.0.1"
  :depends-on (#:drakma
	       #:cl-json)
  :components ((:static-file "README")
	       (:module "src"
			:components
			((:file "package")
			 (:file "conditions")
			 (:file "cl-diffbot")
			 (:file "bulk-apis")
			 (:file "crawl-apis")))
	       (:module "examples"
			:components
			((:file "examples")))))
