cl-diffbot exposes the diffbot (http://diffbot.com) API to common lisp. cl-diffbot has been tested on 
linux runnin sbcl but should work normally on most other platforms as well as with 
most other common lisp implementations.

##DEPENDENCIES

quicklisp (http://quicklisp.org) [for easy package management]
drakma http client (http://weitz.de/drakma/)
cl-json (http://common-lisp.net/projects/cl-json/)

##INSTALLING

Assuming quicklisp is installed in its default location (~/quicklisp/) and packages are kept in 
(~/quicklisp/local-projects/) copy the folder to ~/quicklisp/local-projects/ and load 
via quicklisp at the REPL:
	 
	 (ql:quickload :cl-diffbot)

Follow default asdf package install procedures if you are not using quicklisp.

##EXAMPLES

Simple usage examples can be found at /examples/examples.lisp. Run:
       
       (cl-diffbot-examples:article-api-example)

at the REPL to test out the article API (a valid token is required).

##DOCUMENTATION

cl-diffbot exposes the following symbols:

Conditions
  diffbot-condition
  diffbot-error
  parameter-error
  diffbot-server-error

Main APIs
  article-api
  frontpage-api
  product-api
  image-api
  page-classifier-api
  custom-api

Bulk APIs
  bulk-job-create
  bulk-job-pause
  bulk-job-delete
  bulk-job-retrieve
  bulk-job-view

Crawl APIs
  crawl-job-create
  crawl-job-pause
  crawl-job-restart
  crawl-job-delete
  crawl-job-retrieve
  crawl-job-view

Use your IDE's default lookup functions to obtain documentation. 
Emacs + SLIME: C-c C-d C-d cl-diffbot: TAB

-Initial commit by Cristopher VB-
