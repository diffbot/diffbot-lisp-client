;;;; package.lisp

(defpackage #:cl-diffbot
  (:use #:cl)
  ;; Conditions
  (:export #:diffbot-condition
           #:diffbot-error
	   #:parameter-error
	   #:diffbot-server-error)
  ;; Main APIs
  (:export #:article-api
	   #:frontpage-api
	   #:product-api
	   #:image-api
	   #:page-classifier-api
	   #:custom-api)
  ;; Bulk APIs
  (:export #:bulk-job-create
	   #:bulk-job-pause
	   #:bulk-job-delete
	   #:bulk-job-retrieve
	   #:bulk-job-view)
  ;; Crawl APIs
  (:export #:crawl-job-create
	   #:crawl-job-pause
	   #:crawl-job-restart
	   #:crawl-job-delete
	   #:crawl-job-retrieve
	   #:crawl-job-view))

