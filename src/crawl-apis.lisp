;;;; crawl-apis.lisp

(defun crawl-job-create (token name seeds api-url url-crawl-pattern url-crawl-regex
			 url-process-pattern url-process-regex page-process-pattern
			 &key (restrict-domain t) (only-process-if-new t) max-to-crawl max-to-process 
			   notify-email notify-webhook crawl-delay repeat  max-rounds)
  "The Crawlbot API allows you to programmatically manage Crawlbot crawls and retrieve output.

   token - your diffbot API token.
   name - job name. This should be a unique identifier and can be used 
          to modify your crawl or retrieve its output.
   seeds - list of seed URL(s) see: http://www.diffbot.com/dev/docs/crawl/
   api-url - full Diffbot API URL through which to process pages. 
             E.g., &apiUrl=http://api.diffbot.com/v2/article to process matching 
             links via the Article API. 
             The Diffbot API URL can include querystring parameters to 
             tailor the output. 
             For example, &apiUrl=http://api.diffbot.com/v2/product?fields=querystring,meta 
             will process matching links using the Product API, 
             and also return the querystring and meta fields. 
             see: http://www.diffbot.com/dev/docs/crawl/
   url-crawl-pattern - specify list of strings to limit pages crawled 
                       to those whose URLs contain any of the content strings. 
                       You can use the exclamation point to specify a negative string, e.g. 
                       !product to exclude URLs containing the string \"product.\"
   url-crawl-regex - specify a regular expression to limit pages crawled to 
                     those URLs that match your expression. 
                     This will override any url-crawl-pattern value.
   url-process-pattern - specify list of strings to limit pages processed to 
                         those whose URLs contain any of the content strings. 
                         You can use the exclamation point to specify a negative string, 
                         e.g. !/category to exclude URLs containing the string \"/category.\"
   url-process-regex - specify a regular expression to limit pages processed to 
                       those URLs that match your expression. 
                       This will override any url-process-pattern value.
   page-process-pattern - specify list of strings to limit pages processed to 
                          those whose HTML contains any of the content strings.
   max-to-crawl	- specify max pages to spider.
   max-to-process - specify max pages to process through Diffbot APIs. Default: 10,000.
   restrict-domain - by default crawls will restrict to subdomains within the seed URL domain. 
                     Set to nil to follow all links regardless of domain.
   notify-email - send a message to this email address when the bulk job completes.
   notify-webhook - pass a URL to be notified when the bulk job completes. 
                    You will receive a POST with the full JSON response in the POST body.
   crawl-delay - wait this many seconds between each URL crawled from a single IP address. 
                 Specify the number of seconds as an integer or floating-point number 
                 (e.g., :crawl-delay 0.25).
   repeat - specify the number of days as a floating-point (e.g. :repeat 7.0) to 
            repeat this job. By default bulk jobs will not be repeated.   
   only-process-if-new - by default repeat crawls will only process new (previously unprocessed) pages. 
                         Set to nil (:only-process-if-new nil) to process all content on repeat crawls.
   max-rounds - specify the maximum number of repeats. Use :max-rounds -1 to continually repeat."
  (let ((req-url (generate-endpoint "/bulk"))
	(body `(("token" . ,token)
		("name" . ,name)
		("seeds" . ,seeds)
		("apiUrl" . ,api-url)
		("urlCrawlPattern" . ,(format nil "~{~a~^||~}" url-crawl-pattern))
		("urlCrawlRegEx" . ,url-crawl-regex)
		("urlProcessPattern" . ,(format nil "~{~a~^||~}" url-process-pattern))
		("urlProcessRegEx" . ,url-process-regex)
		("pageProcessPattern" . ,(format nil "~{~a~^||~}" page-process-pattern)))))
    (when max-to-crawl (setf body (append body `(("maxToCrawl" . ,max-to-crawl)))))
    (when max-to-process (setf body (append body `(("maxToProcess" . ,max-to-process)))))
    (unless restrict-domain (setf body (append body `(("restrictDomain" . 0)))))
    (when notify-email (setf body (append body `(("notifyEmail" . ,notify-email)))))
    (when notify-webhook (setf body (append body `(("notifyWebHook" . ,notify-webhook)))))
    (when crawl-delay (setf body (append body `(("crawlDelay" . ,crawl-delay)))))
    (when repeat (setf body (append body `(("repeat" . ,repeat)))))
    (unless only-process-if-new (setf body (append body `(("onlyProcessIfNew" . 0)))))
    (when max-rounds (setf body (append body `(("maxRounds" . ,max-rounds)))))
    (let ((stream (drakma:http-request req-url :method :post :parameters body :want-stream t)))
      (with-decoded (decoded) stream
	(parse-and-raise-errors decoded)
	decoded))))

(defun crawl-job-pause (token name &key resume)
  "Pauses (or resumes) a previously created crawl job defined by name.

   token - your diffbot API token.
   name - name of the previously created crawl job.
   resume - set to nil to resume a job instead."
  (crawl-job-manage token name :pause (not resume)))

(defun crawl-job-delete (token name)
  "Deletes a previously created crawl job defined by name.

   token - your diffbot API token.
   name - name of the previously created crawl job."
  (crawl-job-manage token name :delete t))

(defun crawl-job-restart (token name)
  "Restarts a previously created crawl job defined by name.

   token - your diffbot API token.
   name - name of the previously created crawl job."
  (crawl-job-manage token name :restart t))

(defun crawl-job-download (token name)
  "Retrieves results of your completed crawl jobs.
   
   token - your diffbot API token.
   name - name of the previously created crawl job.

   Returns JSON formatted alist containing the data."
  (let* ((req-url (generate-endpoint (format nil "/crawl/download/~a-~a_data.json" 
					     token name)))
	 (stream (drakma:http-request req-url :method :get :want-stream t)))
    (with-decoded (decoded) stream
      (parse-and-raise-errors decoded)
      decoded)))

(defun crawl-job-manage (token name &key pause restart delete)
  "General bulk job management function."
  (let* ((req-url (generate-req-url "/crawl" `((token . ,token)
					       (name . ,name)
					       (pause . (if ,pause 0 1))
					       (restart . (if ,restart 0 1))
					       (delete . (if ,delete 0 1)))))
	 (stream (drakma:http-request req-url :method :get :want-stream t)))
    (with-decoded (decoded) stream
      (parse-and-raise-errors decoded)
      decoded)))

(defun crawl-job-view (token &key name)
  "View all active crawl job data.
   
   token - your diffbot API token.
   name - limit retrieval to a single job indicated by name.

   Returns JSON formatted alist containing the data."
  (get-request "/crawl" `((token . ,token)
			  (name . ,name))))
