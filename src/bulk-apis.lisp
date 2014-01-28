;;;; bulk-apis.lisp

(defun bulk-job-create (token name urls api-url 
			&key notify-email notify-webhook repeat max-rounds page-process-pattern)
  "The Bulk API is built atop Crawlbot, and allows you to submit 
   a large number of URLs for asynchronous processing by a Diffbot API.

   token - your diffbot API token.
   name - Job name. This should be a unique identifier 
          and will be used to modify your bulk job and retrieve its output.
   urls - list of URLs to process.
   api-url - the full Diffbot API to be used for each URL. 
             For instance, to process each URL via the article API, 
             supply http://api.diffbot.com/v2/article. 
             You may also include API parameters, 
             e.g. http://api.diffbot.com/v2/article?fields=meta,tags.
   notify-email - send a message to this email address when the bulk job completes.
   notify-webhook - pass a URL to be notified when the bulk job completes. 
                    You will receive a POST with the full JSON response in the POST body.
   repeat - specify the number of days as a floating-point (e.g. :repeat 7.0) to 
            repeat this job. By default bulk jobs will not be repeated.
   max-rounds - specify the maximum number of repeats. Use :max-rounds -1 to continually repeat.
   page-process-pattern - list of patterns to limit pages processed to those 
                          whose HTML contains any of the content strings. 
                          If a page does not contain at least one of the strings, it will be ignored."
  (let ((req-url (generate-endpoint "/bulk"))
	(body `(("token" . ,token)
		("name" . ,name)
		("urls" . ,(format nil "~{~a ~}" urls))
		("apiUrl" . ,api-url))))
    (when notify-email (setf body (append body `(("notifyEmail" . ,notify-email)))))
    (when notify-webhook (setf body (append body `(("notifyWebHook" . ,notify-webhook)))))
    (when repeat (setf body (append body `(("repeat" . ,repeat)))))
    (when max-rounds (setf body (append body `(("maxRounds" . ,max-rounds)))))
    (when max-rounds 
      (setf body 
	    (append body `(("pageProcessPattern" . ,(format nil "~{~a||~}" page-process-pattern))))))
    (let ((stream (drakma:http-request req-url :method :post :parameters body :want-stream t)))
      (with-decoded (decoded) stream
	(parse-and-raise-errors decoded)
	decoded))))

(defun bulk-job-pause (token name &key resume)
  "Pauses (or resumes) a previously created bulk job defined by name.

   token - your diffbot API token.
   name - name of the previously created bulk job.
   resume - set to nil to resume a job instead."
  (bulk-job-manage token name :pause (not resume)))

(defun bulk-job-delete (token name)
  "Deletes a previously created bulk job defined by name.

   token - your diffbot API token.
   name - name of the previously created bulk job."
  (bulk-job-manage token name :delete t))

(defun bulk-job-retrieve (token name)
  "Retrieves results of your completed bulk jobs.
   
   token - your diffbot API token.
   name - name of the previously created bulk job.

   Returns JSON formatted alist containing the data."
  (let* ((req-url (generate-endpoint (format nil "/bulk/download/~a-~a_data.json" token name)))
	 (stream (drakma:http-request req-url :method :get :want-stream t)))
    (with-decoded (decoded) stream
      (parse-and-raise-errors decoded)
      decoded)))

(defun bulk-job-manage (token name &key pause delete)
  "General bulk job management function."
  (let* ((req-url (generate-req-url "/bulk" `((token . ,token)
					      (name . ,name)
					      (pause . (if ,pause 0 1))
					      (delete . (if ,delete 0 1)))))
	 (stream (drakma:http-request req-url :method :get :want-stream t)))
    (with-decoded (decoded) stream
      (parse-and-raise-errors decoded)
      decoded)))

(defun bulk-job-view (token &key name)
  "View all active bulk job data.
   
   token - your diffbot API token.
   name - limit retrieval to a single job indicated by name.

   Returns JSON formatted alist containing the data."
  (get-request "/bulk" `((token . ,token)
			 (name . ,name))))
