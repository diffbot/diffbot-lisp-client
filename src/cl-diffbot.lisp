;;;; cl-diffbot.lisp

(in-package #:cl-diffbot)

(defparameter *apiroot* "http://api.diffbot.com/")
(defparameter *apiversion* "v2")

;;; Utilitites
(defun generate-req-url (dest params)
  "Generates full request URL."
  (format nil "~a?~{~a~^&~}" (generate-endpoint dest) (generate-req-params params)))

(defun generate-endpoint (dest)
  "Generates API endpoints."
  (concatenate 'string *apiroot* *apiversion* dest))

(defun generate-req-params (args)
  "Generates and encodes URL parameters."
  (mapcan 
   (lambda (param)
     (cond ((member (car param) '(all stats) :test #'eq)
	    (list (format nil "~(~a~)=" (car param))))
	   ((cdr param)
	     (list (format nil "~(~a~)=~(~a~)" (car param) (cdr param)))))) args))

(defun list-to-fields (lst)
  "Transforms list to a 'fields' suitable parameter string."
  (when (and lst (listp lst))
    (loop 
       with fields = ""
       for elem in lst
       do (typecase elem
	    (list (setf fields 
			(format nil "~a~a(~{~a~^,~})," fields (car elem) (cdr elem))))
	    (t (setf fields (format nil "~a~a," fields elem))))
       finally (return (subseq fields 0 (1- (length fields)))))))

(defun parse-and-raise-errors (decoded)
  "Parses diffbot server response, raises an error if one is present."
  (when (and (listp decoded) (assoc :error decoded))
    (let ((code (cdr (assoc :error-code decoded)))
	  (message (cdr (assoc :error decoded))))
      (error 'diffbot-server-error 
	     :message message 
	     :status-code code
	     :format-control "Diffbot API returned error: \"~a\" Status code: ~a"
	     :format-arguments `(,message ,code)))))

(defmacro with-decoded ((decoded) stream &body body) 
  "Macro to decode stream."
  `(when ,stream
     (let ((,decoded (cl-json:decode-json ,stream)))
       (when ,decoded 
	 ,@body))))

(defun filter-params (param)
  "Filters out all other parameters but 'token' and 'url'"
  (when (or (eq 'token (car param))
	    (eq 'url (car param)))
    (list param)))

(defun call-api (dest params &key (method :get) body)
  "Helper function around drakma:http-request :method :get."
  (let ((req-url (generate-req-url dest params)) (stream))
    (case method
      (:post (setf stream (drakma:http-request req-url 
					       :method method 
					       :want-stream t 
					       :content-length t
					       :content-type "text/plain"
					       :content body)))
      (t (setf stream (drakma:http-request req-url :method method :want-stream t))))
    (with-decoded (decoded) stream
      (parse-and-raise-errors decoded)
      decoded)))

;;; Main
(defun article-api (token url &key (method :get) body fields timeout)
  "The Article API is used to extract clean article text from news article web pages.

   token - your diffbot API token.
   url - target URL.
   method - pass :post to provide text data to crawlbot (defalut :get).
   body - used when passed method is :post. Provide your textual data here.
   fields  - alist of values. Limit response to provided fields. See: diffbot.com/products/automatic/
   timeout - timeout value in milliseconds.

   Returns json formatted as alist."
  (unless (member method '(:get :post) :test #'eq)
    (parameter-error "Unsupported method ~S. Only :GET and :POST are supported." method))
  (when (and (eq method :post) (not body))
    (parameter-error ":POST method requires a valid :BODY."))
  (call-api "/article" `((token . ,token)
			 (url . ,url)
			 (fields . ,(list-to-fields fields))
			 (timeout . ,timeout)) :method method :body body))

(defun frontpage-api (token url &key (method :get) (format :xml) body timeout all)
  "The Frontpage API takes in a multifaceted “homepage” and returns individual page elements.

   token - your diffbot API token.
   url - target URL.
   method - pass :post to provide text data to crawlbot (defalut :get).
   body - used when passed method is :post. Provide your textual data here.
   timeout - timeout value in milliseconds. Overrides the default API value of 5000ms.
   all - Returns all content from page, including navigation and similar links that the Diffbot 
         visual processing engine considers less important / non-core.

   Returns json formatted as alist."
  (unless (member method '(:get :post) :test #'eq)
    (parameter-error "Unsupported method ~S. Only :GET and :POST are supported." method))
  (when (and (eq method :post) (not body))
    (parameter-error ":POST method requires a valid :BODY."))
  (unless (member format '(:xml :json) :test #'eq)
    (parameter-error "Unsupported format ~S. Only :XML and :JSON are supported." format))
  (call-api "/frontpage" `((token . ,token)
			   (url . ,url)
			   (timeout . ,timeout)
			   (format . ,format)
			   (all . ,all)) :method method :body body))

(defun product-api (token url &key fields timeout)
  "The Product API analyzes a shopping or e-commerce product page and returns information on the product.

   token - your diffbot API token.
   url - target URL.
   fields - alist of values. Limit response to provided fields. See: diffbot.com/products/automatic/
   timeout - timeout value in milliseconds.

   Returns json formatted as alist."
  (call-api "/product" `((token . ,token)
			 (url . ,url)
			 (fields . ,(list-to-fields fields))
			 (timeout . ,timeout))))

(defun image-api (token url &key fields timeout)
  "The Image API analyzes a web page and returns its primary image(s).

   token - your diffbot API token.
   url - target URL.
   fields - alist of values. Limit response to provided fields. See: diffbot.com/products/automatic/
   
   Returns json formatted as alist."
  (call-api "/image" `((token . ,token)
		       (url . ,url)
		       (fields . ,(list-to-fields fields))
		       (timeout . ,timeout))))

(defun page-classifier-api (token url &key mode fields stats)
  "The Page Classifier API takes any web link and automatically determines what type of page it is.
   
   token - your diffbot API token.
   url - target URL.
   mode - page-type. See: http://www.diffbot.com/dev/docs/analyze/
   fields - alist of values. Limit response to provided fields. See: diffbot.com/products/automatic/
   stats - set to true to return statistics on page classification and extraction.
 
   Returns json formatted as alist."
  (when (and mode (not (member mode '(article frontpage image product))))
    (parameter-error 
     "Unsupported mode ~S. Only existing page-types (e.g. :ARTICLE) are supported." mode))
  (call-api "/analyze" `((token . ,token)
			 (url . ,url)
			 (mode . ,mode)
			 (fields . ,(list-to-fields fields))
			 (stat . ,stats))))

(defun custom-api (token name url &key (method :get) body timeout)
  "Call previously created custom API.

   token - your diffbot API token.
   name - name of your custom API.
   url - target URL.
   method - pass :post to provide text data to crawlbot (defalut :get).
   body - used when passed method is :post. Provide your textual data here.
   timeout - timeout value in milliseconds.
   "
  (unless (member method '(:get :post) :test #'eq)
    (parameter-error "Unsupported method ~S. Only :GET and :POST are supported." method))
  (when (and (eq method :post) (not body))
    (parameter-error ":POST method requires a valid :BODY."))
  (call-api (concatenate 'string "/" name) `((token . ,token)
					     (url . ,url)
					     (timeout . ,timeout)) :method method :body body))
