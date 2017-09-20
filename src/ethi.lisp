(cl:defpackage #:ethi
  (:use #:cl)
  (:export #:client-version
           #:connected?))
(in-package #:ethi)

;; make it possible for drakma to get json response as text
;; unless it is already possible
(pushnew '("application" . "json")
         drakma:*text-content-types*
         :test 'equal)

(defun uri ()
  "Construct the endpoint out of the configuration"
  (let ((host (getf *config* :host))
        (port (getf *config* :port)))
    (format nil "~A~@[:~A~]" host port)))

(defparameter *default-config*
  '(;; the RPC endpoint
    :host "http://localhost"
    :port 8545
    ;; this will be included in every method call unless overwrited
    :jsonrpc "2.0"
    :id 1)
  "If you are running a local node with the default configuration
you will not need to change this values.")

(defvar *config* *default-config*
  "The config being used by the wrapper")

(defun modify-config (key value)
  (setf (getf *config* key)
        value))

(defun config-id () (getf *config* :id))

(defun make-body (method params)
  `(("jsonrpc" . "2.0")
    ("method" . ,method)
    ("params" . ,params)
    ("id" . ,(config-id))))

(defun api-call (method params)
  (let ((raw-body (cl-json:encode-json-to-string
                   (make-body method params))))
    (drakma:http-request (uri)
                         :method :post
                         :content raw-body)))

(defun client-version ()
  (api-call "web3_clientVersion" #()))

(defun connected? nil
  "Attempts to do a simple call to the RPC endpoint")

