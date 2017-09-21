(cl:defpackage #:ethi
  (:use #:cl)
  ;; web3
  (:export #:web3/client-version
           #:web3/sha3)
  ;; net
  (:export #:net/version
           #:net/peer-count)
  ;; eth
  (:export #:eth/protocol-version)
  )
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
  '(;; the default local RPC endpoint
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

(defun handle-response (response)
  "Convert from json, get result, handle errors, etc..."
  (cdr (assoc :result
         (cl-json:decode-json-from-string response))))

(defun make-request (uri raw-body)
  "Generic http post request with raw body"
  (drakma:http-request uri
                       :method :post
                       :content raw-body))

(defun api-call (method params)
  "To be used by all methods"
  (let ((raw-body (cl-json:encode-json-to-string
                   (make-body method params))))
    (handle-response
     (make-request (uri) raw-body))))

(defun camel-case-to-kebab-case (str)
  (with-output-to-string (out)
    (loop for c across str
       if (upper-case-p c)
       do (format out "-~A" c)
       else
       do (format out "~A" (char-upcase c)))))

(defun geth-method-to-cl-method (geth-method)
  (let* (;; this will deal with the namespace: _ -> /
         (cl-method (substitute #\/ #\_ geth-method))
         ;; this will deal with the case: fooBar -> foo-bar
         (cl-method (camel-case-to-kebab-case cl-method)))
    cl-method))

(defmacro declare-endpoint (method &rest params)
  `(defun ,(intern (geth-method-to-cl-method method)) ,params
     (api-call ,method (list ,@params))))

;;;; API
;;; web3
(defun web3/client-version ()
  (api-call "web3_clientVersion" #()))

(defun web3/sha3 (data)
  (api-call "web3_sha3" (list data)))

;;; net
(defun net/version ()
  (api-call "net_version" #()))

(defun net/peer-count ()
  (api-call "net_peerCount" #()))

;;; eth
(defun eth/protocol-version ()
  (api-call "eth_protocolVersion" #()))
