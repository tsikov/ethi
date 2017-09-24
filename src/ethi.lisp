(cl:defpackage #:ethi
  (:use #:cl)
  ;; web3
  (:export #:web3/client-version
           #:web3/sha3)
  ;; net
  (:export #:net/version
           #:net/peer-count)
  ;; eth
  (:export #:eth/protocol-version
           #:eth/syncing
           #:eth/coinbase
           #:eth/mining
           #:eth/hashrate
           #:eth/gas-price
           #:eth/accounts
           #:eth/block-number
           #:eth/get-balance
           #:eth/get-storage-at
           #:eth/get-transaction-count
           #:eth/get-block-transaction-count-by-hash
           #:eth/get-block-transaction-count-by-number
           #:eth/get-uncle-count-by-block-hash
           #:eth/get-uncle-count-by-block-number
           #:eth/get-code
           #:eth/sign)
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

(defun response-error (response)
  (cdr (assoc :error response)))

(defun handle-response (response)
  "Convert from json, get result, handle errors, etc..."
  ;; decode JSON string to CL alist
  (let ((decoded-response (cl-json:decode-json-from-string response)))
    ;; check for errors
    (if (response-error decoded-response)
        (error (response-error decoded-response))
        ;; ignore the rest of the response and return the result
        (cdr (assoc :result decoded-response)))))

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

(defmacro declare-endpoint (method &rest params)
  (labels (;; let's declare 2 helper functions for the macro
           (camel-case-to-kebab-case (str)
             (with-output-to-string (out)
               (loop for c across str
                  if (upper-case-p c)
                  do (format out "-~A" c)
                  else
                  do (format out "~A" (char-upcase c)))))
           (geth-method-to-cl-method (geth-method)
             (let* (;; this will deal with the namespace: _ -> /
                    (cl-method (substitute #\/ #\_ geth-method))
                    ;; this will deal with the case: fooBar -> foo-bar
                    (cl-method (camel-case-to-kebab-case cl-method)))
               cl-method)))

  ;; The macro itself is really simple.
  `(defun ,(intern (geth-method-to-cl-method method)) ,params
     (api-call ,method (list ,@params)))))

;;;; API
;;; web3
(declare-endpoint "web3_clientVersion")
(declare-endpoint "web3_sha3" data)

;;; net
(declare-endpoint "net_version")
(declare-endpoint "net_peerCount")

;;; eth
(declare-endpoint "eth_protocolVersion")
(declare-endpoint "eth_syncing")
(declare-endpoint "eth_coinbase")
(declare-endpoint "eth_mining")
(declare-endpoint "eth_hashrate")
(declare-endpoint "eth_gasPrice")
(declare-endpoint "eth_accounts")
(declare-endpoint "eth_blockNumber")
(declare-endpoint "eth_getBalance" address quantity/tag)
(declare-endpoint "eth_getStorageAt" address quantity quantity/tag)
(declare-endpoint "eth_getTransactionCount" address quantity/tag)
(declare-endpoint "eth_getBlockTransactionCountByHash" block-hash)
(declare-endpoint "eth_getBlockTransactionCountByNumber" quantity/tag)
(declare-endpoint "eth_getUncleCountByBlockHash" bock-hash)
(declare-endpoint "eth_getUncleCountByBlockNumber" quantity/tag)
(declare-endpoint "eth_getCode" address quantity/tag)
(declare-endpoint "eth_sign" address data)
