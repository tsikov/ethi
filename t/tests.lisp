(cl:defpackage #:ethi-test
  (:use #:cl #:prove #:ethi))
(in-package #:ethi-test)

;; configure prove
(setf prove:*debug-on-error* t)
(setf prove:*enable-colors* nil)

;; set up test node
;; (ethi::modify-config :host "https://mainnet.infura.io/EyJE0yo8FulxSDAjRwMN")
;; (ethi::modify-config :port nil)

(ethi::modify-config :host "http://localhost")
(ethi::modify-config :port 8545)

(plan 1)

;;; helper functions

(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun starts-with-hex-p (str)
  "Determine whether `str` starts with `0x`"
  (starts-with-p str "0x"))

(defun hex-and-of-length (str length)
  (and (starts-with-hex-p str)
       (= (- (length str) 2) length)))

(defun transaction-hash-p (str)
  (hex-and-of-length str 64))

(defun create-transaction ()
  (ethi:eth/send-transaction *transaction-object*))



;; stubs

(defparameter *transaction-object*
  (ethi::transaction-object :from (car (ethi:eth/accounts))
                            :to (cadr (ethi:eth/accounts))
                            :value "0x9184e72a"
                            :data ""))

;;; tests

(vcr:with-vcr "rpc-endpoints"
  ;; (subtest "UTILS"
  ;;   (is (ethi::geth-method-to-cl-method "web3_clientVersion")
  ;;       "WEB3/CLIENT-VERSION"
  ;;       "Converts symbol to string for use of macro")
  ;;   )

  (subtest "API"
    ;; Note: I am assuming you have a running geth on your local
    ;; machine or have configured infra or similar service.
    ;; If not - expect this tests to fail... What you can do is
    ;; run geth for a minute, just for the tests to be recorded
    ;; by VCR and then stop it.
    (subtest "Given the correct host and port are given"

      (ok (starts-with-p (ethi:web3/client-version)
                         "Geth")
          "Can give a client version back")

      (is (ethi:web3/sha3 "0x68656c6c6f20776f726c64")
          "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
          "Can give a sha3 of data")

      (is (ethi:net/version)
          "555" ; check out the t/genesis.json file where it is specified
          "Can give the net version")

      (ok (starts-with-hex-p (ethi:net/peer-count))
          "Can give the peer count")

      (ok (starts-with-hex-p (ethi:eth/protocol-version))
          "Can give the protocol version")

      ;; it is either nil or a list with relevant data
      (ok (listp (ethi:eth/syncing))
          "Can check if geth is syncing")

      ;; (ok (starts-with-hex-p (ethi:eth/coinbase))
      ;;     "Can check the coinbase address")

      (ok (listp (ethi:eth/mining))
          "Can check if geth is currently mining")

      (ok (starts-with-hex-p (ethi:eth/hashrate))
          "Can check what is the hashrate of the mining operation")

      (ok (starts-with-hex-p (ethi:eth/gas-price))
          "Can return the current price per gas in wei")

      (ok (listp (ethi:eth/accounts))
          "Can return all accounts associated with the node")

      (ok (starts-with-hex-p (ethi:eth/block-number))
          "Can return the most recent block")

      (ok (starts-with-hex-p
           (ethi:eth/get-balance
            "0x267be1c1d684f78cb4f6a176c4911b741e4ffdc0" "latest"))
          "Can return the balance of an address")

      (ok (starts-with-hex-p
           (ethi:eth/get-storage-at
            "0x295a70b2de5e3953354a6a8344e616ed314d7251"
            "0x0"
            "latest"))
          "Can return the value from a storage position at a given address")

      (ok (starts-with-hex-p
           (ethi:eth/get-transaction-count
            "0x6085a0b5665b967e1db15b16ae88853b5040cad3"
            "latest"))
          "Can return the number of transactions send from an address")

      (ok (starts-with-hex-p
           (ethi:eth/get-block-transaction-count-by-hash
            (cdr (assoc :hash (ethi:eth/get-block-by-number "0x1" nil)))))
          "Can return the number of transactions in a block from the block hash")

      (ok (starts-with-hex-p
           (ethi:eth/get-block-transaction-count-by-number "latest"))
           "Can get the number of transactions in a block from a block number")

      (ok (starts-with-hex-p
           (ethi:eth/get-uncle-count-by-block-hash
            (cdr (assoc :hash (ethi:eth/get-block-by-number "0x1" nil)))))
          "Returns the number of uncles in a block from a block matching the
given block hash")

      (ok (starts-with-hex-p
           (ethi:eth/get-uncle-count-by-block-number "latest"))
          "Returns the number of uncles in a block from a block matching the
given block number")

      (ok (starts-with-hex-p
           (ethi:eth/get-code
            "0xa94f5374fce5edbc8e2a8697c15331677e6ebf0b"
            "latest"))
          "Returns code at a given address.")

      (ok (starts-with-hex-p
           (ethi:eth/sign (car (ethi:eth/accounts)) "0xdeadbeaf"))
          "Can sign data")

      (ok (transaction-hash-p
           (ethi:eth/send-transaction *transaction-object*))
          "Can send transaction")

      ;; (ok (transaction-hash-p
      ;;      (ethi:eth/send-raw-transaction ""))
      ;;     "Can create new message call transaction or a contract creation for signed transactions.")

      (ok (starts-with-hex-p
           (ethi:eth/call *transaction-object* "latest"))
          "Can execute a new message call immediately without creating a transaction on the block chain")

      (ok (starts-with-hex-p
           (ethi:eth/estimate-gas *transaction-object*))
          "Can make a call or transaction, which won't be added to the blockchain and returns the used gas, which can be used for estimating the used gas")

      (ok (starts-with-hex-p
           (let ((block-hash (cdr (assoc :hash (ethi:eth/get-block-by-number "0x1" nil)))))
             (cdr (assoc :hash
                         (ethi:eth/get-block-by-hash block-hash nil)))))
           "Returns information about a block by block hash.")

      (ok (starts-with-hex-p
           ;; right now with `t` or `nil` geth returns the whole transaction.
           ;; this is a bug with geth
           (cdr (assoc :hash (ethi:eth/get-block-by-number "0x1" nil))))
          "Returns information about a block by block number.")

      (ok (listp
           (let ((transaction-hash (create-transaction)))
            (ethi:eth/get-transaction-by-hash transaction-hash)))
          "Can return the information about a transaction requested by transaction hash.")

      (ok (listp
           (ethi:eth/get-transaction-by-block-hash-and-index
            "0x0000000000000000000000000000000000000000000000000000000000000000"
            "0x0"))
          "Can return information about a transaction by block hash and transaction index position.")

      (ok (listp
           (ethi:eth/get-transaction-by-block-number-and-index
            "0x0" "0x0"))
          "Can return information about a transaction by block hash and transaction index position.")

      (ok (listp
           (let ((transaction-hash (create-transaction)))
             (ethi:eth/get-transaction-receipt transaction-hash)))
          "Can return the receipt of a transaction by transaction hash.")



      )
    )
  )

(finalize)
