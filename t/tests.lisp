(cl:defpackage #:ethi-test
  (:use #:cl #:prove #:ethi))
(in-package #:ethi-test)

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
       (= (length (+ 2 str)) length)))

;;; tests

(vcr:with-vcr "rpc-endpoints"

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
          "1"
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
            "0xeff0e1a88aa19ec53e5e1f9b3a4ea3dd897f037cc36c786712b93f0a197eb79c"))
          "Can return the number of transactions in a block from the block hash")

      (ok (starts-with-hex-p
           (ethi:eth/get-block-transaction-count-by-number "latest"))
           "Can get the number of transactions in a block from a block number")

      (ok (starts-with-hex-p
           (ethi:eth/get-uncle-count-by-block-hash
            "0xb903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"))
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

      ;; needs the first account to be unlocked
      (ok (starts-with-hex-p
           (ethi:eth/sign (car (ethi:eth/accounts)) "0xdeadbeaf"))
          "Can sign data")
      )
    )
  )

(finalize)
