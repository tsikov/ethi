(cl:defpackage #:ethi-test
  (:use #:cl #:prove #:ethi))
(in-package #:ethi-test)

(plan 1)

(vcr:with-vcr "rpc-endpoints"

  (subtest "API"
    ;; Note: I am assuming you have a running geth on your local
    ;; machine or have configured infra or similar service.
    ;; If not - expect this tests to fail... What you can do is
    ;; run geth for a minute, just for the tests to be recorded
    ;; by VCR and then stop it.
    (subtest "Given the correct host and port are given"

      (is (subseq (ethi:web3/client-version) 0 4)
          "Geth"
          "Can give a client version back")

      (is (ethi:web3/sha3 "0x68656c6c6f20776f726c64")
          "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
          "Can give a sha3 of data")

      (is (ethi:net/version)
          "1"
          "Can give the net version")

      (is (ethi:net/peer-count)
          "0x64"
          "Can give the peer count")

      (is (ethi:eth/protocol-version)
          "0x3f"
          "Can give the protocol version")

      )
    )
  )

(finalize)
