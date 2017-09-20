(cl:defpackage #:ethi-test
  (:use #:cl #:prove #:ethi))
(in-package #:ethi-test)

(plan 1)

(subtest "API"
  (subtest "Given the correct host and port is given"
    (ok (ethi:client-version)
        "Can give a client version back")

    ;; (is (ethi:connected?)
        ;; t
        ;; "Can check if the client is connected to the RPC")
  )
)

(finalize)
