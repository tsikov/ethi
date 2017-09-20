;;;; ethi.asd

(asdf:defsystem #:ethi
  :description "Interface for ethereum's RPC"
  :author "Petko Tsikov <tsikov@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "ethi"))))
  :depends-on (#:cl-json
               #:drakma)
  :in-order-to ((test-op (test-op ethi-test))))

(asdf:defsystem #:ethi-test
  :description "Test the ethi library"
  :depends-on (#:ethi
               #:prove
               #:vcr)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module "t"
                        :components
                        ((:test-file "tests"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
