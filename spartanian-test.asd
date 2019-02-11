#|
  This file is a part of spartanian project.
|#

(defsystem "spartanian-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("spartanian"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "spartanian"))))
  :description "Test system for spartanian"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
