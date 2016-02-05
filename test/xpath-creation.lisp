;;;; xpath-creation.lisp --- Unit tests for XPath creation.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite xpath-creation-root (root)
  ()
  (:documentation
   "Root test suite for XPath creation tests."))

(addtest (xpath-creation-root
          :documentation
          "Smoke test for the `create-xpath' function.")
  create-xpath/smoke

  (ensure-cases (node xpath &optional (expected t))
      `( ;; Element creation.
        (,(stp:make-element "root") "bar")
        (,(stp:make-element "root") "bar[1]")
        (,(stp:make-element "root") "node()")
        (,(stp:make-element "root") "*")
        (,(stp:make-element "root") "foo/bar")
        (,(stp:make-element "root") "foo/bar[1]")
        (,(stp:make-element "root") "foo/node()")
        (,(stp:make-element "root") "foo/*")

        ;; Attribute creation.
        (,(stp:make-element "root") "@baz")
        (,(stp:make-element "root") "@*")

        ;; Predicates.
        (,(stp:make-element "root") "bar[@foo=\"bar\"]")

        ;; Functions.
        (,(stp:make-element "root") "union(bar)")

        ;; Some invalid cases.
        (,(stp:make-element "root") "bar[2]"            xpath-creation-error))

    (let+ (((&flet do-it ()
              (create-xpath node xpath))))
      (ecase expected
        (xpath-creation-error
         (ensure-condition xpath-creation-error (do-it)))
        ((t)
         (do-it)
         (ensure-null (xpath:node-set-empty-p
                       (xpath:evaluate xpath node))))))))
