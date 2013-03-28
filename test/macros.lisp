;;;; macros.lisp --- Unit tests for the `with-locations' macros.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite macros-root (root)
  ((simple-document    (cxml:parse
                        "<bla foo='1 2 4' bar='baz'>foo</bla>"
                        (stp:make-builder)))
   (namespace-document (cxml:parse
                        "<bla
xmlns:foo=\"http://foo.bar\"
xmlns:baz=\"http://baz.doo\"
foo:bar='foo.bar:bar' baz:bar='baz.doo:bar'/>"
                        (stp:make-builder)))
   (multi-document     (cxml:parse
                        "<bla>foo<bar foo='1'/><bar foo='2'/></bla>"
                        (stp:make-builder))))
  (:documentation
   "Unit tests for the `with-locations' macros."))

(addtest (macros-root
          :documentation
          "Smoke test for the `with-locations' macros.")
  smoke

  (with-locations (((:name name)                          "node()")
                   (text                                  "bla/text()")
                   ((:@ (foo "foo") :type '(list number)) "node()")
                   ((@ bar)                               "node()"))
      simple-document
    ;; Extract values from generalized variables
    (ensure-same name "bla"    :test #'string=)
    (ensure-same foo  '(1 2 4) :test #'equalp)
    (ensure-same bar  "baz"    :test #'string=)
    (ensure-same text "foo"    :test #'string=)

    ;; Set values of generalized variables
    (setf name "frooble"
          foo  '(5 6)
          bar  42
          text "bubba"))
  (ensure-same
   (with-output-to-string (stream)
     (stp:serialize simple-document
                    (cxml:make-character-stream-sink
                     stream :omit-xml-declaration-p t)))
   "<frooble foo=\"5 6\" bar=\"42\">bubba</frooble>"
   :test #'string=))

(addtest (macros-root
          :documentation
          "Test :loc binding form of `with-locations' macro.")
  loc

  (with-locations (((:loc self) "bla")
                   ((:loc text) "bla/text()")
                   ((:loc self-no-path))) multi-document
    (ensure-same (name self) "bla" :test #'string=)
    (ensure-same (val text)  "foo" :test #'string=)
    (ensure-same (location-result self-no-path) multi-document
                 :test #'eq)))

(addtest (macros-root
          :documentation
          "Unit test for conditions that should be signaled by
`with-locations' macros.")
  conditions

  (iter (for macro in '(with-locations with-locations-r/o))
        (ensure-condition 'invalid-binding-form
          (macroexpand `(,macro ("bla") simple-document)))
        (ensure-condition 'invalid-binding-form
          (macroexpand `(,macro (1) simple-document)))
        (ensure-condition 'invalid-binding-form
          (macroexpand `(,macro (:key-without-value) simple-document)))
        (ensure-condition 'no-such-accessor-form
          (macroexpand `(,macro ((() "p")) simple-document)))
        (ensure-condition 'no-such-accessor-form
          (macroexpand `(,macro (((foo) "p")) simple-document)))
        (ensure-condition 'no-such-accessor-form
          (macroexpand `(,macro (((foo bar) "p")) simple-document)))
        (ensure-condition 'no-such-accessor-form
          (macroexpand `(,macro (((:foo) "p")) simple-document)))
        (ensure-condition 'no-such-accessor-form
          (macroexpand `(,macro (((:foo bar :baz 1) "p"))
                                simple-document)))))

(addtest (macros-root
          :documentation
          "Ensure that optimized expansions still produce valid
results.")
  optimizations

  (with-locations (((:@ (bar1 "ns:bar")) "node()"
                    :namespaces '(("ns" . "http://foo.bar")))
                   ((:@ (bar2 "ns:bar")) "node()"
                    :namespaces '(("ns" . "http://baz.doo"))))
      namespace-document
    (ensure-same bar1 "foo.bar:bar" :test #'string=)
    (ensure-same bar2 "baz.doo:bar" :test #'string=)))

(addtest (macros-root
          :documentation
          "Test specifying global options in `with-locations'.")
  options

  (with-locations-r/o (((:@ foo :type 'number) "bla/bar")
                       :if-multiple-matches :all) multi-document
    (ensure-same foo '(1 2) :test #'equalp)))
