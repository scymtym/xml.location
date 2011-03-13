;;; macros.lisp --- Unit tests for the `with-locations' macros.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :cxml-location.test)

(deftestsuite macros-root (root)
  ((simple-document    (cxml:parse
			"<bla foo='1 2 4' bar='baz'>foo</bla>"
			(stp:make-builder)))
   (namespace-document (cxml:parse
			"<bla
xmlns:foo=\"http://foo.bar\"
xmlns:baz=\"http://baz.doo\"
foo:bar='foo.bar:bar' baz:bar='baz.doo:bar'/>"
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
    (ensure-same name "bla"
		 :test #'string=)
    (ensure-same foo '(1 2 4)
		 :test #'equal)
    (ensure-same bar "baz"
		 :test #'string=)
    (ensure-same text "foo"
		 :test #'string=)

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
	  "Unit test for conditions that should be signaled by
`with-locations' macros.")
  conditions

  (iter (for macro in '(with-locations with-locations-r/o))
	(ensure-condition 'no-such-accessor-form
	  (macroexpand `(,macro ((() "p")) simple-document)))
	(ensure-condition 'no-such-accessor-form
	  (macroexpand `(,macro (((foo) "p")) simple-document)))
	(ensure-condition 'no-such-accessor-form
	  (macroexpand `(,macro (((foo bar) "p")) simple-document)))
	(ensure-condition 'no-such-accessor-form
	  (macroexpand `(,macro (((:foo) "p")) simple-document)))
	(ensure-condition 'no-such-accessor-form
	  (macroexpand `(,macro (((:foo bar :baz 1) "p")) simple-document)))))

(addtest (macros-root
          :documentation
	  "Ensure that optimized expansions still produce valid results.")
  optimizations

  (with-locations (((:@ (bar1 "ns:bar")) "node()"
		    :namespaces '(("ns" . "http://foo.bar")))
		   ((:@ (bar2 "ns:bar")) "node()"
		    :namespaces '(("ns" . "http://baz.doo"))))
      namespace-document
    (ensure-same bar1 "foo.bar:bar"
		 :test #'string=)
    (ensure-same bar2 "baz.doo:bar"
		 :test #'string=)))
