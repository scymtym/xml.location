;;;; ignore-empty-results-mixin.lisp --- Unit tests for ignore-empty-results-mixin.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite ignore-empty-results-mixin (root)
  ((cases '(("<bla/>"
	     "bla/bar"))))
  (:documentation
   "Unit tests for the `ignore-empty-results-mixin' mixin class."))

(addtest (ignore-empty-results-mixin
          :documentation
	  "Smoke test for the `ignore-empty-results-mixin' mixin
class.")
  smoke

  ;; Create locations for the specified documents and paths and then
  ;; ensure that unmatched paths work as expected.
  (iter (for (doc path . args) in cases)
	(let ((loc (apply #'loc doc path
			  :if-no-match :do-nothing
			  args)))
	  ;; name
	  (ensure-same
	   (name loc) nil)
	  (setf (name loc) "foo")
	  ;; val
	  (ensure-same
	   (val loc) nil)
	  (setf (val loc) "foo")
	  ;; @
	  (ensure-same
	   (@ loc "does-not-exist") nil)
	  (setf (@ loc "does-not-exist") "foo")

	  (let ((new-doc (stp:serialize (location-document loc)
					(cxml:make-string-sink
					 :omit-xml-declaration-p t))))
	  (ensure-same
	   new-doc doc
	   :test      #'string=
	   :report    "~@<After applying `setf' methods, the document ~S ~
is no longer identical to the original document ~S.~@:>"
	   :arguments (new-doc doc))))))
