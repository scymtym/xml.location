;;;; location-mixins.lisp --- Unit tests for location mixin classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

;;; Unit tests for `ignore-empty-result-mixin' class

(deftestsuite ignore-empty-results-mixin (root)
  ()
  (:documentation
   "Unit tests for the `ignore-empty-results-mixin' mixin class."))

(addtest (ignore-empty-results-mixin
          :documentation
	  "Smoke test for the `ignore-empty-results-mixin' mixin
class.")
  smoke

  ;; Create locations for the specified documents and paths and then
  ;; ensure that unmatched paths work as expected.
  (ensure-cases (doc path &rest args)
		'(("<bla/>"
		   "bla/bar"))

    (let ((loc (apply #'loc doc path :if-no-match :do-nothing args)))
      ;; name
      (ensure-same (name loc) nil)
      (setf (name loc) "foo")
      ;; val
      (ensure-same (val loc) nil)
      (setf (val loc) "foo")
      ;; @
      (ensure-same (@ loc "does-not-exist") nil)
      (setf (@ loc "does-not-exist") "foo")

      ;; Ensure that the `setf' method did not change anything.
      (let ((new-doc (stp:serialize (location-document loc)
				    (cxml:make-string-sink
				     :omit-xml-declaration-p t))))
	(ensure-same
	 new-doc doc
	 :test      #'string=
	 :report    "~@<After applying `setf' methods, the document ~S ~
is no longer identical to the original document ~S.~@:>"
	 :arguments (new-doc doc))))))

;;; Unit tests for `create-missing-nodes-mixin' class

(deftestsuite create-missing-nodes-mixin-root (root)
  ((cases ))
  (:documentation
   "Unit tests for the `create-missing-nodes-mixin' class."))

(addtest (create-missing-nodes-mixin-root
          :documentation
	  "Smoke test for the `create missing-nodes-mixin' class.")
  smoke

  ;; Create locations for the specified documents and paths and then
  ;; check whether the XPaths match on the modified documents.
  (ensure-cases (doc path &rest args)
      '(("<bla><blup/><blop/></bla>"
	 "/bla/blup/bli/node()")

	("<bla><bli/><bli c='foo'/><bli><blup/></bli><bli><blup a='foo'/></bli></bla>"
	 "/bla/bli/blup/@a"
	 :if-multiple-matches :all)

	("<bla><bli/><bli c='foo'/><bli><blup/></bli><bli><blup a='foo'/></bli></bla>"
	 "/bla/bli/blup/@a"
	 :if-multiple-matches :all)

	("<bla/>"
	 "/bla/text()"))

    (let ((loc (apply #'loc doc path :if-no-match :create args)))
      (ensure (not (xpath:node-set-empty-p
		    (xpath:evaluate
		     path (location-document loc))))))))

;;; Unit tests for `append-nodes-mixin' class

(deftestsuite append-nodes-mixin-root (root)
  ()
  (:documentation
   "Unit tests for the `append-nodes-mixin' mixin class."))

(addtest (append-nodes-mixin-root
          :documentation
	  "Smoke test for the `append-nodes-mixin' mixin class.")
  smoke

  (let ((loc (loc "<foo/>" "foo/bar/text()" :assign-mode :append)))
    (setf (val loc) "hello"
	  (val loc) " world")
    (ensure-same (first (val loc :type 'string)) "hello world"
		 :test #'string=)))

(addtest (append-nodes-mixin-root
          :documentation
	  "Test behavior of the `append-nodes-mixin' mixin class in
case of multiple matches.")
  multiple-matches

  ;; Note that the specifies XPath matches both "bar" elements and
  ;; that M1 and M2 will consequently be stored at both locations,
  ;; yielding a total of four stored objects: two copies of M1 and two
  ;; copies of M2.
  (let* ((m1 (make-instance 'mock-domain-object :content 1))
	 (m2 (make-instance 'mock-domain-object :content 2))
	 (loc   (loc "<foo><bar/><bar/></foo>" "foo/bar/baz"
		     :assign-mode :append))
	 (value (progn
		  (setf (val loc) m1
			(val loc) m2)
		  (val loc :type 'mock-domain-object))))
    (ensure-same value (list m1 m2 m1 m2)
		 :test (lambda (left right)
			 (and (length= left right)
			      (every #'(lambda (left right)
					 (= (mock-domain-object-content left)
					    (mock-domain-object-content right)))
				     left right))))))
