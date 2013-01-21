;;;; append-nodes-mixin.lisp --- Unit tests for the append-nodes-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite append-nodes-mixin-root (root)
  ()
  (:documentation
   "Unit tests for the `append-nodes-mixin' mixin class."))

(addtest (append-nodes-mixin-root
          :documentation
	  "Smoke test for the `append-nodes-mixin' mixin class.")
  smoke

  (let ((loc (loc "<foo/>" "foo/bar/text()"
		  :assign-mode :append)))
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
