;;;; lisplab.lisp --- Unit tests for XML conversion of lisplab matrices.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite lisplab-root (root)
  ((locations '(("<foo/>"             "node()")
		("<foo bar=\"baz\"/>" "node()")
		("<foo><bar/></foo>"  "node()")))
   (matrices  `(,#md((1 2 3))
		,#md((1) (2) (3))
		,#md((1 2) (3 4))
		;,#mm((1 "2" :3))
		,#mm((1 2 3))
		,#mz((#c(1 2) #c(3 4))))))
  (:documentation
   "Unit tests for lisplab-related conversions."))

(addtest (lisplab-root
          :documentation
	  "Test xml-> conversion for lisplab matrices.")
  ->xml

  (iter (for (doc path) in locations)
	(iter (for matrix in matrices)
	      (let ((loc (loc doc path)))
		(setf (val loc) matrix)))))

(addtest (lisplab-root
          :documentation
	  "Test ->xml conversion for lisplab matrices.")
  xml->-new-instance

  (iter (for (doc path) in locations)
	(iter (for matrix in matrices)
	      (let* ((loc    (loc doc path))
		     (result (progn
			       (setf (val loc) matrix)
			       (val loc :type 'lisplab:matrix-base))))
		(ensure-same
		 (lisplab:element-type result)
		 (lisplab:element-type matrix)
		 :test #'type=)
		(ensure-same
		 result matrix
		 :test #'lisplab:.=)))))

(addtest (lisplab-root
          :documentation
	  "Test ->xml conversion for lisplab matrices.")
  xml->-into-instance

  (iter (for (doc path) in locations)
	(iter (for matrix in matrices)
	      (let ((loc    (loc doc path))
		    (result (lisplab:copy matrix)))
		(setf (val loc) matrix)
		(val loc :type result)
		(ensure-same
		 result matrix
		 :test #'lisplab:.=)))))

(addtest (lisplab-root
          :documentation
	  "Test conditions signaled by ->xml conversion for lisplab
matrices.")
  xml->-conditions

  (ensure-cases (doc xpath type)
      '(("<foo rows='1' cols='3' element-type='double-float'>1 2</foo>"
	 "node()"
	 lisplab:matrix-base))
    (ensure-condition 'xml->-conversion-error
      (let ((loc (loc doc xpath)))
	(val loc :type type)))))

(addtest (lisplab-root
          :documentation
	  "Test appending behavior of ->xml conversion for lisplab
matrices.")
  append

  (let* ((m1    (first matrices))
	 (m2    (second matrices))
	 (loc   (loc "<foo/>" "foo/bar"
		     :assign-mode :append))
	 (value (progn
		  (setf (val loc) m1
			(val loc) m2)
		  (val loc :type 'lisplab:matrix-base))))
    (ensure-same (length value) 2  :test #'=)
    (ensure-same (first value)  m1 :test #'lisplab:.=)
    (ensure-same (second value) m2 :test #'lisplab:.=)))
