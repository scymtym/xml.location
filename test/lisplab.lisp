;;; lisplab.lisp --- Unit tests for XML conversion of lisplab matrices.
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
