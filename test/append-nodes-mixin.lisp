;;; append-nodes-mixin.lisp --- Unit tests for the append-nodes-mixin class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package #:xml.location.test)

(deftestsuite append-nodes-mixin-root (root)
  ((m1 #md((1 2)))
   (m2 #md((3 4))))
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
    (ensure-same
     (first (val loc :type 'string)) "hello world"
     :test #'string=))

  (let* ((loc   (loc "<foo/>" "foo/bar"
		:assign-mode :append))
	 (value (progn
		  (setf (val loc) m1
			(val loc) m2)
		  (val loc :type 'lisplab:matrix-base))))
    (ensure-same
     (length value) 2
     :test #'=)
    (ensure-same
     (first value) m1
     :test #'lisplab:.=)
    (ensure-same
     (second value) m2
     :test #'lisplab:.=)))

(addtest (append-nodes-mixin-root
          :documentation
	  "Test behavior of the `append-nodes-mixin' mixin class in
case of multiple matches.")
  multiple-matches

  (let* ((loc   (loc "<foo><bar/><bar/></foo>" "foo/bar/baz"
		     :assign-mode :append))
	 (value (progn
		  (setf (val loc) m1
			(val loc) m2)
		  (val loc :type 'lisplab:matrix-base))))
    (setf (val loc) m1
	  (val loc) m2)

    (ensure-same
     (length value) 4
     :test #'=)
    (ensure-same
     (first value) m1
     :test #'lisplab:.=)
    (ensure-same
     (second value) m2
     :test #'lisplab:.=)
    (ensure-same
     (third value) m1
     :test #'lisplab:.=)
    (ensure-same
     (fourth value) m2
     :test #'lisplab:.=)))
