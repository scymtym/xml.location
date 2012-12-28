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
