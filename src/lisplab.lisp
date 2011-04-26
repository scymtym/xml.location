;;; lisplab.lisp --- XML conversions for Lisplab data types.
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

(in-package :cxml-location)


;;; XML -> conversion
;;

(defmethod xml-> ((value stp:element)
		  (type  (eql 'lisplab:matrix-base))
		  &key
		  inner-types
		  &allow-other-keys)
  "Create a matrix from VALUE by extracting meta-data from attributes
and reading elements from a text child node."
  (with-locations-r/o (((:@   rows         :type 'number)        ".")
		       ((:@   cols         :type 'number)        ".")
		       ((:@   element-type :type 'symbol)        ".") ;; TODO what about (complex double-float)?
		       ((:val text         :type '(list number)) "text()[last()]"))
      value

    (let* ((class  (%element-type->matrix-class
		    (or (first inner-types) element-type)))
	   (result (lisplab:mnew class 0 rows cols)))
      (lisplab:import-list result text)
      result)))


;;; -> XML conversion
;;

(defmethod ->xml :before ((value lisplab:matrix-base)
			  (dest  stp:element)
			  (type  t)
			  &key
			  inner-types
			  &allow-other-keys)
  ;; When INNER-TYPES is supplied, make sure that the specified
  ;; element type is a subtype of the actual element type.
  (when inner-types
    (unless (subtypep (first inner-types)
		      (lisplab:element-type value))
      (error 'type-error
	     :datum         (first inner-types)
	     :expected-type (lisplab:element-type value)))))

(defmethod ->xml ((value lisplab:matrix-base)
		  (dest  stp:element)
		  (type  t)
		  &key
		  inner-types
		  &allow-other-keys)
  "Store the matrix VALUE into DEST by putting meta-data into
attributes and using a text child node for the elements."
  (let ((element-type1 (or (first inner-types)
			   (lisplab:element-type value))))
    (with-locations (((:@   rows         :type 'number) ".")
		     ((:@   cols         :type 'number) ".")
		     ((:@   element-type :type 'symbol) ".")
		     ((:val text         :type `(list ,element-type1))
		                                        "text()"))
	dest

      (setf rows         (lisplab:rows value)
	    cols         (lisplab:cols value)
	    element-type element-type1
	    text         (lisplab:export-list value))))
  value)


;;; Utility functions
;;

(defun %element-type->matrix-class (element-type)
  "Map ELEMENT-TYPE to a matrix class with general structure and ffi
implementation, if possible."
  (cond
    ((eq element-type 'double-float)
     'lisplab:matrix-dge)
    ((equal element-type '(complex double-float))
     'lisplab:matrix-zge)
    (t
     'lisplab:matrix-ge)))
