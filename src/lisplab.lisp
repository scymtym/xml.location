;;;; lisplab.lisp --- XML conversions for Lisplab data types.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)


;;; XML -> conversion
;;

(defmethod xml-> ((value stp:element)
		  (type  lisplab:matrix-base)
		  &key &allow-other-keys)
  "Store matrix data in VALUE into the matrix instance TYPE. Signal an
error if dimensions or element type are incompatible."
  (with-locations-r/o (((:@   rows         :type 'number)        ".")
		       ((:@   cols         :type 'number)        ".")
		       ((:@   element-type :type 'type)          ".")
		       ((:val text         :type '(list number)) "text()[last()]"))
      value

    ;; Check compatibility of dimensions.
    (unless (and (= (lisplab:rows type) rows)
		 (= (lisplab:cols type) cols))
      (xml->-conversion-error
       value type
       "~@<The stored matrix dimensions, ~D x ~D, do not match the ~
dimensions ~D x ~D of the supplied destination matrix.~@:>"
       rows cols (lisplab:rows type) (lisplab:cols type)))

    ;; Check compatibility of element types.
    (unless (subtypep element-type (lisplab:element-type type))
      (xml->-conversion-error
       value type
       "~@<The stored element type ~S is not a subtype of the element ~
type ~S of the supplied destination matrix.~@:>"
       element-type (lisplab:element-type type)))

    ;; Check number of matrix elements, import if ok.
    (let ((elements text))
      (unless (length= (the non-negative-integer (* rows cols))
		       elements)
	(xml->-conversion-error
	 value type
	 "~@<The number of stored elements, ~D, does not match the ~
number of elements, ~D, of the supplied destination matrix.~@:>"
	 (length elements) (* rows cols)))

      (lisplab:import-list type elements))

    type))

(defmethod xml-> ((value stp:element)
		  (type  (eql 'lisplab:matrix-base))
		  &key
		  inner-types
		  matrix-class
		  &allow-other-keys)
  "Create a matrix from VALUE by extracting meta-data from attributes
and reading elements from a text child node."
  (when (and inner-types matrix-class)
    (error "The keyword parameters ~S and ~S are mutually exclusive."
	   :inner-types :matrix-class))

  (with-locations-r/o (((:@ rows         :type 'number) ".")
		       ((:@ cols         :type 'number) ".")
		       ((:@ element-type :type 'type)   "."))
      value

    (let* ((class  (or matrix-class
		       (%element-type->matrix-class
			(or (first inner-types) element-type))))
	   (result (lisplab:mnew class 0 rows cols)))
      (xml-> value result))))

(macrolet ((define-matrix-xml->-method (class)
	     `(defmethod xml-> ((value stp:element)
				(type  (eql ',class))
				&key
				inner-types
				&allow-other-keys)
		(xml-> value 'lisplab:matrix-base
		       :inner-types  inner-types
		       :matrix-class ',class))))
  (define-matrix-xml->-method lisplab:matrix-dge)
  (define-matrix-xml->-method lisplab:matrix-zge)
  (define-matrix-xml->-method lisplab:matrix-ge))


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
		     ((:@   element-type :type 'type)   ".")
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
