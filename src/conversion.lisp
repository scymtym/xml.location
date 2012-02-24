;;; conversion.lisp --- To and from XML conversions for some data types.
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

(in-package :cxml-location)


;;; XML -> * Conversions
;;
;; There are two main paths for this conversion:
;;
;; 1. {Attribute,text} node -> string [ -> final type ]
;;    This conversion is partially implemented via some of the
;;    following methods. For final types other than string, there are
;;    two possibilities:
;;
;;    a) A `read'able representation of the value has been stored
;;       In this case, the conversion to the final type just uses
;;       `read'.
;;
;;    b) A conversion method has to be written
;;       Such a conversion method can be written like this:
;;
;;       (defmethod xml-> ((value t) (type MY-TYPE))
;;         (let ((string (xml-> value 'string)))
;;           (my-conversion string)))
;;
;; 2. Element node -> final type
;;
;;    This conversion is mostly used for more complex types that
;;    require a structured representation. For classes, methods should
;;    specialize `xml->' on the actual class, not its name. This
;;    allows instances as the second argument of `xml->'. The creation
;;    of instances can be handled automatically (see remark below) so
;;    that just specifying a class name as TYPE still works.
;;
;;    Obviously, this cannot be done if the actual class is determined
;;    by means of inspecting VALUE. In such cases, the name of a
;;    common superclass or some other distinctive symbol should be
;;    used as specializer.
;;
;; In addition, there is the following rule: If TYPE is a symbol other
;; than 'string, 'list or any other type that is either `read'able or
;; specializes `xml->', TYPE is taken to designate a class. In that
;; case, `make-instance' is called with TYPE and `xml->' is called
;; with the resulting instance as TYPE.

(macrolet
    ((define-error-method (name)
       `(defmethod ,name ((function (eql (fdefinition 'xml->)))
			  &rest args)
	 "Signal a conversion error if no applicable conversion method is
found."
	 (bind (((value type &rest key-args) args))
	   (declare (ignore key-args))
	   (error 'no-xml->-conversion-method
		  :value value
		  :type  type)))))

  (define-error-method no-applicable-method)
  #+sbcl (define-error-method sb-pcl::no-primary-method))

(defmethod xml-> ((value t) (type list)
		  &key &allow-other-keys)
  "Intended mainly to catch the case in which TYPE is nil."
  (xml->-conversion-error
   value type
   "~@<~S is not valid as a type specification.~@:>"
   type))

(defmethod xml-> :around ((value t) (type list)
			  &key &allow-other-keys)
  "Split composite type specification TYPE into head and tail."
  (cond
    ((null type)
     (call-next-method))
    ((length= 1 type)
     (xml-> value (first type)))
    (t
     (xml-> value (first type) :inner-types (rest type)))))

(defmethod xml-> ((value stp:document) (type t)
		  &rest args
		  &key &allow-other-keys)
  (apply #'xml-> (stp:document-element value) type args))

;; Case 1 methods

(defmethod xml-> ((value stp:text) (type t)
		  &rest args
		  &key &allow-other-keys)
  "Extract text from text node VALUE for further conversion."
  (apply #'xml-> (stp:data value) type args))

(defmethod xml-> ((value stp:text) (type (eql 'string))
		  &key &allow-other-keys)
  "Fast-path method for text nodes if TYPE is string."
  (stp:data value))

(defmethod xml-> ((value stp:attribute) (type t)
		  &rest args
		  &key &allow-other-keys)
  "Extract attribute value from VALUE for further conversion."
  (apply #'xml-> (stp:value value) type args))

(defmethod xml-> ((value stp:attribute) (type (eql 'string))
		  &key &allow-other-keys)
  "Fast-path method for attribute nodes if TYPE is string."
  (stp:value value))

(defmethod xml-> ((value stp:node) (type (eql 'string))
		  &key &allow-other-keys)
  "Catch-all method for STP nodes that have no obvious string
interpretation."
  (xml->-conversion-error
   value type
   "~@<Cannot extract a string from a ~A node; Append /text() to the ~
XPath if you intended to extract an element's text.~@:>"
   (type-of value)))

(defmethod xml-> ((value string) (type t)
		  &key &allow-other-keys)
  "Convert intermediate string VALUE to requested type TYPE."
  (nth-value 0 (read-from-string value)))

(defmethod xml-> ((value string) (type (eql 'string))
		  &key &allow-other-keys)
  "Fast-path method for string VALUE."
  value)

(defmethod xml-> ((value string) (type (eql 'list))
		  &key
		  (inner-types '(string) inner-types-supplied?))
  "Convert intermediate string VALUE to requested list type TYPE."
  (cond
    ((not inner-types-supplied?)
     (%split-at-whitespace value))
    ((length= 1 inner-types)
     (let ((inner-type (first inner-types)))
       (if (subtypep inner-type 'atom)
	   (%parse-list-of-atoms value)
	   (map 'list (rcurry #'xml-> inner-type)
		(%split-at-whitespace value)))))
    (t
     (map 'list (rcurry #'xml-> inner-types)
	  (%split-at-whitespace value)))))

(defmethod xml-> ((value string) (type (eql 'type))
		  &key &allow-other-keys)
  "Convert string VALUE to a Common Lisp type."
  (nth-value 0 (read-from-string value)))

;; Case 2 methods

(defmethod xml-> ((value stp:node) (type class)
		  &key &allow-other-keys)
  "Interpret TYPE as a class name. Try to create an instance and load
the contents of VALUE into that instance."
  (xml-> value (make-instance type)))

(defmethod xml-> ((value stp:node) (type symbol)
		  &key &allow-other-keys)
  "Interpret TYPE as a class name. Try to create an instance and load
the contents of VALUE into that instance."
  (let ((class (find-class type nil)))
    (unless class
      (xml->-conversion-error
       value type
       "~@<Since there is no ~S method specialized on the symbol ~S, ~
it is interpreted as a class name, but there is no class of that ~
name.~@:>"
       'xml-> type))
    (xml-> value class)))


;;; * -> XML Conversions
;;
;; There are two main paths for this conversion (this is mostly
;; analogous to the mechanics of `xml->')
;;
;; 1. VALUE -> nil                   DEST with 'string TYPE
;;          -> {text,attribute} node DEST with 'string TYPE
;;    This conversion is partially implemented via some of the
;;    following methods. For types other than string, there are two
;;    possibilities:
;;
;;    a) The type of VALUE has a `read'able print representation. In
;;       this case, the conversion to the final type just uses
;;       `prin1'.
;;
;;    b) A conversion method has to be written
;;       Such a conversion method can be written like this:
;;
;;       (defmethod ->xml ((value MY-TYPE) (dest (eql nil)) (type (eql 'string)))
;;         (my-to-string-conversion value))
;;
;; 2. VALUE -> element node DEST with arbitrary TYPE
;;    This conversion is mostly used for more complex types that
;;    require a structured representation. The value of TYPE is only
;;    relevant if more than one way to store objects of a given type
;;    is implemented. Otherwise, dispatching on VALUE is sufficient to
;;    select the appropriate method.

(macrolet
    ((define-error-method (name)
       `(defmethod ,name ((function (eql (fdefinition '->xml)))
			  &rest args)
	  "Signal a conversion error if no applicable conversion method is
found."
	  (bind (((value dest type &rest key-args) args))
	    (declare (ignore key-args))
	    (error 'no-->xml-conversion-method
		   :value       value
		   :destination dest
		   :type        type)))))

  (define-error-method no-applicable-method)
  #+sbcl (define-error-method sb-pcl::no-primary-method))

(defmethod ->xml ((value t) (dest t) (type list)
		  &key &allow-other-keys)
  "Intended mainly to catch the case in which TYPE is nil."
  (->xml-conversion-error
   value type dest
   "~@<~S is not valid as a type specification.~@:>"
   type))

(defmethod ->xml :around ((value t) (dest t) (type list)
			  &key &allow-other-keys)
  "Split composite type specification TYPE into head and tail."
  (cond
    ((null type)
     (call-next-method))
    ((length= 1 type)
     (->xml value dest (first type)))
    (t
     (->xml value dest (first type) :inner-types (rest type)))))

(defmethod ->xml ((value t) (dest stp:document) (type t)
		  &rest args
		  &key &allow-other-keys)
  "Convert VALUE to string and store in DEST."
  (apply #'->xml value (stp:document-element dest) type args))

;; Case 1 methods

(defmethod ->xml ((value t) (dest stp:text) (type t)
		  &key &allow-other-keys)
  "Convert VALUE to string and store in DEST."
  (setf (stp:data dest) (->xml value 'string type))
  value)

(defmethod ->xml ((value string) (dest stp:text) (type t)
		  &key &allow-other-keys)
  "Fast-path method for storing string VALUE into text node DEST."
  (setf (stp:data dest) value))

(defmethod ->xml ((value t) (dest stp:attribute) (type t)
		  &key &allow-other-keys)
  "Convert VALUE to string and store in DEST."
  (setf (stp:value dest) (->xml value 'string type))
  value)

(defmethod ->xml ((value string) (dest stp:attribute) (type t)
		  &key &allow-other-keys)
  "Fast-path method for storing string VALUE into text node DEST."
  (setf (stp:value dest) value))

(defmethod ->xml ((value string) (dest stp:node) (type (eql 'string))
		  &key &allow-other-keys)
  "Catch-all for STP nodes that do not have an obvious string
interpretation."
  (->xml-conversion-error
   value type dest
   "~@<Cannot store a string into an ~A node; Append /text() to the ~
XPath if you intended to write an element's text.~@:>"
   (type-of dest)))

(defmethod ->xml ((value t) (dest (eql 'string)) (type t)
		  &key &allow-other-keys)
  "Convert VALUE to requested type string by `prin1'ing it."
  (with-standard-io-syntax
    (prin1-to-string value)))

(defmethod ->xml ((value string) (dest (eql 'string)) (type t)
		  &key &allow-other-keys)
  "Fast-path method for string VALUE."
  value)

(defmethod ->xml ((value sequence) (dest (eql 'string)) (type t)
		  &key &allow-other-keys)
  "Convert sequence VALUE to string by `format'ting."
  (with-standard-io-syntax
    (format nil "~{~S~^ ~}" (coerce value 'list))))

(defmethod ->xml ((value list) (dest (eql 'string)) (type (eql 'type))
		  &key &allow-other-keys)
  "Convert VALUE to string by `prin1'ing it."
  (with-standard-io-syntax
    (prin1-to-string value)))


;;; Utility functions
;;

(declaim (ftype (function (string) list) %parse-list-of-atoms))

(defun %parse-list-of-atoms (string)
  (let ((stream     (make-string-input-stream string))
	(eof-marker (gensym)))
    (iter (for value next (read stream nil eof-marker))
	  (until (eq value eof-marker))
	  (collect value))))

(declaim (ftype (function (string) list) %split-at-whitespace))

(defun %split-at-whitespace (string)
  (nth-value 0 (split-sequence:split-sequence-if
		#'(lambda (c) (or (eq c #\Space)
				  (eq c #\Newline)))
		string :remove-empty-subseqs t)))
