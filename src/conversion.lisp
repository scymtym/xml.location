;;; conversion.lisp --- To and from XML conversions for some data types.
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


;;; XML -> * Conversions
;;

(defmethod xml-> :around ((value t) (type (eql nil))
			  &key &allow-other-keys)
  (error "NIL is not valid as a type specification."))

(defmethod xml-> :around ((value t) (type list)
			  &key &allow-other-keys)
  "Split composite type specification TYPE into head and tail."
  (if (length= 1 type)
      (xml-> value (first type))
      (xml-> value (first type) :inner-types (rest type))))

(defmethod xml-> ((value stp:text) (type t)
		  &rest args
		  &key &allow-other-keys)
  "Extract text from text node VALUE for further conversion."
  (apply #'xml-> (stp:data value) type args))

(defmethod xml-> ((value stp:text) (type (eql 'string))
		  &rest args
		  &key &allow-other-keys)
  "Fast-path method for text nodes if TYPE is string."
  (declare (ignore args))
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
  (error "Cannot extract string from ~A node; Append /text() to XPath ~
if you intended to extract an element's text."
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
  (let ((chunks (split-sequence:split-sequence
		 #\Space value :remove-empty-subseqs t)))
    (cond
      ((not inner-types-supplied?)
       chunks)
      ((length= 1 inner-types)
       (let ((type (first inner-types)))
	 (map 'list (rcurry #'xml-> type) chunks)))
      (t
       (map 'list (rcurry #'xml-> inner-types) chunks)))))


;;; * -> XML Conversions
;;

(defmethod ->xml ((value t) (dest stp:text))
  "Convert VALUE to string and store in DEST."
  (setf (stp:data dest) (->xml value 'string)))

(defmethod ->xml ((value string) (dest stp:text))
  "Fast-path method for storing string VALUE into text node DEST."
  (setf (stp:data dest) value))

(defmethod ->xml ((value t) (dest stp:attribute))
  "Convert VALUE to string and store in DEST."
  (setf (stp:value dest) (->xml value 'string)))

(defmethod ->xml ((value string) (dest stp:attribute))
  "Fast-path method for storing string VALUE into text node DEST."
  (setf (stp:value dest) value))

(defmethod ->xml ((value string) (dest stp:node))
  "Catch-all for STP nodes that no obvious string interpretation."
  (error "Cannot store string into ~A node; Append /text() to XPath if ~
you intended to write an element's text."
	 (type-of dest)))

(defmethod ->xml ((value t) (dest (eql 'string)))
  "Convert VALUE to requested type string by `prin1'ing it."
  (prin1-to-string value))

(defmethod ->xml ((value string) (dest (eql 'string)))
  "Fast-path method for string VALUE."
  value)

(defmethod ->xml ((value sequence) (dest (eql 'string)))
  "Convert sequence VALUE to string by `format'ting."
  (format nil "~{~A~^ ~}" (coerce value 'list)))
