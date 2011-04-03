;;; multi-location.lisp ---
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


;;; Multi Location
;;

(defclass multi-location (location)
  ()
  (:documentation
   "Instances of this class represent and operate on a set of XPath
matches in a single document simultaneously."))

(defmethod name ((location multi-location)
		 &key
		 prefix?)
  (flet ((get-name (item)
	   (check-type item stp:element "an element and thus does not
have a name")
	   (if prefix?
	       (stp:qualified-name item)
	       (stp:local-name item))))
    (xpath:map-node-set->list #'get-name (location-result location))))

(defmethod (setf name) ((new-value string)
			(location  multi-location))
  (xpath:map-node-set->list
   #'(lambda (item) (setf (stp:local-name item) new-value))
   (location-result location))
  new-value)

(defmethod (setf name) ((new-value list)
			(location  multi-location))
  (bind (((local-name prefix uri) new-value))
    (xpath:map-node-set->list
     #'(lambda (item)
	 (setf (stp:namespace-uri    item) uri
	       (stp:namespace-prefix item) prefix
	       (stp:local-name       item) local-name))
     (location-result location)))
  new-value)

(defmethod val ((location multi-location)
		&key
		(type 'string))
  (xpath:map-node-set->list
   (rcurry #'xml-> type) (location-result location)))

(defmethod (setf val) ((new-value t)
		       (location  multi-location)
		       &key
		       (type :any))
  (xpath:map-node-set->list
   #'(lambda (node) (->xml new-value node type))
   (location-result location)))

(defmethod @ ((location multi-location)
	      (name     string)
	      &key
	      (type 'string))
  (map 'list (rcurry #'xml-> type)
       (location-attribute location name)))

(defmethod (setf @) ((new-value t)
		     (location  multi-location)
		     (name      string)
		     &key
		     (type 'string))
  (map 'list #'(lambda (attr) (->xml new-value attr type))
       (location-attribute location name)))

(defmethod location-attribute ((location multi-location)
			       (name     string)
			       &key
			       uri)
  (flet ((find-attribute (item)
	   (check-type item stp:element "an element node (and thus
does not have attribute children). Did you try to use `@' or `(setf
@)' on a location that already represents an attribute node?")
	   (or (apply #'stp:find-attribute-named item name
		      (when uri `(,uri)))
	       (error "No attribute ~S at location ~A" name item))))
    (xpath:map-node-set->list #'find-attribute
			      (location-result location))))
