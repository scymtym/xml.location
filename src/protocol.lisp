;;; protocol.lisp ---
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


;;; Location Protocol
;;

(defgeneric location-document (location)
  (:documentation
   "Return the document (an stp:node instance) associated to
LOCATION."))

(defgeneric (setf location-document) (new-value location)
  (:documentation
   "Set NEW-VALUE as LOCATION's associated document. NEW-VALUE has to
be an stp:node instance."))

(defgeneric location-path (location)
  (:documentation
   "Return the XPath of LOCATION."))

(defgeneric (setf location-path) (new-value location)
  (:documentation
   "Set NEW-VALUE as LOCATION's associated XPath. NEW-VALUE has to be
a string."))

(defgeneric location-result (location)
  (:documentation
   "Return the node or node set that has been produced by evaluating
LOCATION's associated XPath on LOCATION's associated document."))

(defgeneric name (location
		  &key
		  prefix?)
  (:documentation
   "Return the name of the node represented by LOCATION.
If PREFIX? is non-nil, the concatenation of the prefix and the local
name is returned."))

(defgeneric (setf name) (new-value location)
  (:documentation
   "Set NEW-VALUE as the local name of the node represented by
LOCATION."))

(defgeneric val (location &key type)
  (:documentation
   "Return the value of the node represented by LOCATION. If TYPE is
supplied, a type conversion may be performed. When LOCATION represents
an element node, TYPE has to be supplied. For attribute and text
nodes, the text value is returned in that case."))

(defgeneric (setf val) (new-value location &key type)
  (:documentation
   "Set NEW-VALUE as the value of the node represented by LOCATION. If
TYPE is supplied, a type conversion may be performed prior to
assigning the value. When LOCATION represents an element node, TYPE
has to be supplied. For attribute and text nodes, NEW-VALUE has to be
a string in that case."))

(defgeneric @ (location name &key type)
    (:documentation
     "Return the value of the attribute named NAME of the node
represented by LOCATION. If TYPE is supplied, a type conversion may be
performed. LOCATION has to represent an element node."))

(defgeneric (setf @) (new-value location name &key type)
  (:documentation
   "Set NEW-VALUE as the value of the attribute named NAME of the node
represented by LOCATION. If TYPE is supplied, a type conversion may be
performed prior to assigning the value. LOCATION has to represent an
element node."))


;;; Dynamic Location Class Family
;;

(define-dynamic-class-family location
    "This dynamic class family consists of all location classes. A
location class has the class `location' and a possibly several mixin
classes as its superclasses. Typical location mixin classes are:
+ `singleton-location'
+ `multi-location'
+ `create-missing-nodes-mixin'
+ `ignore-empty-results-mixin'"
  (:common-superclasses (location)))


;;; Location Construction Protocol
;;

(defgeneric loc (document path
		 &rest args
		 &key
		 namespaces
		 if-no-match
		 if-multiple-matches
		 &allow-other-keys)
  (:documentation
   "Construct and return a new `location' instance that represents the
nodes that result from applying the XPath PATH to the XML document
DOCUMENT. ARGS are passed to the `make-instance' call that creates the
`location' instance.

NAMESPACES can be an alist of the form ((PREFIX . NAMESPACE)*) that
specifies XML namespaces that should be available in PATH.

IF-NO-MATCH specifies the policy for dealing with the situation that
the node set produced by evaluating PATH on DOCUMENT is empty. Valid
values are :error, :create and :do-nothing.

IF-MULTIPLE-MATCHES specifies the policy for dealing with the
situation that the node set produced by evaluating PATH on DOCUMENT
consists of multiple nodes. Valid values are :error, :first, :any and
last.

The type of the returned `location' instance can depend on the
arguments but is a sub-type of `location'."))


;;; Conversion Protocol
;;

(defgeneric xml-> (value type &key inner-types)
  (:documentation
   "Convert VALUE to the type designated by TYPE and, possibly
INNER-TYPES. The result of the conversion is returned."))

(defgeneric ->xml (value dest)
  (:documentation
   "Convert VALUE to a suitable type and store the result of the
conversion in the XML node DEST."))
