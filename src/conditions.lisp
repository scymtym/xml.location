;;; conditions.lisp --- Conditions used in the cxml-locations system.
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

(define-condition location-error (error)
  ((document :initarg  :document
	     :type     (or stp:document string)
	     :reader   location-error-document
	     :documentation
	     "The document of the location that caused the error.")
   (path     :initarg  :path
	     :type     string
	     :reader   location-error-path
	     :documentation
	     "The XPath of the location that caused the error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<An error involving the location described by~
document ~A and XPATH ~A occurred.~@:>"
	     (%location-error-document-string (location-error-document condition))
	     (location-error-path condition))))
  (:documentation
   "This condition class can be used to discriminate location-related
errors."))

(define-condition missing-xpath-source (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<XPath source not available - cannot ~
recompile.~@:>")))
  (:documentation
   "This condition is signaled when recompilation of an XPath would be
required but the XPath source is not available. This can happen, for
example, when the namespace table of a location is changed."))


;;; Result-set-related Errors
;;

(define-condition result-set-error-mixin (condition)
  ((result-set :initarg  :result-set
	       :type     xpath:node-set
	       :reader   location-error-result-set
	       :documentation
	       "The invalid result set."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The result set for XPath ~S on ~S is invalid~@:>"
	     (location-error-path condition)
	     (%location-error-document-string condition))))
  (:documentation
   "This error is signaled when an XPath evaluation produces a result
set that is invalid in its context."))

(define-condition empty-result-set (location-error
				    result-set-error-mixin)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The result set for XPath ~S on ~S is empty, ~
but exactly one match is required~@:>"
	     (location-error-path condition)
	     (%location-error-document-string condition))))
  (:documentation
   "This error is signaled when an XPath evaluation produces an empty
result in a context that requires a non-empty result set."))

(define-condition too-many-matches-in-result-set (location-error
						  result-set-error-mixin)
  ((expected :initarg  :expected
	     :type     non-negative-integer
	     :reader   location-error-expected
	     :documentation
	     "The number of elements the result set should have
had."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Too many matches (~A) in result set for XPatch ~
~S on ~S; exactly ~A match~:*~P is required~@:>"
	     (length (xpath:all-nodes (location-error-result-set condition)))
	     (location-error-path condition)
	     (%location-error-document-string condition)
	     (location-error-expected condition))))
  (:documentation
   "This error is signaled when an XPath evaluation produces a result
set that consists of multiple elements in a context that permits at
most one element."))


;;; `with-locations'-related Errors
;;

(define-condition invalid-binding-form (error)
  ((form :initarg  :form
	 :reader   invalid-binding-form-form
	 :documentation
	 "The invalid binding form."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The binding form ~A is invalid.~@:>"
	     (invalid-binding-form-form condition))))
  (:documentation
   "This error is signaled when an invalid binding form is encountered
during expansion of the `with-locations' macro."))

(define-condition no-such-accessor-form (invalid-binding-form)
  ((spec :initarg  :spec
	 :reader   invalid-binding-form-spec
	 :documentation
	 "The accessor specification which contains the unknown
accessor.")
   (name :initarg  :name
	 :reader   invalid-binding-form-name
	 :documentation
	 "The unknown accessor."))
  (:report
   (lambda (condition stream)
     (format stream "~@<When expanding the binding form ~A containing ~
the accessor specification ~A, the specified accessor ~S is not ~
known.~@:>"
	     (invalid-binding-form-form condition)
	     (invalid-binding-form-spec condition)
	     (invalid-binding-form-name condition))))
  (:documentation
   "This error is signaled if a binding form is encountered within a
use of in the `with-locations' macro which contains an unknown
accessor."))


;;; XPath-based Creation Errors
;;

(define-condition xpath-creation-error (simple-error)
  ((location  :initarg  :location
	      :reader   xpath-creation-error-location
	      :documentation
	      "The location at which a node should have been added
according to the XPath.")
   (type      :initarg  :type
	      :reader   xpath-creation-error-type
	      :documentation
	      "The type of the XPath fragment for which the creation
of a node failed.")
   (name      :initarg  :name
	      :reader   xpath-creation-error-name
	      :documentation
	      "The name mentioned in the XPath fragment for which the
creation of a node failed.")
   (predicate :initarg  :predicate
	      :reader   xpath-creation-error-predicate
	      :documentation
	      "The predicate of the XPath fragment for which the
creation of a node failed."))
  (:default-initargs :format-control   ""
		     :format-arguments nil)
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not create node for XPath fragment (~S ~
~S ~S) at location ~A~@:>"
	     (xpath-creation-error-type      condition)
	     (xpath-creation-error-name      condition)
	     (xpath-creation-error-predicate condition)
	     (xpath-creation-error-location  condition))
     (%maybe-add-explanation condition stream)))
  (:documentation
   "This error is signaled when the creation of a node based on a
XPath fragment fails."))


;;; Conversion Errors
;;

(define-condition conversion-error (simple-error)
  ((value :initarg  :value
	  :type     t
	  :reader   conversion-error-value
	  :documentation
	  "The value for which the conversion failed.")
   (type  :initarg  :type
	  :type     t
	  :reader   conversion-error-type
	  :documentation
	  "The type involved in the failed conversion."))
  (:default-initargs
   :format-control   ""
   :format-arguments nil)
  (:report
   (lambda (condition stream)
     (format stream "~@<Conversion failed for value ~S and type ~S~@:>"
	     (conversion-error-value condition)
	     (conversion-error-type  condition))))
  (:documentation
   "This error is signaled when a conversion fails."))

(define-condition no-conversion-method-mixin (condition)
  ((function :initarg    :function
	     :type       symbol
	     :allocation :class
	     :reader     conversion-error-function
	     :documentation
	     "The name of the conversion function for which no
suitable method could be found."))
  (:documentation
   "This condition class can be mixed into condition classes that
indicate a conversion failure because of a missing conversion
method."))

(define-condition xml->-conversion-error (conversion-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The XML node ~S could not be converted into a ~
Lisp object with type ~S~@:>"
	     (conversion-error-value condition)
	     (conversion-error-type  condition))
     (%maybe-add-explanation condition stream)))
  (:documentation
   "This error is signaled when converting an XML location into a Lisp
object with a certain type fails."))

(defun xml->-conversion-error (value type &optional format-control
			       &rest format-arguments)
  "Signal an `xml->-conversion-error' with data VALUE and TYPE and
optional FORMAT-CONTROL and FORMAT-ARGUMENTS."
  (error 'xml->-conversion-error
	 :value            value
	 :type             type
	 :format-control   format-control
	 :format-arguments format-arguments))

(define-condition no-xml->-conversion-method (xml->-conversion-error
					      no-conversion-method-mixin)
  ((function :initform 'xml->))
  (:report
   (lambda (condition stream)
     (format stream "~@<There is no method on ~S to convert the XML ~
node ~S into a Lisp object with type ~S.~@:>"
	     (conversion-error-function condition)
	     (conversion-error-value    condition)
	     (conversion-error-type     condition))
     (%add-available-conversion-methods condition stream)))
  (:documentation
   "This error is signaled when no method is available to convert an
XML location into a Lisp object with a certain type."))

(define-condition ->xml-conversion-error (conversion-error)
  ((destination :initarg  :destination
		:type     t
		:reader   conversion-error-destination
		:documentation
		"The destination of the failed conversion. Usually an
XML node."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The value ~S could not be stored in the ~
destination ~S with type ~S~@:>"
	     (conversion-error-value       condition)
	     (conversion-error-destination condition)
	     (conversion-error-type        condition))
     (%maybe-add-explanation condition stream)))
  (:documentation
   "This error is signaled when storing a value into an XML location
with a certain type fails."))

(defun ->xml-conversion-error (value type destination
			       &optional format-control
			       &rest format-arguments)
  "Signal an `->xml-conversion-error' with data VALUE, TYPE and
DESTINATION and optional FORMAT-CONTROL and FORMAT-ARGUMENTS."
  (error '->xml-conversion-error
	 :value            value
	 :type             type
	 :destination      destination
	 :format-control   format-control
	 :format-arguments format-arguments))

(define-condition no-->xml-conversion-method (->xml-conversion-error
					      no-conversion-method-mixin)
  ((function :initform '->xml))
  (:report
   (lambda (condition stream)
     (format stream "~@<There is no method on ~S to store the value ~S ~
in the destination ~S with type ~S.~@:>"
	     (conversion-error-function    condition)
	     (conversion-error-value       condition)
	     (conversion-error-destination condition)
	     (conversion-error-type        condition))
     (%add-available-conversion-methods condition stream)))
  (:documentation
   "This error is signaled when no method is available to store a
value into an XML location with a certain type."))


;;; Utility Functions
;;

(defun %location-error-document-string (condition)
  "Return a serialization of the XML document associated with
CONDITION."
  (with-output-to-string (stream)
    (stp:serialize
     (location-error-document condition)
     (cxml:make-character-stream-sink stream))))

(defun %maybe-add-explanation (condition stream)
  "If there is one, format the message contained in the
`simple-condition' CONDITION on STREAM. Otherwise just add a colon."
  (let ((control   (simple-condition-format-control   condition))
	(arguments (simple-condition-format-arguments condition)))
    (if (and (not (emptyp control)) arguments)
	(progn
	  (format stream ": ~%")
	  (apply #'format stream control arguments))
	(write-char #\. stream))))

(defun %add-available-conversion-methods (condition stream)
  "Print a list of methods of the generic function associated to
CONDITION onto STREAM."
  (%add-available-conversion-methods-for-function
   (slot-value condition 'function)
   stream))

(defun %add-available-conversion-methods-for-function (function stream)
  "Format the list of methods of the generic function designated by
NAME onto STREAM. "
  (let* ((methods        (closer-mop:generic-function-methods
			  (symbol-function function)))
	 (specializers   (map 'list #'closer-mop:method-specializers
			      methods))
	 (*print-circle* nil)) ;; make the method list more regular
    (terpri stream)
    (format stream "~@<Available conversion methods:~_~{+ (~{~A~^ ~})~_~}~@:>"
	    specializers)))
