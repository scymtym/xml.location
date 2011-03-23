;;; create-missing-nodes-mixin.lisp --- Create nodes mentioned in XPath.
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

(defclass create-missing-nodes-mixin ()
  ()
  (:documentation
   "This class adds the automatic creation of XML nodes that are
references in the path but missing in the document to location
classes."))

(defmethod initialize-instance ((instance create-missing-nodes-mixin)
				&key)
  (call-next-method)
  ;; After the instance has been constructed, immediately check
  ;; whether nodes have to be created in the document.
  (let ((xpath::*dynamic-namespaces* (slot-value instance 'namespaces)))
    (create-xpath (location-document instance)
		  (location-path     instance))))

(defmethod location-attribute ((location create-missing-nodes-mixin)
			       (name     string)
			       &key
			       uri)
  ;; When the requested attribute NAME is missing, create it with an
  ;; empty value.
  (let ((item (location-result location)))
    (or (apply #'stp:find-attribute-named item name
	       (when uri `(,uri)))
	(let ((attribute
	       (apply #'stp:make-attribute ""
		      (if uri (concatenate 'string "foo:" name) name)
		      (when uri `(,uri)))))
	  (stp:add-attribute item attribute)
	  attribute))))


;;; XPath Creation Functions
;;

(defgeneric create-xpath (document path)
  (:documentation
   "Ensure that the nodes referenced in PATH actually exist in
DOCUMENT, creating them if necessary."))

(defgeneric %create-xpath-element (location type name predicate)
  (:documentation
   "Create the XPath element designated by TYPE, NAME and PREDICATE in
LOCATION."))

(defmethod create-xpath ((document stp:node) (path string))
  "Parse XPath and process parsed representation."
  (create-xpath document (xpath:parse-xpath path)))

(defmethod create-xpath ((document stp:node) (path list))
  "Walk along XPath PATH in DOCUMENT, creating missing nodes as they
are encountered. "
  (bind ((steps (iter (for e in (rest path))
		      (collect `(xpath:xpath (:path ,e)))))
	 ((:flet path-guts (expr))
	  (lastcar (cdadr expr)))
	 ((:labels one-step (location step &rest steps))
	  (let* ((result (xpath:evaluate step location))
		 (nodes  (if (xpath:node-set-empty-p result)
			     (let ((spec (path-guts step)))
			       (apply #'%create-xpath-element
				      location (%expand-xpath-element spec)))
			     (xpath:all-nodes result))))
	    (when steps
	      (map nil #'(lambda (n) (apply #'one-step n steps)) nodes)))))

    (apply #'one-step document steps)))

(defmethod %create-xpath-element ((location  t)
				  (type      t)
				  (name      t)
				  (predicate t))
  (error 'xpath-creation-error
	 :location         location
	 :type             type
	 :name             name
	 :predicate        predicate))

(defmethod %create-xpath-element ((location  stp:element)
				  (type      (eql :child))
				  (name      (eql :node))
				  (predicate t))
  (%create-xpath-element location type "somenode" predicate))

(defmethod %create-xpath-element ((location  stp:element)
				  (type      (eql :child))
				  (name      (eql '*))
				  (predicate t))
  (%create-xpath-element location type "somenode" predicate))

(defmethod %create-xpath-element ((location  stp:element)
				  (type      (eql :child))
				  (name      (eql :text))
				  (predicate (eql nil)))
  (let ((child (stp:make-text "text")))
    (stp:append-child location child)
    (list child)))

(defmethod %create-xpath-element :around ((location  stp:element)
					  (type      (eql :child))
					  (name      t)
					  (predicate integer))
  (when (< (stp:number-of-children location) predicate)
    (error 'xpath-creation-error
	   :location         location
	   :type             type
	   :name             name
	   :predicate        predicate
	   :format-control   "~@<Cannot create child node at position ~A.~@:>"
	   :format-arguments `(,predicate)))
  (%create-xpath-element location type name nil))

(defmethod %create-xpath-element ((location  stp:element)
				  (type      (eql :child))
				  (name      string)
				  (predicate (eql nil)))
  (let ((child (stp:make-element name)))
    (stp:append-child location child)
    (list child)))

(defmethod %create-xpath-element ((location  stp:element)
				  (type      (eql :child))
				  (name      list)
				  (predicate (eql nil)))
  (bind (((marker prefix local-name) name))
    (unless (eq marker :qname)
      (error 'xpath-creation-error
	     :location         location
	     :type             type
	     :name             name
	     :predicate        predicate
	     :format-control   "~@<Invalid marker in qualified name component list: ~S.~@:>"
	     :format-arguments `(,marker)))
    (let ((child (stp:make-element
		  (concatenate 'string prefix ":" local-name)
		  (xpath::find-dynamic-namespace prefix))))
      (stp:append-child location child)
      (list child))))

(defmethod %create-xpath-element ((location  stp:element)
				  (type      (eql :attribute))
				  (name      string)
				  (predicate (eql nil)))
  "Create an attribute node at LOCATION."
  (let ((attribute (stp:make-attribute "" name)))
    (stp:add-attribute location attribute)
    (list attribute)))


;;; Utility functions
;;

(declaim (ftype (function (list) (cons t (cons t (cons t null))))
		%expand-xpath-element)
	 (inline %expand-xpath-element))

(defun %expand-xpath-element (spec)
  "When necessary, pad the list SPEC with nil elements such that its
length becomes 3."
  (let ((missing (- 3 (length spec))))
    (if (zerop missing)
	spec
	(append spec (make-list missing)))))
