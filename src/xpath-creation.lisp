;;; xpath-creation.lisp --- XPath creation functions.
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


;;; XPath creation protocol
;;

(defgeneric create-xpath (document path)
  (:documentation
   "Ensure that the nodes referenced in PATH actually exist in
DOCUMENT, creating them if necessary."))

(defgeneric %create-xpath-element (location type name predicate)
  (:documentation
   "Create the XPath element designated by TYPE, NAME and PREDICATE in
LOCATION."))


;;; Implementation
;;

(defmethod create-xpath ((document stp:node) (path string))
  "Parse XPath and process parsed representation."
  (create-xpath document (xpath:parse-xpath path)))

(defmethod create-xpath ((document stp:node) (path list))
  "Walk along XPath PATH in DOCUMENT, creating missing nodes as they
are encountered. "
  (labels
      ((one-step (location step &rest steps)
	 (let* ((result (xpath:evaluate
			 `(xpath:xpath (:path ,step)) location))
		(nodes  (if (xpath:node-set-empty-p result)
			    (apply #'%create-xpath-element
				   location (%expand-xpath-element step))
			    (xpath:all-nodes result))))
	   (if steps
	       (mapcan #'(lambda (n) (apply #'one-step n steps)) nodes)
	       nodes))))
    (apply #'one-step document (rest path))))

(defmethod %create-xpath-element ((location  t)
				  (type      t)
				  (name      t)
				  (predicate t))
  (error 'xpath-creation-error
	 :location  location
	 :type      type
	 :name      name
	 :predicate predicate))

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
	     :format-control   "~@<Invalid marker in qualified name ~
component list: ~S.~@:>"
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

(declaim (ftype (function (list) (list-of-length 3))
		%expand-xpath-element)
	 (inline %expand-xpath-element))

(defun %expand-xpath-element (spec)
  "When necessary, pad the list SPEC with nil elements such that its
length becomes 3."
  (let ((missing (- 3 (length spec))))
    (if (zerop missing)
	spec
	(append spec (make-list missing)))))
