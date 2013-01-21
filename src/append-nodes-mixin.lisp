;;;; append-nodes-mixin.lisp --- Append nodes when location is assigned to.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

(defclass append-nodes-mixin ()
  ()
  (:documentation
   "This mixin changes the default replacing assignment behavior to an
appending assignment behavior. That is, assignments to the `val' place
of locations create siblings of the node designated by the final XPath
component and assign values to these new nodes."))

(defmethod (setf val) ((new-value t)
		       (location  append-nodes-mixin)
		       &key
		       (type :any))
  (let ((nodes (create-xpath-sibling (location-document location)
				     (location-path     location))))
    (iter (for node in nodes)
	  (->xml new-value node type)))
  new-value)

(defmethod (setf val) ((new-value list)
		       (location  append-nodes-mixin)
		       &key
		       (type :any))
  (iter (for value in new-value)
	(let ((nodes (create-xpath-sibling
		      (location-document location)
		      (location-path     location))))
	  (iter (for node in nodes)
		(->xml value node type))))
  new-value)

(defmethod (setf val) :after ((new-value t)
			      (location  append-nodes-mixin)
			      &key &allow-other-keys)
  "Sibling creation invalidates our previous XPath evaluation result,
so we have to re-evaluate."
  ;; TODO this is super inefficient; maybe we have to switch to lazy
  ;; evaluation after all?
  (evaluate! location))

(defmethod (setf @) ((new-value list)
		     (location  append-nodes-mixin)
		     (name      string)
		     &key
		     (type 'string))
  (iter (for value in new-value)
	(setf (@ location name :type type) value))
  new-value)

(defmethod location-attribute ((location append-nodes-mixin)
			       (name     string)
			       &key
			       uri)
  (let+ ((nodes (create-xpath-sibling (location-document location)
				      (location-path     location)))
	 (uri?  (and uri (not (emptyp uri))))
	 ((&flet do-one (item)
	    (let ((attribute
		    (apply #'stp:make-attribute ""
			   (if uri? (concatenate 'string "foo:" name) name)
			   (when uri? (list uri)))))
	      (stp:add-attribute item attribute)
	      attribute))))
    (mapcar #'do-one nodes)))
