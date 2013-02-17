;;;; location-mixins.lisp --- Mixin classes for location classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

;;; `ignore-empty-result-mixin' class

(defclass ignore-empty-result-mixin ()
  ()
  (:documentation
   "This mixin class adds the behavior of turning the methods `name',
`val' and `@' (and their `setf' variants) into no-ops if the result
set is empty."))

(defmethod name :around ((location ignore-empty-result-mixin)
			 &key &allow-other-keys)
  (when (location-result location)
    (call-next-method)))

(defmethod (setf name) :around ((new-value t)
				(location  ignore-empty-result-mixin))
  (when (location-result location)
    (call-next-method)))

(defmethod val :around ((location ignore-empty-result-mixin)
			&key &allow-other-keys)
  (when (location-result location)
    (call-next-method)))

(defmethod (setf val) :around ((new-value t)
			       (location  ignore-empty-result-mixin)
			       &key &allow-other-keys)
  (when (location-result location)
    (call-next-method)))

(defmethod @ :around ((location ignore-empty-result-mixin)
		      (name     t)
		      &key &allow-other-keys)
  (when (location-result location)
    (call-next-method)))

(defmethod (setf @) :around ((new-value t)
			     (location  ignore-empty-result-mixin)
			     (name      t)
			     &key &allow-other-keys)
  (when (location-result location)
    (call-next-method)))

;;; `create-missing-nodes-mixin' class

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
  (let+ (((&accessors-r/o (result location-result)) location)
	 ((&flet do-one (item)
	    ;; If the attribute exists, just use it, otherwise create
	    ;; it.
	    (or (apply #'stp:find-attribute-named item name
		       (when uri (list uri)))
		(let ((attribute
			(apply #'stp:make-attribute ""
			       (if uri (concatenate 'string "foo:" name) name) ;; TODO
			       (when uri (list uri)))))
		  (stp:add-attribute item attribute)
		  attribute)))))
    (typecase result
      (xpath:node-set
       (xpath:map-node-set->list #'do-one result))
      (t
       (do-one result)))))

;;; `append-nodes-mixin' class

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
