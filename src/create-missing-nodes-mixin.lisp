;;; create-missing-nodes-mixin.lisp --- Create nodes mentioned in XPath.
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

(cl:in-package #:xml.location)

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
