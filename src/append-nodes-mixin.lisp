;;; append-nodes-mixin.lisp --- Append nodes when location is assigned to.
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
