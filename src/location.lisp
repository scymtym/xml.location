;;; location.lisp ---
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


;;;
;;

(defclass location ()
  ((document      :initarg  :document
		  :type     stp:node
		  :accessor location-document
		  :documentation
		  "The XML document to which the location refers.")
   (namespaces    :initarg  :namespaces
		  :type     list
		  :accessor location-namespaces
		  :initform xpath::*initial-namespaces*
		  :documentation
		  "An alist of namespaces that should be available in
the XPath of the location.")
   (path          :initarg  :path
		  :type     (or null string)
		  :accessor location-path
		  :documentation
		  "An XPath that selects nodes in the document of the
location.")
   (compiled-path :initarg  :compiled-path
		  :type     (or null function)
		  :accessor location-compiled-path
		  :initform nil
		  :documentation
		  "Compiled version of the XPath of the location.")
   (result        :initarg  :result
		  :type     (or null xpath:node-set)
		  :reader   location-result
		  :initform nil
		  :documentation
		  "The node-set produced by evaluating the XPath of
the location to the document of the location."))
  (:documentation
   "A location consists of an XML document and an XPath that refers to
nodes in the document. Technically the location uses the node set
resulting from evaluating the XPath on the document as a concrete
representation of this relation. Operation on the location are carried
out on the node or nodes of the node set."))

(defmethod initialize-instance :after ((instance location)
				       &key)
  ;; Augment the list of namespaces if necessary.
  (%maybe-add-default-namespaces instance)
  ;; Compile and evaluate the XPath (compilation is not necessary if a
  ;; compiled XPath has been provided).
  (unless (slot-value instance 'compiled-path)
    (compile! instance))
  (evaluate! instance))

(defmethod (setf location-document) :after ((new-value t)
					    (location  location))
  "Reset computed result of LOCATION when the document is changed."
  (evaluate! location))

(defmethod (setf location-namespaces) :after ((new-value t)
					      (location  location))
  "Reset computed result of LOCATION when the path is changed."
  (compile! location))

(defmethod (setf location-path) :after ((new-value t)
					(location  location))
  "Reset computed result of LOCATION when the path is changed."
  (compile! location))

(defmethod (setf location-compiled-path) :after  ((new-value t)
						  (location  location))
  "Reset computed result of LOCATION when the path is changed."
  (evaluate! location))

(defmethod compile! :before ((location location))
  "Check whether the XPath source is available."
  (unless (slot-value location 'path)
    (error "XPath source not available - Cannot recompile")))

(defmethod compile! ((location location))
  "Compile the XPath and store the result."
  (with-slots (path namespaces) location
    (setf (location-compiled-path location)
	  (xpath:compile-xpath
	   path (xpath::make-dynamic-environment namespaces)))))

(defmethod evaluate! ((location location))
  "Evaluate the XPath on the document and store the resulting node
set."
  (with-slots (document compiled-path result) location
    (setf result (xpath:evaluate-compiled compiled-path document))))

(defmethod print-object ((object location) stream)
  (with-slots (document path) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A in " path)
      (stp:serialize
       document
       (cxml:make-character-stream-sink
	stream :omit-xml-declaration-p t)))))


;;; Utility Functions
;;

(defun %maybe-add-default-namespaces (location)
  "Add default namespaces as defined by `xpath::*dynamic-namespaces*'
to the slot-value of slot namespaces in LOCATION."
  (with-slots (namespaces) location
    (when (member '&default namespaces)
      (setf namespaces
	    (append xpath::*dynamic-namespaces*
		    (remove '&default namespaces))))))
