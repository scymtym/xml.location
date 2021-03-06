;;;; location.lisp --- Standard location class.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

(defclass location ()
  ((document      :initarg  :document
                  :type     stp:node
                  :accessor location-document
                  :documentation
                  "The XML document to which the location refers.")
   (namespaces    :type     list
                  :accessor location-namespaces
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
                  ;; The `singleton-location' subclass stores
                  ;; elements, not `node-set's.
                  ;; :type     (or null xpath:node-set)
                  :reader   location-result
                  :initform nil
                  :documentation
                  "The node-set produced by evaluating the XPath of
the location to the document of the location."))
  (:default-initargs
   :namespaces xpath::*initial-namespaces*)
  (:documentation
   "A location consists of an XML document and an XPath that refers to
nodes in the document. Technically the location uses the node set
resulting from evaluating the XPath on the document as a concrete
representation of this relation. Operation on the location are carried
out on the node or nodes of the node set."))

(defmethod initialize-instance ((instance location)
                                &key
                                namespaces)
  (call-next-method)
  ;; Augment the list of namespaces if necessary. We do this as early
  ;; as possible.
  (setf (slot-value instance 'namespaces)
        (%maybe-add-default-namespaces namespaces)))

(defmethod initialize-instance :after ((instance location)
                                       &key)
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
  "Reset computed result of LOCATION when namespaces are changed."
  (compile! location))

(defmethod (setf location-path) :after ((new-value t)
                                        (location  location))
  "Reset computed result of LOCATION when the path is changed."
  (compile! location))

(defmethod (setf location-compiled-path) :after  ((new-value t)
                                                  (location  location))
  "Reset computed result of LOCATION when the compiled path is
changed."
  (evaluate! location))

(defmethod location-attribute :around ((location location)
                                       (name     string)
                                       &rest args
                                       &key &allow-other-keys)
  "Handle qualified attribute names."
  (let+ (((&values local-name &ign uri)
          (maybe-decode-qname location name)))
    (if uri
        (apply #'call-next-method location local-name :uri uri args)
        (call-next-method))))

(defmethod compile! :before ((location location))
  "Check whether the XPath source is available."
  (unless (slot-value location 'path)
    (error 'missing-xpath-source)))

(defmethod compile! ((location location))
  "Compile the XPath and store the result."
  (with-slots (path namespaces) location
    (setf (location-compiled-path location)
          (xpath:compile-xpath
           path (xpath::make-dynamic-environment namespaces)))))

(defmethod evaluate! ((location location))
  "Evaluate the XPath on the document and store the resulting node
set."
  (let+ (((&slots document path compiled-path result) location)
         (result-candidate
          (xpath:evaluate-compiled compiled-path document)))
    (unless (xpath:node-set-p result-candidate)
      (error 'invalid-result-type
             :document document
             :path     path
             :result   result-candidate))
    (setf result result-candidate)))

(defmethod maybe-decode-qname ((location location)
                               (name     string))
  "If NAME is qualified, decode it into local-name prefix and uri
using the configured namespaces of LOCATION. Return the components as
multiple value. If NAME is not qualified, the secondary and tertiary
values are both nil."
  (if-let ((index (position #\: name)))
    (let+ (((&slots-r/o namespaces) location)
           (env (xpath::make-dynamic-environment namespaces))
           ((&values local-name uri)
            (xpath::decode-qname name env t)))
      (values local-name (subseq name 0 index) uri))
    (values name nil nil)))

(defmethod (setf name) :around ((new-value string)
                                (location  location))
  "Determine whether NEW-VALUE is qualified and call and appropriate
next method."
  (let+ (((&values local-name prefix uri)
          (maybe-decode-qname location new-value)))
    (if uri
        (setf (name location) (list local-name prefix uri))
        (call-next-method))))

(defmethod print-object ((object location) stream)
  (with-slots (document path) object
    (print-unreadable-object (object stream :identity t)
      (format stream "~A ~A in " (class-name (class-of object)) path)
      (stp:serialize
       document
       (cxml:make-character-stream-sink
        stream :omit-xml-declaration-p t)))))

;;; Utility Functions

(defun %maybe-add-default-namespaces (namespaces)
  "Add default namespaces as defined by `xpath::*dynamic-namespaces*'
to NAMESPACES."
    (if (member '&default namespaces)
        (append xpath::*dynamic-namespaces*
                (remove '&default namespaces))
        namespaces))
