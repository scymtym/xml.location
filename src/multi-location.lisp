;;;; multi-location.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

;;; Multi Location

(defclass multi-location (location)
  ()
  (:documentation
   "Instances of this class represent and operate on a set of XPath
matches in a single document simultaneously."))

(defmethod name ((location multi-location)
                 &key
                 prefix?)
  (flet ((get-name (item)
           (check-type item stp:element "an element and thus does not
have a name")
           (if prefix?
               (stp:qualified-name item)
               (stp:local-name item))))
    (xpath:map-node-set->list #'get-name (location-result location))))

(defmethod (setf name) ((new-value string)
                        (location  multi-location))
  (xpath:map-node-set->list
   #'(lambda (item) (setf (stp:local-name item) new-value))
   (location-result location))
  new-value)

(defmethod (setf name) ((new-value list)
                        (location  multi-location))
  (let+ (((local-name prefix uri) new-value))
    (xpath:map-node-set->list
     #'(lambda (item)
         (setf (stp:namespace-uri    item) uri
               (stp:namespace-prefix item) prefix
               (stp:local-name       item) local-name))
     (location-result location)))
  new-value)

(defmethod val ((location multi-location)
                &key
                (type 'string))
  (xpath:map-node-set->list
   (rcurry #'xml-> type) (location-result location)))

(defmethod (setf val) ((new-value t)
                       (location  multi-location)
                       &key
                       (type :any))
  (xpath:map-node-set->list
   #'(lambda (node) (->xml new-value node type))
   (location-result location)))

(defmethod (setf val) ((new-value list)
                       (location  multi-location)
                       &key
                       (type :any))
  (let* ((xpath::*dynamic-namespaces* (slot-value location 'namespaces))
         (nodes      (xpath:all-nodes (location-result location)))
         (old-length (length nodes))
         (new-length (length new-value)))
    (iter (generate node in nodes)
          (generate value in new-value)
          (for i :from 0 :below (max old-length new-length))
          (cond
            ;; i-th child is beyond new number of children => delete
            ;; it
            ((>= i new-length)
             (let ((node (next node)))
               (stp:delete-child node (stp:parent node))))

            ;; i-th child did exist previously => just store the new
            ;; value.
            ((< i old-length)
             (collect (->xml (next value) (next node) type)))

            ;; i-th child did not exist previously => create it and
            ;; store the value.
            (t
             (let ((new-node (first (create-xpath-sibling
                                     (location-document location)
                                     (location-path location)))))
               (collect (->xml (next value) new-node type))))))))

(defmethod @ ((location multi-location)
              (name     string)
              &key
              (type 'string))
  (map 'list (rcurry #'xml-> type)
       (location-attribute location name)))

(defmethod (setf @) ((new-value t)
                     (location  multi-location)
                     (name      string)
                     &key
                     (type 'string))
  (map 'list #'(lambda (attr) (->xml new-value attr type))
       (location-attribute location name)))

(defmethod loc ((location multi-location)
                (path     t)
                &rest args
                &key &allow-other-keys)
  (xpath:map-node-set->list
   #'(lambda (node) (apply #'loc node path args))
   (location-result location)))

(defmethod location-attribute ((location multi-location)
                               (name     string)
                               &key
                               uri
                               (if-does-not-exist #'error))
  (flet ((find-attribute (item)
           (check-type item stp:element "an element node (and thus
does not have attribute children). Did you try to use `@' or `(setf
@)' on a location that already represents an attribute node?")
           (or (apply #'stp:find-attribute-named item name
                      (when uri `(,uri)))
               (cond
                 ((null if-does-not-exist)
                  nil)
                 ((member if-does-not-exist `(error ,#'error))
                  (error "No attribute ~S at location ~A" name item))))))
    (xpath:map-node-set->list #'find-attribute
                              (location-result location))))
