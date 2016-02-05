;;;; singleton-location.lisp --- A location that corresponds to a single node.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

(defclass singleton-location (location)
  ((if-no-match         :initarg  :if-no-match
                        :type     if-no-match-policy-designator
                        :initform :error
                        :documentation
                        "Policy for the situation that the XPath
evaluates to an empty node set.")
   (if-multiple-matches :initarg  :if-multiple-matches
                        :type     (member :error :first :last :any)
                        :reader   location-if-multiple-matches
                        :initform :any
                        :documentation
                        "Policy for the situation that the XPath
evaluates to a node set that consists of more than one nodes."))
  (:documentation
   "This location class consists of an XML document along with an
XPath that produces exactly one match in the document."))

(defmethod evaluate! :around ((location singleton-location))
  (let+ (((&slots document path result if-no-match if-multiple-matches) location)
         (result-set (call-next-method)))

    (cond
      ((not (xpath:node-set-p result-set))
       (setf result result-set))

      ;; Apply no-match policy. Note that :create is implemented in a
      ;; mixin class and thus does not have to be handled here.
      ((xpath:node-set-empty-p result-set)
       (ecase if-no-match
         (:error
          (error 'empty-result-set
                 :document   document
                 :path       path
                 :result-set result-set))
         (:do-nothing
          (setf result nil))
         (:create)))

      ;; Exactly one match.
      ((length= 1 (xpath:all-nodes result-set))
       (setf result (xpath:first-node result-set)))

      ;; Apply multiple matches policy
      (t
       (ecase if-multiple-matches
         ((:error
           (error 'too-many-matches-in-result-set
                  :document   document
                  :path       path
                  :result-set result-set
                  :expected   1)))
         ((:first :any)
          (setf result (xpath:first-node result-set)))
         (:last
          (setf result (lastcar (xpath:all-nodes result-set)))))))))

(defmethod name ((location singleton-location)
                 &key
                 prefix?)
  (let ((item (location-result location)))
    (check-type item stp:element "an element and thus does not have a name")
    (if prefix?
        (stp:qualified-name item)
        (stp:local-name item))))

(defmethod (setf name) ((new-value string)
                        (location  singleton-location))
  (let ((item (location-result location)))
    (setf (stp:local-name item) new-value)))

(defmethod (setf name) ((new-value list)
                        (location  singleton-location))
  (let+ (((local-name prefix uri) new-value)
         (item (location-result location)))
    (setf (stp:namespace-uri    item) uri
          (stp:namespace-prefix item) prefix
          (stp:local-name       item) local-name))
  new-value)

(defmethod val ((location singleton-location)
                &key
                (type 'string))
  (xml-> (location-result location) type))

(defmethod (setf val) ((new-value t)
                       (location  singleton-location)
                       &key
                       (type :any))
  (->xml new-value (location-result location) type))

(defmethod @ ((location singleton-location)
              (name     string)
              &key
              (type 'string))
    (xml-> (location-attribute location name) type))

(defmethod (setf @) ((new-value t)
                     (location  singleton-location)
                     (name      string)
                     &key
                     (type 'string))
  (->xml new-value (location-attribute location name) type))

(defmethod loc ((location singleton-location)
                (path     t)
                &rest args
                &key &allow-other-keys)
  (apply #'loc (location-result location) path args))

(defmethod location-attribute :before ((location singleton-location)
                                       (name     string)
                                       &key &allow-other-keys)
  (let ((item (location-result location)))
    (check-type item stp:element "an element node (and thus does not
have attribute children). Did you try to use `@' or `(setf @)' on a
location that already represents an attribute node?")))

(defmethod location-attribute ((location singleton-location)
                               (name     string)
                               &key
                               (uri "")
                               (if-does-not-exist #'error))
  (let ((item (location-result location)))
    (or (stp:find-attribute-named item name uri)
        (cond
          ((null if-does-not-exist)
           nil)
          ((member if-does-not-exist `(error ,#'error))
           (error "No attribute ~S at location ~A" name item))))))
