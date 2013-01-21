;;;; ignore-empty-results.lisp --- Ignore XPath mismatches without errors.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)


;;; Location mixin class `ignore-empty-result-mixin'
;;

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
