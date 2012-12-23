;;; ignore-empty-results.lisp --- Ignore XPath mismatches without errors.
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
