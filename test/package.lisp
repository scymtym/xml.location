;;; package.lisp --- Package Definition for Unit Tests of the xml.location System.
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

(cl:defpackage #:xml.location.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:lift

   #:xml.location)

  (:export
   #:root)

  (:documentation
   "This package contains unit tests for the xml.location system."))

(cl:in-package #:xml.location.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the xml.location system."))


;;;
;;

(defclass mock-domain-object ()
  ((content :initarg  :content
	    :type     real
	    :accessor mock-domain-object-content)))

(defmethod print-object ((object mock-domain-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (mock-domain-object-content object))))

(defmethod xml-> ((value stp:element)
		  (type  mock-domain-object)
		  &key &allow-other-keys)
  "Store data in VALUE into the `mock-domain-object' instance TYPE."
  (with-locations-r/o (((:@ content :type 'real) ".")) value
    (setf (mock-domain-object-content type) content)
    type))

(defmethod ->xml ((value mock-domain-object)
		  (dest  stp:element)
		  (type  t)
		  &key &allow-other-keys)
  "Store the `mock-domain-object' VALUE into DEST."
  (with-locations (((:@ content :type 'real) ".")) dest
    (setf content (mock-domain-object-content value)))
  value)
