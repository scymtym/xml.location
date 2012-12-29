;;; xml.location.asd --- System definition of the xml.location system.
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

(cl:defpackage #:xml.location-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:xml.location-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 2
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :xml.location
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides a convenient interface for
 manipulating XML data. It is inspired by the xmltio library."
  :depends-on  (:alexandria
		:split-sequence
		:iterate
		:let-plus
		:closer-mop

		:cxml-stp
		:xpath)
  :components  ((:file       "compat"
		 :pathname   "src/compat")

		(:module     "src"
		 :depends-on ("compat")
		 :components ((:file       "package")
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "variables"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "xpath-creation"
			       :depends-on ("package" "types"))

			      (:file       "location"
			       :depends-on ("package" "conditions"
					    "protocol"))
			      (:file       "singleton-location"
			       :depends-on ("package" "types"
					    "location"))
			      (:file       "multi-location"
			       :depends-on ("package" "location"))
			      (:file       "create-missing-nodes-mixin"
			       :depends-on ("package" "xpath-creation"
					    "location"))
			      (:file       "ignore-empty-results-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "append-nodes-mixin"
			       :depends-on ("package" "xpath-creation"
					    "protocol"))

			      (:file       "conversion"
			       :depends-on ("package" "protocol"))
			      (:file       "construction"
			       :depends-on ("package" "types"
					    "conditions" "protocol"
					    "location"
					    "singleton-location"
					    "multi-location"
					    "create-missing-nodes-mixin"
					    "ignore-empty-results-mixin"))
			      (:file       "macros"
			       :depends-on ("package" "protocol")))))

  :in-order-to ((test-op (test-op :xml.location-test))))

(defsystem :xml.location-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for the xml.location system."
  :depends-on  ((:version :xml.location #.(version/string))
		(:version :lift         "1.7.1"))
  :components  ((:module     "test"
		 :components ((:file       "package")

			      (:file       "location"
			       :depends-on ("package"))
			      (:file       "multi-location"
			       :depends-on ("package"))

			      (:file       "conversion"
			       :depends-on ("package"))
			      (:file       "macros"
			       :depends-on ("package"))

			      (:file       "create-missing-nodes-mixin"
			       :depends-on ("package"))
			      (:file       "ignore-empty-results-mixin"
			       :depends-on ("package"))
			      (:file       "append-nodes-mixin"
			       :depends-on ("package"))))))

(defmethod perform ((this test-op) (component (eql (find-system :xml.location-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
