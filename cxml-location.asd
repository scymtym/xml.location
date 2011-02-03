;;; cxml-location.asd --- System definition of the cxml-location system.
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

(defpackage :cxml-location-system
  (:use
   :cl
  :asdf))

(in-package :cxml-location-system)

(defsystem :cxml-location
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3"
  :description "This system provides a convenient interface for
 manipulating XML data. It is inspired by the xmltio library."
  :depends-on  (:iterate
		:metabang-bind
		:split-sequence
		:cxml-stp
		:xpath)
  :components  ((:module "src"
		 :components ((:file "package")
			      (:file "conditions"
			       :depends-on ("package"))
			      (:file "protocol"
			       :depends-on ("package"))
			      (:file "location"
			       :depends-on ("package" "conditions"
					    "protocol"))
			      (:file "singleton-location"
			       :depends-on ("package" "location"))
			      (:file "multi-location"
			       :depends-on ("package" "location"))
			      (:file "conversion"
			       :depends-on ("package" "protocol"))
			      (:file "construction"
			       :depends-on ("package" "conditions"
					    "protocol" "location"
					    "singleton-location"
					    "multi-location")))))
  :in-order-to ((test-op (test-op :cxml-location-test))))

(defsystem :cxml-location-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3"
  :description "Unit tests for the cxml-location system."
  :depends-on  (:cxml-location
		:lift)
  :components  ((:module "test"
		 :components ((:file "package")
			      (:file "location"
			       :depends-on ("package")))))
  :in-order-to ((test-op (load-op :cxml-location-test))))

(defmethod perform ((this test-op) (component (eql (find-system :cxml-location-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
