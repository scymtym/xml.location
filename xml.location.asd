;;;; xml.location.asd --- System definition of the xml.location system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:xml.location-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:xml.location-system)

;;; Version stuff

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

(defsystem :xml.location
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "This system provides a convenient interface for
 manipulating XML data. It is inspired by the xmltio library."
  :depends-on  (:alexandria
                :split-sequence
                :iterate
                (:version :let-plus "0.2")
                :more-conditions
                :closer-mop

                :cxml-stp
                :xpath)
  :components  ((:file       "compat"
                 :pathname   "src/compat")

                (:module     "src"
                 :depends-on ("compat")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "conditions")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "xpath-creation")

                              (:file       "location")
                              (:file       "singleton-location")
                              (:file       "multi-location")
                              (:file       "location-mixins")

                              (:file       "conversion")
                              (:file       "construction")

                              (:file       "macros"))))

  :in-order-to ((test-op (test-op :xml.location/test))))

(defsystem :xml.location/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests for the xml.location system."
  :depends-on  ((:version :xml.location #.(version/string))
                (:version :lift         "1.7.1"))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "xpath-creation")

                              (:file       "location")
                              (:file       "multi-location")

                              (:file       "conversion")
                              (:file       "macros")

                              (:file       "location-mixins")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :xml.location/test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
