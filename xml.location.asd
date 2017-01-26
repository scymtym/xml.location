;;;; xml.location.asd --- System definition of the xml.location system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :xml.location
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
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
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests for the xml.location system."
  :depends-on  ((:version :xml.location (:read-file-form "version-string.sexp"))
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
