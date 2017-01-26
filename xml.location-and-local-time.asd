;;;; xml.location-and-local-time.asd --- System definition for xml-location and local-time interop.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    (load (merge-pathnames "xml.location.asd" *load-truename*))
    (values))

(defsystem :xml.location-and-local-time
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "To and from XML conversion for local-time timestamps."
  :depends-on  ((:version :xml.location (:read-file-form "version-string.sexp"))
                :local-time)
  :components  ((:file       "local-time"
                 :pathname   "src/local-time")))
