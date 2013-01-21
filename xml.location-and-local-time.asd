;;;; xml.location-and-local-time.asd --- System definition for xml-location and local-time interop.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:xml.location-and-local-time-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:xml.location-and-local-time-system)

#.(progn
    (load (merge-pathnames "xml.location.asd" *load-truename*))
    (values))

(defsystem :xml.location-and-local-time
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(xml.location-system:version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "To and from XML conversion for local-time timestamps."
  :depends-on  ((:version :xml.location #.(xml.location-system:version/string))
	        :local-time)
  :components  ((:file       "local-time"
		 :pathname   "src/local-time")))
