;;;; xml.location-and-lisplab.asd --- System definition for xml.location and lisplab interop.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:xml.location-and-lisplab-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:xml.location-and-lisplab-system)

#.(progn
    (load (merge-pathnames "xml.location.asd" *load-truename*))
    (values))

(defsystem :xml.location-and-lisplab
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(xml.location-system:version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "To and from XML conversion for matrices."
  :depends-on  ((:version :xml.location #.(xml.location-system:version/string))
                :lisplab)
  :components  ((:file       "lisplab"
                 :pathname   "src/lisplab")))
