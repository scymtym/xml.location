;;;; variables.lisp --- Variables used in the xml.location system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

(defparameter *cl-namespaces*
  '(&default ("cl" . "http://common-lisp.net"))
  "Namespace list with a made-up Common Lisp namespace. This namespace
should be used when Common Lisp-specific data has to be stored in XML
documents in order to represent objects without loss of information.")
