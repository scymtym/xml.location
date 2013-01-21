;;;; local-time.lisp --- XML conversions for local-time timestamps.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)


;;; String locations
;;

(defmethod xml-> ((value string) (type (eql 'local-time:timestamp))
		  &key
		  inner-types
		  &allow-other-keys)
  "Deserialize timestamp from VALUE."
  (when inner-types
    (xml->-conversion-error
     value type
     "~@<The type ~S does not have inner types, but ~S has been ~
specified as inner types.~@:>"
     type inner-types))

  (local-time:parse-timestring
   value
   :start         (if (starts-with #\@ value) 1 0)
   :fail-on-error t))

;; we get the primary ->xml method for free via prin1ing in this case

(defmethod ->xml :before  ((value local-time:timestamp)
			   (dest  t)
			   (type  (eql 'local-time:timestamp))
			   &key
			   inner-types
			   &allow-other-keys)
  "Signal an error when INNER-TYPES are supplied."
  (when inner-types
    (->xml-conversion-error
     value type dest
     "~@<The type ~S does not have inner types, but ~S has been ~
specified as inner types.~@:>"
     type inner-types)))
