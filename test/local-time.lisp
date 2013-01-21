;;;; local-time.lisp --- Unit tests for local-time XML conversion.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite local-time-root (root)
  ((now       (local-time:now))
   (empty-loc (loc "<foo ts=''/>" "node()/@ts"))
   (now-loc))
  (:setup
   (setf now-loc (loc (format nil "<foo ts='~S'/>" now)
		      "node()/@ts")))
  (:documentation
   "Unit tests for local-time XML conversions."))

(addtest (local-time-root
          :documentation
	  "Test case for ->xml conversion.")
  ->xml

  (setf (val empty-loc) now)
  (ensure-same (val empty-loc)
	       (format nil "~S" now)
	       :test 'string=))

(addtest (local-time-root
          :documentation
	  "Test case for xml-> conversion.")
  xml->

  (ensure-same (val now-loc :type 'local-time:timestamp)
	       now
	       :test 'local-time:timestamp=))

(addtest (local-time-root
          :documentation
	  "Test conditions signaled by local-time:timestamp
conversions.")
  conditions

  ;; Inner types are invalid
  (ensure-condition 'xml->-conversion-error
    (val now-loc :type '(local-time:timestamp :some-inner-type)))

  ;; For both directions
  (ensure-condition '->xml-conversion-error
    (setf (val empty-loc :type '(local-time:timestamp :some-inner-type))
	  now))

  ;; Two invalid timestamp values.
  (let ((loc (loc "<foo ts='invalid timestamp'/>" "node()/@ts")))
    (ensure-condition 'xml->-conversion-error
      (val loc :type 'local-time:timestamp)))

  (let ((loc (loc "<foo ts='@invalid timestamp'/>" "node()/@ts")))
    (ensure-condition 'xml->-conversion-error
      (val loc :type 'local-time:timestamp))))
