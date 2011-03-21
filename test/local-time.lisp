;;; local-time.lisp --- Unit tests for local-time XML conversion.
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

(in-package :cxml-location.test)

(deftestsuite local-time-root (root)
  ()
  (:documentation
   "Unit tests for local-time XML conversions."))

(addtest (local-time-root
          :documentation
	  "Test case for ->xml conversion.")
  ->xml

  (let ((now (local-time:now))
	(loc (loc "<foo ts='~S'/>" "node()/@ts")))
    (setf (val loc) now)
    (ensure-same (val loc)
		 (format nil "~S" now)
		 :test 'string=)))

(addtest (local-time-root
          :documentation
	  "Test case for xml-> conversion.")
  xml->

  (let* ((now (local-time:now))
	 (loc (loc (format nil "<foo ts='~S'/>" now)
		   "node()/@ts")))
    (ensure-same (val loc :type 'local-time:timestamp)
		 now
		 :test 'local-time:timestamp=)

    ;; Inner types are invalid
    (ensure-condition 'xml->-conversion-error
      (val loc :type '(local-time:timestamp :some-inner-type))))

  ;; Two invalid timestamp values.
  (let ((loc (loc "<foo ts='invalid timestamp'/>" "node()/@ts")))
    (ensure-condition 'xml->-conversion-error
      (val loc :type 'local-time:timestamp)))

  (let ((loc (loc "<foo ts='@invalid timestamp'/>" "node()/@ts")))
    (ensure-condition 'local-time::invalid-timestring
      (val loc :type 'local-time:timestamp))))
