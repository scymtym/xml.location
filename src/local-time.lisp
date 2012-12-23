;;; local-time.lisp --- XML conversions for local-time timestamps.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package #:cxml-location)


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
