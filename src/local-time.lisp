;;; local-time.lisp --- XML conversions for local-time timestamps.
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

(in-package :cxml-location)


;;; String locations
;;

(defmethod xml-> ((value string) (type (eql 'local-time:timestamp))
		  &key
		  inner-types
		  &allow-other-keys)
  "Deserialize timestamp from VALUE."
  (when inner-types
    (error 'xml->-conversion-error
	   :value            value
	   :type             type
	   :format-control   "~@<The type ~S does not have inner types, ~
but ~S has been specified as inner types.~@:>"
	   :format-arguments `(,type ,inner-types)))

  (unless (eq (aref value 0) #\@)
    (error 'xml->-conversion-error
	   :value            value
	   :type             type
	   :format-control   "~@<Serialized timestamp value ~S does not ~
start with '@'.~@:>"
	   :format-arguments `(,value)))

  (local-time:parse-timestring (subseq value 1) :fail-on-error t))

;; we get the primary ->xml method for free via prin1ing in this case

(defmethod ->xml :before  ((value local-time:timestamp)
			   (dest  t)
			   (type  (eql 'local-time:timestamp))
			   &key
			   inner-types
			   &allow-other-keys)
  "Signal an error when INNER-TYPES are supplied."
  (when inner-types
    (error '->xml-conversion-error
	   :value            value
	   :destination      dest
	   :type             type
	   :format-control   "~@<The type ~S does not have inner types, ~
but ~S has been specified as inner types.~@:>"
	   :format-arguments `(,type ,inner-types))))
