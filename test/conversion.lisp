;;; conversion.lisp --- Unit tests for conversion methods.
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

(deftestsuite conversion-root (root)
  ()
  (:documentation
   "Root test suite for conversion tests."))

(deftestsuite xml->-root (conversion-root)
  ()
  (:function
   (check-xml->-conversion (value type expected)
     (let ((result (xml-> value type)))
       (ensure-same
	result expected
	:test      #'equal
	:report    "~@<`xml->' conversion of the value ~S to type ~S ~
produced ~S, not ~S.~@:>"
	:arguments (value type result expected)))))
  (:documentation
   "Root test suite for from-XML conversion tests."))

(addtest (xml->-root
          :documentation
	  "Smoke test for from-XML conversion.")
  smoke

  (ensure-cases (value type expected)
      '(("string"                 string        "string")
	("5"                      real          5)
	("5"                      (real)        5)
	(":a :b"                  list          (":a" ":b"))
	(":a :b"                  (list symbol) (:a :b))
	("(COMPLEX DOUBLE-FLOAT)" type          (complex double-float)))
    (check-xml->-conversion value                 type expected)
    (check-xml->-conversion (stp:make-text value) type expected)
    (check-xml->-conversion
     (stp:make-attribute value "foo") type expected)))

(addtest (xml->-root
          :documentation
	  "Test for conditions signaled by from-XML conversions.")
  conditions

  ;; Generic error behavior
  (ensure-condition 'no-xml->-conversion-method
    (xml-> :value :no-such-type))
  (ensure-condition 'no-xml->-conversion-method
    (xml-> :value '(:no-such-type :with-inner-type-foo)))
  (ensure-condition 'xml->-conversion-error
    (xml-> :value nil))

  ;; Error behavior for stp nodes
  (ensure-condition 'xml->-conversion-error
    (xml-> (stp:make-element "value") 'string)))

(deftestsuite ->xml-root (conversion-root)
  ()
  (:function
   (check-->xml-conversion (value dest type expected)
     (let ((result (->xml value dest type)))
       (ensure-same
	result expected
	:test      #'equal
	:report    "~@<`xml->' conversion of the value ~S to destination ~S ~
with type ~S produced ~S, not ~S.~@:>"
	:arguments (value dest type result expected)))))
  (:function
   (check-->xml-conversion-node (value dest type expected)
     (let ((result (->xml value dest type)))
       (ensure-same
	result value
	:test      #'equal
	:report    "~@<`xml->' conversion of the value ~S to destination ~S ~
with type ~S returned ~S, not ~S.~@:>"
	:arguments (value dest type result value))
       (ensure-same
	(stp:string-value dest) expected
	:test      #'equal
	:report    "~@<`xml->' conversion of the value ~S to destination ~S ~
with type ~S produced ~S, not ~S.~@:>"
	:arguments (value dest type result expected)))))
  (:documentation
   "Root test suite for to-XML conversion tests."))

(addtest (->xml-root
          :documentation
	  "Smoke test for to-XML conversion.")
  smoke

  (ensure-cases (value type expected)
      '(("string"               string "string")
	(5                      string "5")
	((":a" ":b")            string "\":a\" \":b\"")
	((:a :b)                string ":A :B")
	((complex double-float) type   "(COMPLEX DOUBLE-FLOAT)"))
    (check-->xml-conversion value 'string type expected)
    (check-->xml-conversion-node
     value (stp:make-text "") type expected)
    (check-->xml-conversion-node
     value (stp:make-attribute "" "foo") type expected)))

(addtest (->xml-root
          :documentation
	  "Test for conditions signaled by to-XML conversions.")
  conditions

  ;; Generic error behavior
  (ensure-condition 'no-->xml-conversion-method
    (->xml :value :dest :no-such-type))
  (ensure-condition 'no-->xml-conversion-method
    (->xml :value :dest '(:no-such-type :with-inner-type-foo)))
  (ensure-condition '->xml-conversion-error
    (->xml :value :dest nil)) ;; nil is not a valid type

  ;; Error behavior for stp nodes
  (ensure-condition '->xml-conversion-error
    (->xml "value" (stp:make-element "foo") 'string)))
