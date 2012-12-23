;;; ignore-empty-results-mixin.lisp --- Unit tests for ignore-empty-results-mixin.
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

(cl:in-package #:cxml-location.test)

(deftestsuite ignore-empty-results-mixin (root)
  ((cases '(("<bla/>"
	     "bla/bar"))))
  (:documentation
   "Unit tests for the `ignore-empty-results-mixin' mixin class."))

(addtest (ignore-empty-results-mixin
          :documentation
	  "Smoke test for the `ignore-empty-results-mixin' mixin
class.")
  smoke

  ;; Create locations for the specified documents and paths and then
  ;; ensure that unmatched paths work as expected.
  (iter (for (doc path . args) in cases)
	(let ((loc (apply #'loc doc path
			  :if-no-match :do-nothing
			  args)))
	  ;; name
	  (ensure-same
	   (name loc) nil)
	  (setf (name loc) "foo")
	  ;; val
	  (ensure-same
	   (val loc) nil)
	  (setf (val loc) "foo")
	  ;; @
	  (ensure-same
	   (@ loc "does-not-exist") nil)
	  (setf (@ loc "does-not-exist") "foo")

	  (let ((new-doc (stp:serialize (location-document loc)
					(cxml:make-string-sink
					 :omit-xml-declaration-p t))))
	  (ensure-same
	   new-doc doc
	   :test      #'string=
	   :report    "~@<After applying `setf' methods, the document ~S ~
is no longer identical to the original document ~S.~@:>"
	   :arguments (new-doc doc))))))
