;;; multi-location.lisp --- Unit tests for the multi-location class.
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

(deftestsuite multi-location-root (root)
  ((simple-document "<bla a='1'><bli b='5'>umumum</bli><bli/></bla>"))
  (:documentation
   "Unit tests for the `multi-location' location class."))

(addtest (multi-location-root
          :documentation
	  "Test name accessor of the `multi-location' class.")
  name

  (let ((loc (loc simple-document "/bla/bli"
		  :if-multiple-matches :all)))
    (ensure-same (name loc) '("bli" "bli")
		 :test #'equal)

    (setf (name loc) "blup")

    (ensure-same (name loc) '("blup" "blup")
		 :test #'equal)))