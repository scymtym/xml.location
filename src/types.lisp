;;; types.lisp --- Types used in the cxml-location system.
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

(deftype list-of-length (length &optional (element-type t))
  "A list of LENGTH with elements of type ELEMENT-TYPE or t when
ELEMENT-TYPE IS not supplied."
  (%list-of-length length element-type))

(deftype if-no-match-policy-designator ()
  `(member :error :do-nothing :create))

(deftype if-multiple-matches-policy-designator ()
  `(member :error :first :last :any :all))

(deftype assign-mode-designator ()
  `(member :replace :append))


;;; Utility functions
;;

(defun %list-of-length (length element-type)
  (if (zerop length)
      'null
      `(cons ,element-type
	     ,(%list-of-length (1- length) element-type))))
