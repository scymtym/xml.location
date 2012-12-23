;;; package.lisp --- Package Definition for Unit Tests of the xml.location System.
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

(cl:defpackage #:xml.location.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:lift

   #:xml.location)

  (:export
   #:root)

  (:documentation
   "This package contains unit tests for the xml.location system."))

(cl:in-package #:xml.location.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the xml.location system."))
