;;; xml.location-and-lisplab.asd --- System definition for xml.location and lisplab interop.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:defpackage #:xml.location-and-lisplab-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:xml.location-and-lisplab-system)

#.(progn
    (load (merge-pathnames "xml.location.asd" *load-truename*))
    (values))

(defsystem :xml.location-and-lisplab
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(xml.location-system:version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "To and from XML conversion for matrices."
  :depends-on  ((:version :xml.location #.(xml.location-system:version/string))
	        :lisplab)
  :components  ((:file       "lisplab"
		 :pathname   "src/lisplab")))
