;;; package.lisp --- Package Definition for xml.location Module.
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

(cl:defpackage #:xml.location
  (:nicknames
   #:xloc)

  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus)

  (:import-from #:xml.location.compat
   #:define-dynamic-class-family)

  ;; Special Symbols and variables
  (:export
   #:&default
   #:*cl-namespaces*)

  ;; Conditions
  (:export
   #:location-error
   #:location-error-document
   #:location-error-path

   #:missing-xpath-source

   #:result-set-error-mixin
   #:location-error-result-set

   #:empty-result-set

   #:too-many-matches-in-result-set

   #:invalid-binding-form
   #:invalid-binding-form-form

   #:no-such-accessor-form
   #:invalid-binding-form-spec
   #:invalid-binding-form-name

   #:xpath-creation-error
   #:xpath-creation-error-location
   #:xpath-creation-error-type
   #:xpath-creation-error-name
   #:xpath-creation-error-predicate

   #:conversion-error
   #:conversion-error-value
   #:conversion-error-type

   #:no-conversion-method-mixin
   #:conversion-error-function

   #:xml->-conversion-error

   #:no-xml->-conversion-method

   #:->xml-conversion-error
   #:conversion-error-destination

   #:no-->xml-conversion-method)

  ;; Location Protocol
  (:export
   #:location-document
   #:location-namespaces
   #:location-path
   #:location-result
   #:name #:val #:@)

  ;; Dynamic Location Classes
  (:export
   #:make-location-class
   #:ensure-location-class
   #:location-classes)

  ;; Location Construction Protocol
  (:export
   #:loc)

  ;; Conversion Protocol
  (:export
   #:xml-> #:->xml)

  ;; Location Classes
  (:export
   #:location
   #:singleton-location
   #:multi-location
   #:create-missing-nodes-mixin
   #:ignore-empty-results-mixin
   #:append-nodes-mixin)

  ;; Macros
  (:export
   #:with-locations #:with-locations-r/o)

  (:documentation
   "This package contains the public interface of the cmxl-location
system. The main entry point is the generic function `loc', which
creates `location' instances from XML documents and XPaths. The
resulting objects can be queried and modified using the location-*
accessors and the generic functions `name', `val' and `@'."))
