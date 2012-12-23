;;; multi-location.lisp --- Unit tests for the multi-location class.
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

(deftestsuite multi-location-root (root)
  ((simple-document "<bla a='1'><bli b='5'>umumum</bli><bli b='-5'/></bla>"))
  (:documentation
   "Unit tests for the `multi-location' location class."))

(addtest (multi-location-root
          :documentation
	  "Smoke test for `multi-location' class.")
  smoke

  (ensure (loc simple-document "bla/bli/text()"
	       :if-multiple-matches :all))

  ;; We get a location, its result set is empty
  (let ((loc (loc simple-document "no-such-element"
		  :if-multiple-matches :all)))
    (ensure loc)
    (ensure (xpath:node-set-empty-p (location-result loc)))))

(addtest (multi-location-root
          :documentation
	  "Test name accessor of the `multi-location' class.")
  name

  (let ((loc (loc simple-document "/bla/bli"
		  :if-multiple-matches :all
		  :namespaces '(&default ("foo" . "http://foo.org/bar")))))
    (ensure-same (name loc) '("bli" "bli")
		 :test #'equal)

    (setf (name loc) "blup")
    (ensure-same (name loc) '("blup" "blup")
		 :test #'equal)

    ;; A name with a colon should be treated as a qualified name.
    (setf (name loc) "foo:blup")
    (ensure-same (name loc) '("blup" "blup")
		 :test #'equal)
    (ensure-same (name loc :prefix? t) '("foo:blup" "foo:blup")
		 :test #'equal)

    ;; A list of three strings should be treated as a qualified name.
    (setf (name loc) '("local" "prefix" "http://url.org/"))
    (ensure-same (name loc) '("local" "local")
		 :test #'equal)
    (ensure-same (name loc :prefix? t) '("prefix:local" "prefix:local")
		 :test #'equal)))

(addtest (multi-location-root
          :documentation
	  "Test attribute accessor of the `multi-location'
class.")
  attribute

  (let ((loc (loc simple-document "/bla/bli"
		  :if-multiple-matches :all)))
    (ensure-same (@ loc "b") '("5" "-5")
		 :test #'equal)

    (setf (@ loc "b") "6")
    (ensure-same (@ loc "b") '("6" "6")
		 :test #'equal)

    (ensure-condition 'error ;;; TODO(jmoringe): proper condition
      (@ loc "attribute-does-not-exist"))))
