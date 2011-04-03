;;; location.lisp ---
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

(deftestsuite location-root (root)
  ()
  (:documentation
   "Root test suite for test of location class."))

(addtest (location-root
          :documentation
	  "Smoke test for location class.")
  smoke

  (ensure (loc "<simple attr='8'>text</simple>" "simple/text()")))

(addtest (location-root
          :documentation
	  "Test name accessor of location.")
  name

  (let ((loc (loc "<bla a='1'><bli b='5'>umumum</bli><bli/></bla>"
		  "/bla/bli")))
    (ensure-same (name loc) "bli"))

  (let ((loc (loc "<bla a='1'><bli b='5'>umumum</bli><bli/></bla>"
		  "/bla/bli"
		  :if-multiple-matches :all)))
    (ensure-same (name loc) '("bli" "bli"))

    (setf (name loc) "blup")
    (ensure-same (name loc) '("blup" "blup"))))

(addtest (location-root
          :documentation
	  "Test `loc' reader.")
  loc

  (let* ((loc   (loc "<foo><bar baz='bla'/></foo>" "node()"))
	 (child (loc loc "bar")))
    (ensure-same
     (name child) "bar"
     :test #'string=)
    (ensure-same
     (@ child "baz") "bla"
     :test #'string=)))

(addtest (location-root
          :documentation
	  "Test namespace handling in locations.")
  namespaces

  (let ((loc (loc "<bla/>" "node()"
		  :namespaces '(&default ("fb" . "http://foo.org/bar")))))
    (ensure-same
     (location-namespaces loc)
     '((nil     . "")
       ("xmlns" . "http://www.w3.org/2000/xmlns/")
       ("xml"   . "http://www.w3.org/XML/1998/namespace")
       ("fb"    . "http://foo.org/bar"))
     :test #'equal))

  (let ((loc (loc "<bla xmlns='http://foo.org/bar' xmlns:fb='http://foo.org/bar' a='1'><bli b='5'>umumum</bli><fb:bli/></bla>"
		  "/fb:bla/fb:bli"
		  :if-multiple-matches :all
		  :namespaces '(&default ("fb" . "http://foo.org/bar")))))
    (ensure-same (name loc)            '("bli" "bli"))
    (ensure-same (name loc :prefix? t) '("bli" "fb:bli")))

  (let ((loc (loc "<fb:bla xmlns:fb='http://foo.org/bar' a='1'><bli b='5'>umumum</bli><bli/></fb:bla>"
		  "/fb:bla"
		  :namespaces '(&default ("fb" . "http://foo.org/bar")))))
    (ensure-same (name loc)            "bla")
    (ensure-same (name loc :prefix? t) "fb:bla"))

  ;; Test setting the element's name to a qualified name.
  (let ((loc (loc "<bla/>" "node()"
		  :namespaces '(&default ("foo" . "http://bar.bzr")))))
    (setf (name loc) "foo:bar")
    (ensure-same
     (name loc :prefix? t) "foo:bar"
     :test #'string=)))
