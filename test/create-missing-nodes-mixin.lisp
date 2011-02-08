;;; create-missing-nodes-mixin.lisp --- Unit tests for create-missing-nodes-mixin.
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

(deftestsuite create-missing-nodes-mixin-root (root)
  ((cases '(("<bla><blup/><blop/></bla>"
	     "/bla/blup/bli/node()")
	    ("<bla><bli/><bli c='foo'/><bli><blup/></bli><bli><blup a='foo'/></bli></bla>"
	     "/bla/bli/blup/@a"
	     :if-multiple-matches :all)
	    ("<bla><bli/><bli c='foo'/><bli><blup/></bli><bli><blup a='foo'/></bli></bla>"
	     "/bla/bli/blup/@a"
	     :if-multiple-matches :all))))
  (:documentation
   "Unit tests for the `create-missing-nodes-mixin' class."))

(addtest (create-missing-nodes-mixin-root
          :documentation
	  "Smoke test for the `create missing-nodes-mixin' class.")
  smoke

  ;; Create locations for the specified documents and paths and then
  ;; check whether the XPaths match on the modified documents.
  (iter (for (doc path . args) in cases)
	(let ((loc (apply #'loc doc path
			  :if-no-match :create
			  args)))
	  (ensure (not (xpath:node-set-empty-p
			(xpath:evaluate
			 path (location-document loc))))))))
