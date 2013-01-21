;;;; create-missing-nodes-mixin.lisp --- Unit tests for create-missing-nodes-mixin.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite create-missing-nodes-mixin-root (root)
  ((cases '(("<bla><blup/><blop/></bla>"
	     "/bla/blup/bli/node()")

	    ("<bla><bli/><bli c='foo'/><bli><blup/></bli><bli><blup a='foo'/></bli></bla>"
	     "/bla/bli/blup/@a"
	     :if-multiple-matches :all)

	    ("<bla><bli/><bli c='foo'/><bli><blup/></bli><bli><blup a='foo'/></bli></bla>"
	     "/bla/bli/blup/@a"
	     :if-multiple-matches :all)

	    ("<bla/>"
	     "/bla/text()"))))
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
