;;;; types.lisp --- Types used in the xml.location system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

(deftype if-no-match-policy-designator ()
  `(member :error :do-nothing :create))

(deftype if-multiple-matches-policy-designator ()
  `(member :error :first :last :any :all))

(deftype assign-mode-designator ()
  `(member :replace :append))
