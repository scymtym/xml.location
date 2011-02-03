;;; construction.lisp --- Construction of location instances.
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

(defmethod loc ((document stp:node) (path t)
		&rest args
		&key
		if-multiple-matches
		if-no-match
		&allow-other-keys)
  (bind (((class args) (cond
			 ((eq if-no-match :create)
			  (error ":create is not supported, yet."))
			 ((eq if-multiple-matches :all)
			  `(multi-location
			    ,(remove-from-plist args :if-multiple-matches)))
			 (t
			  `(singleton-location ,args)))))
    (apply #'make-instance class
	   :document document
	   :path     path
	   args)))

(defmethod loc ((document string) (path t)
		&rest args)
  "Parse DOCUMENT as XML document before constructing the location."
  (let ((document (cxml:parse document (stp:make-builder))))
    (apply #'loc document path args)))

(defmethod loc :around ((document t) (path function)
			&rest args)
  "Interpret PATH as compiled XPath, skipping the compilation step."
  (apply #'loc document nil :compiled-path path args))
