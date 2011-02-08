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

(defmethod loc ((document stp:node) (path string)
		&rest args
		&key
		(if-multiple-matches :error)
		if-no-match
		&allow-other-keys)
  "Create a location for DOCUMENT and PATH. The class of the location
instance is determined based on the values of IF-MULTIPLE-MATCHES and
IF-NO-MATCH."
  (let ((mixins))
    ;; Multiple matches policy
    (case if-multiple-matches
      ((:error :first :last :any)
       (push 'singleton-location mixins))
      (:all
       (push 'multi-location mixins)
       (remove-from-plistf args :if-multiple-matches :if-no-match)))

    ;; No match policy
    (when if-no-match
      (ecase if-no-match
	(:create
	 (push 'create-missing-nodes-mixin mixins))
	((:error :do-nothing))))

    ;; Create the location instance
    (apply #'make-instance (ensure-location-class mixins)
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
