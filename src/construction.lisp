;;; construction.lisp --- Construction of location instances.
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

(in-package :cxml-location)

(defmethod loc ((document stp:node) (path string)
		&rest args
		&key
		(if-multiple-matches :error)
		(if-no-match         :error)
		(assign-mode         :replace)
		&allow-other-keys)
  "Create a location for DOCUMENT and PATH. The class of the location
instance is determined based on the values of IF-MULTIPLE-MATCHES and
IF-NO-MATCH."
  ;; The keyword arguments are just there to document the interface.
  (declare (ignore if-multiple-matches if-no-match assign-mode))

  ;; Create the location instance
  (let+ (((&values class args) (apply #'%compute-location-class args)))
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


;;; Utility Functions
;;

(declaim (ftype (function (&rest t
			   &key
			   (:if-multiple-matches if-multiple-matches-policy-designator)
			   (:if-no-match         if-no-match-policy-designator)
			   (:assign-mode         assign-mode-designator)
			   &allow-other-keys)
			  (values class list))
		%compute-location-class))

(defun %compute-location-class (&rest args
				&key
				(if-multiple-matches :error)
				(if-no-match         :error)
				(assign-mode         :replace)
				&allow-other-keys)
  "Compute a location class based on the values of IF-MULTIPLE-MATCHES
and IF-NO-MATCH. This is a separate function to make it usable in
compiler macros."
  (let ((mixins))

    ;; Assign mode
    (ecase assign-mode
      (:append
       (pushnew 'multi-location mixins)
       (pushnew 'append-nodes-mixin mixins) ;; ensure precedence
       (setf if-multiple-matches :all))
      (:replace))
    (remove-from-plistf args :assign-mode)

    ;; Multiple matches policy
    (ecase if-multiple-matches
      ((:error :first :last :any)
       (pushnew 'singleton-location mixins))
      (:all
       (pushnew 'multi-location mixins)
       (remove-from-plistf args :if-multiple-matches :if-no-match)))

    ;; No match policy
    (ecase if-no-match
      (:create
       (pushnew 'create-missing-nodes-mixin mixins))
      (:do-nothing
       (pushnew 'ignore-empty-result-mixin mixins))
      (:error))

    ;; Return the class and initargs
    (values (ensure-location-class mixins) args)))
