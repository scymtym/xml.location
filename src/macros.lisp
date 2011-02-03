;;; macros.lisp ---
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

(defmacro with-locations ((&rest bindings) document &body body)
  "Execute body with certain variables, specified by BINDINGS bound to
locations in DOCUMENT. DOCUMENT has to be of type
`stp:document'.

BINDINGS specifies let-like (generalized) variable
bindings according to the following syntax:
BINDINGS  ::= (BINDING*)
BINDING   ::= (VAR-SPEC XPATH [ARG*])
VAR-SPEC  ::= NAME-SPEC | VAL-SPEC | @-SPEC
NAME-SPEC ::= (:name SYMBOL [:prefix? BOOL])
VAL-SPEC  ::= SYMBOL | (:val SYMBOL [:type TYPE])
@-SPEC    ::= (:@ @-name [:type TYPE])
@-NAME    ::= SYMBOL | (SYMBOL \"STRING\")

In all cases, SYMBOL is the name of the generalized variable that is
created by the binding. If the (SYMBOL \"STRING\") form of @-NAME is
used, STRING specifies the name of the attribute independently of the
variable name, otherwise the name of attribute is computed as (string
SYMBOL).

Instead of the keywords :name, :val and :@ symbols of the same name in
the cxml-location package can be used."
  (once-only (document)
    (bind (((:values locations places)
	    (iter (for spec in bindings)
		  (bind (((access-spec path &rest args) spec)
			 (location-var (gensym "LOCATION"))
			 ((:values name access-form)
			  (%parse-access-spec access-spec
					      :location-var location-var)))
		    (collect `(,location-var
			       (loc ,document ,path
				    ,@(remove-from-plist args :type)))
		      :into locations)
		    (collect `(,name ,access-form) :into places))
		  (finally (return (values locations places))))))
      `(let ,locations
	 (symbol-macrolet ,places
	   ,@body)))))


;;; Access Spec Parser Methods
;;

(defgeneric %parse-access-spec (spec &rest args
				&key &allow-other-keys))

(defmethod %parse-access-spec ((spec list)
			       &rest args)
  (apply #'%parse-access-spec (first spec)
	 :inner-specs (rest spec)
	 args))

(defmethod %parse-access-spec ((spec symbol)
			       &rest args)
  (apply #'%parse-access-spec :val
	 :inner-specs `(,spec)
	 args))

(defmethod %parse-access-spec ((spec (eql :name))
			       &key
			       location-var
			       inner-specs)
  (bind (((name &key prefix?) inner-specs))
    (values
     name
     `(name ,location-var ,@(when prefix? '((:prefix t)))))))

(defmethod %parse-access-spec ((spec (eql 'name))
			       &rest args)
  (apply #'%parse-access-spec :name args))

(defmethod %parse-access-spec ((spec (eql :val))
			       &key
			       location-var
			       inner-specs)
  (bind (((name &key type) inner-specs))
    (values
     name
     `(val ,location-var
	   ,@(when type
		   `(:type ,type))))))

(defmethod %parse-access-spec ((spec (eql 'val))
			       &rest args)
  (apply #'%parse-access-spec :val args))

(defmethod %parse-access-spec ((spec (eql :@))
			       &key
			       location-var
			       inner-specs)
  (bind (((name-spec &key type) inner-specs)
	 ((name attribute-name) (if (listp name-spec)
				    name-spec
				    (list name-spec (string name-spec)))))
    (values
     name
     `(@ ,location-var ,attribute-name
		       ,@(when type
			       `(:type ,type))))))

(defmethod %parse-access-spec ((spec (eql '@))
			       &rest args)
  (apply #'%parse-access-spec :@ args))
