;;;; macros.lisp --- Convenience macros for the xml.location system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

(defmacro with-locations ((&rest bindings-and-options) document
                          &body body)
  "Execute body with certain variables, specified by BINDINGS bound to
locations in DOCUMENT. DOCUMENT has to be of type
`stp:document'.

BINDINGS-AND-OPTIONS specifies let-like (generalized) variable
bindings according to the following syntax:
BINDINGS  ::= (BINDING* OPTION*)
BINDING   ::= (VAR-SPEC XPATH [ARG*])
VAR-SPEC  ::= NAME-SPEC | VAL-SPEC | @-SPEC | LOC-SPEC
NAME-SPEC ::= (:name SYMBOL [:prefix? BOOL])
VAL-SPEC  ::= SYMBOL | (:val SYMBOL [:type TYPE])
@-SPEC    ::= (:@ @-NAME [:type TYPE])
@-NAME    ::= SYMBOL | (SYMBOL \"STRING\")
LOC-SPEC  ::= (:loc SYMBOL)
OPTION    ::= KEY VALUE

In all cases, SYMBOL is the name of the generalized variable that is
created by the binding. If the (SYMBOL \"STRING\") form of @-NAME is
used, STRING specifies the name of the attribute independently of the
variable name, otherwise the name of attribute is computed as (string
SYMBOL).

Instead of the keywords :name, :val and :@ symbols of the same name in
the xml.location package can be used.

Example:
XLOC> (xloc:with-locations-r/o
          (((:@ (description-brief \"brief\")) \"package/description\")
           (description-long                \"package/description/text()\")
           (author                           \"package/author/text()\")
           (dependencies                     \"package/depend/@package\"
                                       :if-multiple-matches :all)
           (build-dependencies               \"package/rosbuild2/depend/@package\"
                                       :if-multiple-matches :all)
           ((:val messages :type 'list)      \"package/rosbuild2/msgs/text()\")
           ((:val services :type 'list)      \"package/rosbuild2/srvs/text()\")) doc
        (values author messages))
=> \"Joe User\" '(\"msg/bla.msg\")"
  (once-only (document)
    (let+ (((&values bindings options)
            (%parse-bindings-and-options bindings-and-options))
           ((&values locations places)
            (%make-location-and-place-forms document bindings
                                            :global-args options
                                            :writable?   t)))
      `(let+ ,locations
         (symbol-macrolet ,places
           ,@body)))))

(defmacro with-locations-r/o ((&rest bindings-and-options) document
                              &body body)
  "Like `with-locations', but binding places are not `setf'-able."
  (once-only (document)
    (let+ (((&values bindings options)
            (%parse-bindings-and-options bindings-and-options))
           ((&values locations places)
            (%make-location-and-place-forms document bindings
                                            :global-args options)))
      `(let ,locations
         (let+ ,places
           ,@body)))))

;;; Location and Place Forms

(defun %make-location-and-place-forms (document bindings
                                       &key
                                       global-args
                                       writable?)
  "Generate location and place forms for DOCUMENT and BINDINGS.
When WRITABLE? is non-nil, locations are created in the document if
necessary. Return two values:
+ A list of location forms
+ A list of place forms"
  (let ((reusable-locations (make-hash-table :test #'equal)))
    (iter (for spec in bindings)
          (let+ (((access-spec &optional (path ".") &rest args) spec)
                 (effective-args (append (when writable?
                                           `(:if-no-match :create))
                                         global-args
                                         args))
                 ((&flet make-location-form ()
                    (multiple-value-list
                     (%make-location-form document path effective-args))))
                 ((&values name access-form effective-args)
                  (%parse-access-spec access-spec :args effective-args))
                 (key (cons path effective-args))
                 ((location-var &optional location-form)
                  (cond
                    ;; If PATH is not constant, we have to emit a
                    ;; separate location form.
                    ((not (constantp path))
                     (make-location-form))
                    ;; Otherwise, we can try to look it up in the
                    ;; table of reusable locations.
                    ((butlast (gethash key reusable-locations)))
                    ;; If there was no reusable location, we have to
                    ;; create a new one and add it to the table.
                    (t
                     (setf (gethash key reusable-locations)
                           (make-location-form))))))

            ;; Collect location construction form.
            (when location-form
              (collect location-form :into locations))

            ;; Collect symbol-macrolet form.
            (collect `(,name ,(funcall access-form location-var)) :into places))

          (finally (return (values locations places))))))

(defun %make-location-form (document path args)
  "Make a form that creates the `location' instance for DOCUMENT, PATH
and ARGS. Return two values:
+ a symbol for the variable that will hold the location instance
+ a form that should be evaluated to compute the location instance or
  nil, if a location variable emitted earlier can be reused."
  (let ((location-var (gensym "LOCATION")))
    (values location-var `(,location-var
                           (loc ,document ,path ,@args)))))

;;; Access Spec Parser Methods

(defgeneric %parse-access-spec (spec &rest args
                                &key &allow-other-keys))

(defmethod %parse-access-spec ((spec t)
                               &key
                               inner-specs)
  (%signal-no-such-accessor-form spec inner-specs))

(defmethod %parse-access-spec ((spec (eql nil))
                               &key
                               inner-specs)
  (%signal-no-such-accessor-form spec inner-specs))

(defmethod %parse-access-spec ((spec list)
                               &rest args)
  (if (not (symbolp (first spec)))
      (call-next-method)
      (apply #'%parse-access-spec
             (make-keyword (first spec))
             :inner-specs (rest spec)
             args)))

(defmethod %parse-access-spec ((spec symbol)
                               &rest args)
  (if (keywordp spec)
      (call-next-method)
      (apply #'%parse-access-spec :val
             :inner-specs `(,spec)
             args)))

(defmethod %parse-access-spec ((spec (eql :loc))
                               &key
                               inner-specs
                               args)
  (let+ (((name) inner-specs))
    (values name (lambda (location-var) `(loc ,location-var "."))  args)))

(defmethod %parse-access-spec ((spec (eql :name))
                               &key
                               inner-specs
                               args)
  (let+ (((name &key prefix?) inner-specs))
    (values name (lambda (location-var)
                   `(name ,location-var ,@(when prefix? '((:prefix t))))) args)))

(defmethod %parse-access-spec ((spec (eql 'name))
                               &rest args)
  (apply #'%parse-access-spec :name args))

(defmethod %parse-access-spec ((spec (eql :val))
                               &key
                               inner-specs
                               args)
  (let+ (((name &key type) inner-specs))
    (values name (lambda (location-var)
                   `(val ,location-var ,@(when type `(:type ,type)))) args)))

(defmethod %parse-access-spec ((spec (eql 'val))
                               &rest args)
  (apply #'%parse-access-spec :val args))

(defmethod %parse-access-spec ((spec (eql :@))
                               &key
                               inner-specs
                               args)
  (let+ (((name-spec &key type) inner-specs)
         ((&plist-r/o (if-no-match :if-no-match)) args)
         ((name &optional (attribute-name (string-downcase name)))
          (ensure-list name-spec)))
    (values name
            (lambda (location-var)
              (if (eq if-no-match :do-nothing) ; TODO allow evaluation at runtime
                  `(when (location-attribute ,location-var ,attribute-name
                                             :if-does-not-exist nil)
                     (@ ,location-var ,attribute-name
                                        ,@(when type
                                            `(:type ,type))))
                  `(@ ,location-var ,attribute-name
                                    ,@(when type
                                        `(:type ,type)))))
            (remove-from-plist args :if-no-match))))

(defmethod %parse-access-spec ((spec (eql '@))
                               &rest args)
  (apply #'%parse-access-spec :@ args))

;;; Utility Functions

(defun %parse-bindings-and-options (bindings-and-options)
  "Separate BINDINGS-AND-OPTIONS into binding forms and
options. Return two values: the collected binding forms and a plist
containing the collected options."
  (iter (for  (binding-or-key binding-or-value) on bindings-and-options)
        (with skip?)
        (cond
          (skip?
           (setf skip? nil))
          ((listp binding-or-key)
           (unless (>= (length binding-or-key) 1)
             (error 'invalid-binding-form
                    :form binding-or-key))
           (collect binding-or-key :into bindings))
          ((keywordp binding-or-key)
           (unless binding-or-value
             (error 'invalid-binding-form
                    :form binding-or-key))
           (collect binding-or-key   :into options)
           (collect binding-or-value :into options)
           (setf skip? t))
          (t
           (error 'invalid-binding-form
                  :form binding-or-key)))
        (finally (return (values bindings options)))))

(defun %signal-no-such-accessor-form (spec args)
  (error 'no-such-accessor-form
         :form `((,spec ,@args) "<path>")
         :spec `(,spec ,@args)
         :name spec))
