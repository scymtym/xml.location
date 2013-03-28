;;;; compat.lisp --- Internal compatibility module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:xml.location.compat
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:export
   #:define-dynamic-class-family)

  (:documentation
   "Internal compatibility package."))

(cl:in-package #:xml.location.compat)

;;; Types

(deftype superclass-spec-simple ()
  "A simple superclass spec is a class name."
  `(and symbol (not keyword)))

(deftype superclass-spec-start ()
  "Superclass spec for appending a superclass at the start of the list
of direct superclasses."
  `(cons (eql :start) (cons superclass-spec-simple null)))

(deftype superclass-spec-end ()
  "Superclass spec for appending a superclass at the end of the list
of direct superclasses."
  `(cons (eql :end) (cons superclass-spec-simple null)))

(deftype superclass-spec ()
  `(or superclass-spec-simple
       superclass-spec-start
       superclass-spec-end))

;;; Main macro

(defmacro define-dynamic-class-family (name &rest doc-and-options)
  "Define a family of classes in the category NAME. Classes in the
family are fully specified by their respective sets of
superclasses. No further structure is added.

For a family named NAME, the following things are defined:
+ *NAME-classes*         [variable]
+ make-NAME-class        [generic function]
  + make-NAME-class list [method]
+ ensure-NAME-class      [function]
+ NAME-classes           [function]

Unless the :package option is supplied, all names will be interned in
NAME's package. Using the :package option, a different package can be
specified. When supplied, the value of the :package option is
evaluated.

Valid options and their respective defaults are:
+ package             [(package-name (symbol-package name))]
  The package in which defined variables and functions will be placed.
+ metaclass           ['standard-class]
  The metaclass that should be used for classes in the family.
+ common-superclasses [nil]
  A list of superclasses that should be added to all classes in the
  family. List elements can either be classes or lists of the
  form (PLACEMENT CLASS) where PLACEMENT is either :start or :end and
  CLASS is a class. PLACEMENT controls where a superclass is inserted
  into the list of superclasses. The default placement is :end.
+ sort-mixins?        [nil]
  If non-nil, the list of mixin classes is sorted (according to their
  `class-name's as strings) prior to looking for or creating a dynamic
  class.

Generated documentation is available for the symbol NAME using
type :dynamic-class-family."
  (check-type name symbol "a symbol")

  (let+ (;; Argument parsing
         ((doc
           &key
           (package            (package-name (symbol-package name)) package-supplied?)
           (metaclass          'standard-class)
           common-superclasses
           sort-mixins?
           (stripped-suffix    "-MIXIN"))
          (%parse-doc-and-options doc-and-options))

         ;;
         (metaclass (find-class metaclass))
         ((&values start-superclasses end-superclasses)
          (%process-superclass-specs common-superclasses))

         ;; Maybe evaluate package
         (package (if package-supplied? (eval package) package))

         ;; Name processing helpers
         ((&flet make-name (format)
            (intern (format nil format name) package)))

         ;; Symbols
         (class-hash-name    (make-name "*~A-CLASSES*"))
         (make-class-name    (make-name "MAKE-~A-CLASS"))
         (ensure-class-name  (make-name "ENSURE-~A-CLASS"))
         (list-classes-name  (make-name "~A-CLASSES"))
         (clear-classes-name (make-name "CLEAR-~A-CLASSES!")))
    `(progn
       (defvar ,class-hash-name (make-hash-table :test 'equal)
         "Association of lists of mixins to previously created
classes.")

       (defgeneric ,make-class-name (mixins)
         (:method ((mixins list))
           (let+ (;; Name processing helpers
                  ((&flet strip-suffix (name)
                     (cond
                       ((ends-with-subseq ,stripped-suffix name)
                        (subseq name 0 (- (length name) ,(length stripped-suffix))))
                       (t name))))
                  ((&flet short-class-name (class)
                     (intern (strip-suffix (string (class-name class)))
                             :keyword)))
                  ((&flet full-class-name (superclasses)
                     `(,,(intern (string name) :keyword)
                       ,@(if (> (length superclasses) 2)
                             `(,(length superclasses) :mixins)
                             (mapcar #'short-class-name superclasses)))))

                  ;; Superclasses and class
                  (superclasses (mapcar #'ensure-find-class
                                        (append ,@(when start-superclasses
                                                    `(',start-superclasses))
                                                mixins
                                                ,@(when end-superclasses
                                                    `(',end-superclasses)))))
                  (class        (make-instance ,metaclass
                                               :name
                                               (full-class-name superclasses)
                                               :direct-superclasses
                                               superclasses)))
             class))
         (:documentation
          "Dynamically make a class composed of MIXINS."))

       (defun ,ensure-class-name (mixins)
         "Find or make the dynamic class composed of MIXINS."
         (let ((key (,(if sort-mixins?
                          '%make-key/sorted
                          '%make-key/unsorted) mixins)))
           (ensure-gethash key ,class-hash-name (,make-class-name mixins))))

       (defun ,clear-classes-name ()
         "Clear all previously defined dynamic classes."
         (clrhash ,class-hash-name))

       ;; Introspection and Documentation
       (defun ,list-classes-name ()
         "Return list of all classes in the family."
         (hash-table-values ,class-hash-name))

       ,@(when doc
           `((defmethod documentation ((thing (eql ',name))
                                       (type  (eql :dynamic-class-family)))
               (format nil "~@<~A ~_~:[[No known ~
classes]~;~:*Known classes: ~:@_~{+ ~S~^~_~}~]~@:>"
                       ,doc (,list-classes-name)))))

       ;; Return names of defined things
       (values ',make-class-name ',ensure-class-name
               ',clear-classes-name ',list-classes-name))))

;;; Utility functions

;; Note: used at runtime
(defun ensure-find-class (class-or-name)
  (etypecase class-or-name
    (class
     class-or-name)
    (t
     (nth-value 0 (find-class class-or-name)))))

(defun %parse-doc-and-options (doc-and-options)
  "Parse DOC-AND-OPTIONS which is expected to be a list of the form
\([DOC] (:NAME VALUE)*).
Return a list of the form
\(DOC :NAME1 VALUE1 ...)
in which DOC may be nil."
  (let+ (((doc options) (if (stringp (first doc-and-options))
                            (list (first doc-and-options)
                                  (rest  doc-and-options))
                            (list nil doc-and-options))))
    `(,doc ,@(apply #'append options))))

(defun %ensure-class-name (class-or-name)
  (etypecase class-or-name
    (class
     (nth-value 0 (class-name class-or-name)))
    (t
     class-or-name)))

(defun %make-key/sorted (mixins)
  "Return a key for the dynamic class described by MIXINS."
  (sort (%make-key/unsorted mixins) #'string< :key #'symbol-name))

(defun %make-key/unsorted (mixins)
  "Return a key for the dynamic class described by MIXINS."
  (mapcar #'%ensure-class-name mixins))

(defun %process-superclass-specs (specs)
  "Parse superclass specs SPECS and return two values: superclasses
that should be inserted at the start of the list of direct
superclasses and superclasses that should inserted at the end of the
list of direct superclasses."
  (check-type specs list "a list of superclass specifications")

  (let ((start)
        (end))
    (dolist (spec specs)
      (typecase spec
        (superclass-spec-simple
         (appendf end (list spec)))
        (superclass-spec-start
         (appendf start (rest spec)))
        (superclass-spec-end
         (appendf end (rest spec)))
        (t
         (error 'invalid-superclass-spec
                :spec spec))))
    (values start end)))
