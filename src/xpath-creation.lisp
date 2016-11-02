;;;; xpath-creation.lisp --- XPath creation functions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location)

;;; Utility functions

(declaim (inline %expand-xpath-element))

;;; When necessary, pad the list SPEC with nil elements such that its
;;; length becomes 3.
(defun %expand-xpath-element (spec)
  (let ((missing (- 3 (length spec))))
    (if (plusp missing)
        (append spec (make-list missing))
        spec)))

;;; Implementation

(defmethod create-xpath ((document stp:node) (path string))
  ;; Parse XPath and process parsed representation.
  (create-xpath document (xpath:parse-xpath path)))

(defmethod create-xpath ((document stp:node) (path list))
  ;; Walk along XPath PATH in DOCUMENT, creating missing nodes as they
  ;; are encountered.
  (labels
      ((one-step (location step &rest steps)
         (let* ((result (xpath:evaluate
                         `(xpath:xpath (:path ,step)) location))
                (nodes  (if (xpath:node-set-empty-p result)
                            (apply #'create-xpath-element
                                   location (%expand-xpath-element step))
                            (xpath:all-nodes result))))
           (if steps
               (mapcan #'(lambda (n) (apply #'one-step n steps)) nodes)
               nodes))))
    (apply #'one-step document (rest path))))

(defmethod create-xpath-sibling ((document stp:node) (path string))
  ;; Parse XPath and process parsed representation.
  (create-xpath-sibling document (xpath:parse-xpath path)))

(defmethod create-xpath-sibling ((document stp:node) (path list))
  ;; Potentially create XPath PATH in DOCUMENT except for the final
  ;; element. Then unconditionally create a node matching the final
  ;; element of PATH.
  (let* ((butlast  (butlast (rest path)))
         (parents  (if butlast
                       (create-xpath document `(:path ,@butlast))
                       (list document)))
         (final    (lastcar path))
         (expanded (%expand-xpath-element final)))
    (mapcan #'(lambda (parent)
                (apply #'create-xpath-element parent expanded))
            parents)))

;;; `create-xpath-element'
;;;
;;; The following methods are specialized on `stp:node' instead of
;;; `stp:element' in order to work properly when LOCATION is an
;;; `stp:document'.

(defmethod no-applicable-method ((fuction (eql (fdefinition 'create-xpath-element)))
                                 &rest args)
  ;; Signal an error if no `create-xpath-element' method is
  ;; applicable.
  (let+ (((location type name predicate) args))
    (error 'xpath-creation-error
           :location  location
           :type      type
           :name      name
           :predicate predicate)))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      (eql :node))
                                 (predicate (eql nil))
                                 &rest predicates)
  (assert (null predicates))
  (create-xpath-element location type "somenode" predicate))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      (eql '*))
                                 (predicate (eql nil))
                                 &rest predicates)
  (assert (null predicates))
  (create-xpath-element location type "somenode" predicate))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      (eql :text))
                                 (predicate (eql nil))
                                 &rest predicates)
  (assert (null predicates))
  (let ((child (stp:make-text "text")))
    (stp:append-child location child)
    (list child)))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      t)
                                 (predicate integer)
                                 &rest predicates)
  (let ((children (create-xpath-element location type name nil)))
    (if predicates
        (apply #'create-xpath-element location type name predicates)
        children)))

(defmethod create-xpath-element :around ((location  stp:node)
                                         (type      (eql :child))
                                         (name      t)
                                         (predicate integer)
                                         &rest predicates)
  (declare (ignore predicates))

  (unless (= (1+ (stp:number-of-children location)) predicate)
    (error 'xpath-creation-error
           :location         location
           :type             type
           :name             name
           :predicate        predicate
           :format-control   "~@<Cannot create child node at position ~A.~@:>"
           :format-arguments `(,predicate)))
  (call-next-method))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      string)
                                 (predicate (eql nil))
                                 &rest predicates)
  (assert (null predicates))
  (let ((child (stp:make-element name)))
    (stp:append-child location child)
    (list child)))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      list)
                                 (predicate (eql nil))
                                 &rest predicates)
  (assert (null predicates))
  (let+ (((marker prefix local-name) name))
    (unless (eq marker :qname)
      (error 'xpath-creation-error
             :location         location
             :type             type
             :name             name
             :predicate        predicate
             :format-control   "~@<Invalid marker in qualified name ~
                                component list: ~S.~@:>"
             :format-arguments `(,marker)))
    (let ((child (stp:make-element
                  (concatenate 'string prefix ":" local-name)
                  (xpath::find-dynamic-namespace prefix))))
      (stp:append-child location child)
      (list child))))

(defmethod create-xpath-element ((location  stp:node)
                                 (type      (eql :child))
                                 (name      t)
                                 (predicate list)
                                 &rest predicates)
  ;; Create a child node satisfying PREDICATE.
  (let+ (;; First check whether LOCATION exists but does not satisfy
         ;; PREDICATE. If it exists, use it, otherwise, create it.
         (result   (xpath:evaluate
                    `(xpath:xpath (:path (,type ,name))) location))
         (children (if (xpath:node-set-empty-p result)
                       (create-xpath-element location type name nil)
                       (xpath:all-nodes result))))
    ;; Warn in case of ambiguity.
    (unless (length= 1 children)
      (warn "~@<Ignoring ~D sibling~:P after first child.~@:>"
            (1- (length children))))

    ;; Try to satisfy the requirement imposed by PREDICATE.
    (etypecase predicate
      ;; When predicate requires a path to exist, create the path.
      ((cons (eql :path))
       (create-xpath (first children) predicate))
      ;; When PREDICATE requires equality for a certain VALUE, just
      ;; store the required value.
      ((cons symbol (cons t (cons t)))
       (let+ (((relation path value) predicate)
              (values (first (create-xpath (first children) path))))
         (ecase relation
           (= (->xml value values 'string))))))

    ;; Process remaining PREDICATES, if any.
    (if predicates
        (apply #'create-xpath-element location type name
               (first predicates) (rest predicates))
        children)))

(flet ((add-attribute (location name)
         (let ((attribute (stp:make-attribute "" name)))
           (stp:add-attribute location attribute)
           (list attribute))))

  (defmethod create-xpath-element ((location  stp:element)
                                   (type      (eql :attribute))
                                   (name      string)
                                   (predicate (eql nil))
                                   &rest predicates)
    ;; Create an attribute node at LOCATION.
    (assert (null predicates))
    (add-attribute location name))

  (defmethod create-xpath-element ((location  stp:element)
                                   (type      (eql :attribute))
                                   (name      (eql '*))
                                   (predicate (eql nil))
                                   &rest predicates)
    ;; Create an attribute node at LOCATION.
    (assert (null predicates))
    (add-attribute location "someattribute")))
