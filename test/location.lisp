;;;; location.lisp --- Unit tests for the singleton-location class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:xml.location.test)

(deftestsuite location-root (root)
  ((simple-document "<bla a='1'><bli b='5'>umumum</bli><bli/></bla>"))
  (:documentation
   "Root test suite for test of location class."))

(addtest (location-root
          :documentation
          "Smoke test for location class.")
  smoke

  (ensure (loc simple-document "bla/bli/text()"))

  (ensure-condition 'empty-result-set
    (loc simple-document "no-such-node")))

(addtest (location-root
          :documentation
          "Test name accessor of the `singleton-location' class.")
  name

  (let ((loc (loc simple-document "/bla/bli"
                  :namespaces '(&default ("foo" . "http://foo.org/bar")))))
    (ensure-same (name loc) "bli"
                 :test #'string=)

    (setf (name loc) "blup")
    (ensure-same (name loc) "blup"
                 :test #'string=)

    ;; A name with a colon should be treated as a qualified name.
    (setf (name loc) "foo:blup")
    (ensure-same (name loc) "blup"
                 :test #'string=)
    (ensure-same (name loc :prefix? t) "foo:blup"
                 :test #'string=)

    ;; A list of three strings should be treated as a qualified name.
    (setf (name loc) '("local" "prefix" "http://url.org/"))
    (ensure-same (name loc) "local"
                 :test #'string=)
    (ensure-same (name loc :prefix? t) "prefix:local"
                 :test #'string=)))

(addtest (location-root
          :documentation
          "Test attribute accessor of the `singleton-location'
class.")
  attribute

  (let ((loc (loc simple-document "/bla/bli")))
    (ensure-same (@ loc "b") "5")

    (setf (@ loc "b") "6")
    (ensure-same (@ loc "b") "6")

    (ensure-condition 'error ; TODO(jmoringe): proper condition
      (@ loc "attribute-does-not-exist"))))

(addtest (location-root
          :documentation
          "Test `loc' reader.")
  loc

  (let* ((loc   (loc "<foo><bar baz='bla'/></foo>" "node()"))
         (child (loc loc "bar")))
    (ensure-same
     (name child) "bar"
     :test #'string=)
    (ensure-same
     (@ child "baz") "bla"
     :test #'string=)))

(addtest (location-root
          :documentation
          "Test namespace handling in locations.")
  namespaces

  (let ((loc (loc "<bla/>" "node()"
                  :namespaces '(&default ("fb" . "http://foo.org/bar")))))
    (ensure-same
     (location-namespaces loc)
     '((nil     . "")
       ("xmlns" . "http://www.w3.org/2000/xmlns/")
       ("xml"   . "http://www.w3.org/XML/1998/namespace")
       ("fb"    . "http://foo.org/bar"))
     :test #'equal))

  (let ((loc (loc "<bla xmlns='http://foo.org/bar' xmlns:fb='http://foo.org/bar' a='1'><bli b='5'>umumum</bli><fb:bli/></bla>"
                  "/fb:bla/fb:bli"
                  :if-multiple-matches :all
                  :namespaces '(&default ("fb" . "http://foo.org/bar")))))
    (ensure-same (name loc)            '("bli" "bli"))
    (ensure-same (name loc :prefix? t) '("bli" "fb:bli")))

  (let ((loc (loc "<fb:bla xmlns:fb='http://foo.org/bar' a='1'><bli b='5'>umumum</bli><bli/></fb:bla>"
                  "/fb:bla"
                  :namespaces '(&default ("fb" . "http://foo.org/bar")))))
    (ensure-same (name loc)            "bla")
    (ensure-same (name loc :prefix? t) "fb:bla"))

  ;; Test setting the element's name to a qualified name.
  (let ((loc (loc "<bla/>" "node()"
                  :namespaces '(&default ("foo" . "http://bar.bzr")))))
    (setf (name loc) "foo:bar")
    (ensure-same
     (name loc :prefix? t) "foo:bar"
     :test #'string=)))
