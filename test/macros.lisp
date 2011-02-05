;;; macros.lisp --- Unit tests for the `with-locations' macro.
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

(in-package :cxml-location.test)

(deftestsuite macros-root (root)
  ((simple-document (cxml:parse "<bla foo='1 2 4' BAR='baz'>foo</bla>"
				(stp:make-builder))))
  (:documentation
   "Unit tests for the `with-locations' macro."))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-locations' macro.")
  smoke

  (with-locations (((:name name)                          "node()")
		   (text                                  "bla/text()")
		   ((:@ (foo "foo") :type '(list number)) "node()")
		   ((@ bar)                               "node()"))
      simple-document
    ;; Extract values from generalized variables
    (ensure-same name "bla"
		 :test #'string=)
    (ensure-same foo '(1 2 4)
		 :test #'equal)
    (ensure-same bar "baz"
		 :test #'string=)
    (ensure-same text "foo"
		 :test #'string=)

    ;; Set values of generalized variables
    (setf name "frooble"
	  foo  '(5 6)
	  bar  42
	  text "bubba"))
  (ensure-same
   (with-output-to-string (stream)
     (stp:serialize simple-document
		    (cxml:make-character-stream-sink
		     stream :omit-xml-declaration-p t)))
   "<frooble foo=\"5 6\" BAR=\"42\">bubba</frooble>"
   :test #'string=))
