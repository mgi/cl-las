;;; -*- Mode: Lisp -*-
(in-package :asdf-user)

(asdf:defsystem :cl-las
  :name "cl-las"
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :description "Library to manipulate LAS files"
  :serial t
  :depends-on (:alexandria :com.gigamonkeys.binary-data)
  :components ((:file "package")
	       (:file "geotiff")
               (:file "las")))
