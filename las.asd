;;; -*- Mode: Lisp -*-
(in-package :asdf-user)

(asdf:defsystem :las
  :name "LAS"
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :description "Library to manipulate LAS files"
  :serial t
  :depends-on (:binary-types)
  :components ((:file "package")
               (:file "las")))
