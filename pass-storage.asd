;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage pass-storage-system
  (:use :common-lisp :asdf))

(in-package :pass-storage-system)

(defsystem "pass-storage"
  :description "Password storage utility"
  :version "0.2"
  :author "Andrey Kutejko <andy128k@gmail.com>"
  :licence "LGPL"
  :depends-on (:cffi
               :trivial-garbage
               :cl-cffi-gtk
               :babel
               :ironclad
               :cl-z
               :s-xml
               :cl-binary-location
               :cl-fad
               :anaphora)
  :serial t
  :components ((:file "pass-storage.package")
               (:file "pass-storage.config")
               (:file "pass-storage.password")
               (:file "pass-storage.utils")
               (:file "pass-storage.xml")
               (:file "pass-storage.item")
               (:file "pass-storage.load")
               (:file "pass-storage.version")
               (:file "pass-storage.entropy")
               (:file "pass-storage.main")))

