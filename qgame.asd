;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: QGAME; Base: 10 -*- file: qgame.asd

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage qgame-asd
    (:use :cl :asdf)
    (:export #:*qgame-version*))

(in-package :qgame-asd)

(defparameter *qgame-version* "1.3.0")

(defsystem qgame
    :version #.*qgame-version*
    :author "Lee Spector <lspector@hampshire.edu>"
    :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
    :license "MIT"
    :description "QGAME: Quantum Gate and Measurement Emulator, a quantum computer simulator in Common Lisp"
    :serial t
    :depends-on (:gsll
                 :cl-ppcre
                 :cl-fad
                 :ltk)
    :components ((:file "packages")
                 (:file "qgame")
                 (:file "gate-compression")))

;; EOF
