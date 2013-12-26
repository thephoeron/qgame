;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: QGAME; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:qgame
    (:use :cl)
    (:export #:*qgame-version*
             #:quantum-system
             #:run-qsys
             #:execute-quantum-program
             #:test-quantum-program))

;; see asdf system definition
(defvar qgame:*qgame-version*
  #.qgame-asd::*qgame-version*)

;; EOF
