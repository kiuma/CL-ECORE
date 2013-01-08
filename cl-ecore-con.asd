;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: cl-ecore-conn.asd $

;;; Copyright (c) 2012, Andrea Chiumenti.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(cl:eval-when (:load-toplevel :execute)
    (asdf:load-system 'cffi-grovel))

(asdf:defsystem :cl-ecore-con
  :name "cl-ecore-con"
  :author "Andrea Chiumenti"
  :description "Bindings for ECORE enlightenment library. - Connections"
  :depends-on (:cffi :cl-ecore)
  :components ((:module "src"
		:components ((:module "con"
			      :components ((:file "packages")
					   (cffi-grovel:grovel-file "grovel" :depends-on ("packages"))
					   (:file "ffi" :depends-on ("grovel"))
					   (:file "ecore-con" :depends-on ("packages" "ffi"))))))))


(defmethod asdf:perform ((op asdf:test-op) (sys (eql (asdf:find-system :cl-ecore-con))))
  (asdf:oos 'asdf:test-op :cl-ecore-con-tests))

(defmethod asdf:operation-done-p ((op asdf:test-op) (sys (eql (asdf:find-system :cl-ecore-con))))
  nil)
