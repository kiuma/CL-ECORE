;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: cl-ecore-tests.asd $

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

(asdf:defsystem :cl-ecore-tests
  :name "cl-ecore"
  :author "Andrea Chiumenti"
  :description "Bindings for ECORE enlightenment library."
  :depends-on (:fiveam :cl-ecore)
  :components ((:module tests
                        :components ((:file "packages")
				     (:file "defsuites" :depends-on ("packages"))
				     (:file "utils" :depends-on ("packages"))
                                     (:file "ecore-timer-suite" 
				      :depends-on ("packages" "defsuites" "utils"))
				     (:file "ecore-event-suite" 
				      :depends-on ("packages" "defsuites" "utils"))
				     (:file "ecore-poller-suite" 
				      :depends-on ("packages" "defsuites" "utils"))
				     (:file "ecore-idler-suite" 
				      :depends-on ("packages" "defsuites" "utils"))
				     (:file "ecore-job-suite" 
				      :depends-on ("packages" "defsuites" "utils"))))))


(defmethod asdf:perform ((op asdf:test-op) (sys (eql (asdf:find-system :cl-ecore-tests))))
  (asdf:oos 'asdf:load-op :cl-ecore-tests)
  (time (funcall (intern (symbol-name '#:run!) '#:5am) :ecore)))

(defmethod asdf:operation-done-p ((op asdf:test-op) (sys (eql (asdf:find-system :cl-ecore-tests))))
  nil)
