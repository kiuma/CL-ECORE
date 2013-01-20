;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-job.lisp $

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

(in-package :ecore)


(defclass ecore-job (ecore)
  ())

(defcallback job-callback :int
    ((data :pointer))
  (let* ((*ecore-object* (ecore-object-from-data-pointer data)))
    (with-slots ((pointer pointer)
		 (object-cb object-cb))
	*ecore-object*
      (setf pointer nil)
      (when object-cb
	(funcall object-cb)))
    (ecore-del *ecore-object*))
  0)

(defmethod initialize-instance :after ((ecore-job ecore-job) &key)
  (with-slots ((pointer pointer)
	       (data-pointer data-pointer))
      ecore-job
    (setf pointer (ffi-ecore-job-add (callback job-callback)
				     data-pointer))))


(defun make-job (job)
  "You can queue jobs that are to be done by the main loop when the current event is dealt with.

Jobs are processed by the main loop similarly to events. They also will be executed in the order in which they were added.

A good use for them is when you don't want to execute an action immediately, but want to give the control back to the main loop so that it will call your job callback when jobs start being processed \(and if there are other jobs added before yours, they will be processed first\). This also gives the chance to other actions in your program to cancel the job before it is started.

- JOB callback to excute." 
  (make-instance 'ecore-job
		 :object-cb job))

(defmethod ecore-del :after ((ecore-job ecore-job))
  (with-slots ((pointer pointer))
      ecore-job    
    (when (and pointer (not (null-pointer-p pointer)))     
      (ffi-ecore-job-del pointer))))

