;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/ecore-job.lisp $

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
  ((callback :initarg :callback)))

(defmethod initialize-instance :after ((ecore-job ecore-job) &key)
  (with-slots ((pointer pointer)
	       (callback callback))
      ecore-job
    (flet ((job-add (job)
	     (let ((cb (def-task-callback job ecore-job)))	        
	       (foreign-funcall "ecore_job_add"	     
				:pointer (get-callback cb)
				:pointer (null-pointer)
				:pointer))))
      (setf pointer
	    (job-add (lambda () 
		       (unwind-protect
			    (funcall callback)
			 (setf pointer nil))))))))


(defmacro make-job (job)
  "You can queue jobs that are to be done by the main loop when the current event is dealt with.

Jobs are processed by the main loop similarly to events. They also will be executed in the order in which they were added.

A good use for them is when you don't want to execute an action immediately, but want to give the control back to the main loop so that it will call your job callback when jobs start being processed \(and if there are other jobs added before yours, they will be processed first\). This also gives the chance to other actions in your program to cancel the job before it is started.

- JOB callback to excute."
  (let ((g-job (gensym)))
    `(let ((,g-job ,job)) 
       (make-instance 'ecore-job
		      :callback ,g-job))))

(defmethod ecore-del :after ((ecore-job ecore-job))
  (with-slots ((pointer pointer))
      ecore-job
    (when pointer       
      (foreign-funcall "ecore_job_del" 
		       :pointer pointer 
		       :pointer)
      (setf pointer nil))))

