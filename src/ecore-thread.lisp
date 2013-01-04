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

(defvar *wakeup* (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *thread-data*)

(defclass ecore-thread (ecore-job) 
  ())

(defmacro %make-thread (job data)
  (let ((g-job (gensym))
	(g-data (gensym)))
    `(let ((,g-job ,job)
	   (,g-data ,data)) 
       (make-instance 'ecore-thread
		      :callback ,g-job
		      :data ,g-data))))

(defun make-thread (job &key on-end initial-data)
  "Schedule a task to run in a parallel thread to avoid locking the main loop.

- JOB blocking function
- ON-END Notifies main loop when JOB has completed"
  (let ((pipe (and on-end (make-pipe (lambda ()
				       (let ((*thread-data* (slot-value *ecore-object* 'data)))
					 (funcall on-end))
				       (ecore-del *ecore-object*))))))
    (%make-thread (lambda () 
		    (let ((*thread-data* initial-data))
		      (bt:make-thread (lambda ()  
					(funcall job)
					(when pipe
					  (setf (slot-value pipe 'data) *thread-data*)
					  (pipe-write pipe *wakeup*))))))
		  initial-data)))

