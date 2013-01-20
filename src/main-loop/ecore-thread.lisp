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

(defvar *notify* (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *thread-data* nil)

(defvar *running-threads* 0)

(defclass ecore-thread (ecore) 
  ((pipe :initarg :pipe)
   (job :initarg :job)
   (data :initarg :data)
   (on-end-callback :initarg :on-end)
   (on-notify-callback :initarg :on-notify)
   (notify-lock :initarg :notify-lock)
   (notify-condition :initarg :notify-condition)
   (cancelled-p :initarg :cancelled-p)
   (running-p :initarg :running-p :reader thread-running-p))
  (:default-initargs
   :data nil
   :job nil
   :pipe nil
   :cancelled-p nil
   :running-p nil
   :notify-lock (bt:make-lock) 
   :notify-condition (bt:make-condition-variable)))

(defgeneric ecore-notify (thread)
  (:documentation "Notifies a change status to the main thread with the *THREAD-DATA* variable"))

(defgeneric thread-run (thread)
  (:documentation "Activates a thread"))

(defmethod thread-run ((thread ecore-thread))
  (incf *running-threads*)
  (with-slots ((cancelled-p cancelled-p)
	       (running-p running-p)
	       (pipe pipe)	       
	       (job job)
	       (data data)
	       (notify-lock notify-lock)
	       (notify-condition notify-condition)
	       (object-cb object-cb)
	       (on-end on-end-callback)
	       (on-notify on-notify-callback))
      thread
    (unless (or running-p cancelled-p job)      
      (setf running-p t
	    pipe (make-pipe (lambda ()
			      (bt:with-lock-held (notify-lock)
				(let ((*thread-data* (slot-value *ecore-object* 'data))
				      (*ecore-object* thread))
				  (if (= (aref *ecore-buffer* 0) 0)
				      (progn
					(when on-end 
					  (funcall on-end))
					(decf *running-threads*)
					(dequeue-threads)
					(setf running-p nil)
					(ecore-del thread))
				      (when on-notify
					(funcall on-notify))))
				(bt:condition-notify notify-condition))))
	    job (make-job (lambda () 
			    (bt:make-thread 
			     (lambda ()  
			       (let ((*thread-data* data)
				     (*ecore-object* thread))
				 (funcall object-cb)  
				 (with-slots ((pipe pipe) 
					      (notify-lock notify-lock)
					      (notify-condition notify-condition))
				     thread
				   (setf (slot-value pipe 'data) *thread-data*
					 (aref *notify* 0) 0)
				   (pipe-write pipe *notify*)
				   (bt:with-lock-held (notify-lock)
				     (bt:condition-wait notify-condition notify-lock))))))))))))


(defun make-thread (job &key on-end on-notify initial-data)
  "Schedule a task to run in a parallel thread to avoid locking the main loop.

- JOB blocking function
- ON-END Notifies main loop when JOB has completed
- ON-NOTIFY Callback called when you want to update the main thread status \(using the *THREAD-DATA* exchange variable\) and calling ECORE-NOTIFY from the JOB calllback function"
  (let ((thread (make-instance 'ecore-thread
			       :object-cb job
			       :data initial-data
			       :on-end on-end
			       :on-notify on-notify)))
    (if (< *running-threads* *max-running-threads*)
	(thread-run thread)
	(arnesi:enqueue *thread-queue* thread))))

(defun dequeue-threads ()
  (loop 
    while (and (not (arnesi:queue-empty-p *thread-queue*))
	       (< *running-threads* *max-running-threads*))
    do (thread-run (arnesi:dequeue *thread-queue*))))

(defmethod ecore-del :after ((thread ecore-thread))
  (with-slots ((pipe pipe)
	       (job job)
	       (running-p running-p))
      thread
    (when (not running-p)
      (if job
	  (mapcar #'ecore-del (list pipe job))
	  (let ((new-queue (make-instance 'arnesi:queue)))
	    (loop
	      while (not (arnesi:queue-empty-p *thread-queue*))
	      do (let ((q-thread (arnesi:dequeue *thread-queue*)))
		   (when (not (eq thread q-thread))
		     (arnesi:enqueue new-queue q-thread))))
	    (setf *thread-queue* new-queue)))
      thread)))

(defmethod ecore-notify ((thread ecore-thread))
  "Notifies changes to the main thread thought the *THREAD-DATA* variable"
  (with-slots ((pipe pipe)
	       (notify-lock notify-lock)
	       (notify-condition notify-condition))
      thread
    (setf (slot-value pipe 'data) *thread-data*
	  (aref *notify* 0) 1)
    (pipe-write pipe *notify*)
    (bt:with-lock-held (notify-lock)
      (bt:condition-wait notify-condition notify-lock))))

(defun ecore-running-threads ()
  *running-threads*)

(defun ecore-pending-threads ()
  (arnesi:queue-count *thread-queue*))
