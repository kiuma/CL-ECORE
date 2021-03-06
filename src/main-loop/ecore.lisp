;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore.lisp $

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

(in-package :ecore.sys)

(defvar *ecore-init-functions* ())
(defvar *ecore-shutdown-functions* ())

(defun add-ecore-init-function  (func)
  (pushnew func *ecore-init-functions*))

(defun add-ecore-shutdown-function  (func)
  (pushnew func *ecore-shutdown-functions*))

;; =============================================================================
;; =============================================================================
;; =============================================================================

(in-package :ecore)

(defvar *system-events* '(:NULL :USER :HUP :EXIT :POWER :REALTIME))
(defvar *event-types* nil)

(defgeneric queue-size (queue))
(defgeneric push-queue (queue item))
(defgeneric pop-queue (queue))
(defgeneric queue-as-list (queue))
(defgeneric list-as-queue (items))

(defclass queue ()
  ((head :initform nil)
   (tail :initform nil)
   (size :initform 0 :reader queue-size)))

(defclass queue-item ()
  ((value :initarg :value)
   (next :initform nil)))

(defun make-queue ()
  (make-instance 'queue))

(defmethod push-queue ((q queue) item)
  (with-slots ((head head)
	       (tail tail)
	       (size size))
      q
    (let ((queue-item (make-instance 'queue-item
				     :value item)))
      (when tail
	(setf (slot-value tail 'next) queue-item))
      (setf tail queue-item)
      (unless head
	(setf head queue-item))
      (incf size))))

(defmethod pop-queue ((q queue))
  (with-slots ((head head)
	       (tail tail)
	       (size size))
      q
    (when head
      (decf size)
      (let ((result (slot-value head 'value))
	    (new-head (slot-value head 'next)))
	(setf head new-head)
	result))))

(defmethod queue-as-list ((q queue))
  (loop for item = (pop-queue q) then (pop-queue q)
	while (> (queue-size q) 0)
	collect item))

(defmethod list-as-queue ((items list))
  (let ((queue (make-queue)))
    (mapcar (lambda (item) (push-queue queue item)) items)
    queue))


(defun init-event-types ()  
  (setf *event-types* (make-hash-table :test 'equal))
  (loop for ev in ecore::*system-events*
	for i = 0 then (incf i)
	do (setf (gethash ev *event-types*) i
		 (gethash i *event-types*) ev)))

(defvar *ecore-objects* (make-hash-table))
(defvar *short-life-ecore-objects* (make-queue))

(defvar *thread-queue* (make-queue))

(defvar *ecore-object* nil
  "Varibale used to get the current Ecore object when inside a timer callback")

(defvar *ecore-buffer* nil
  "Varibale used to get datain pipe and socket callbacks")

(defvar *ecore-buffer-size* 4096
  "Should be kept as a multiple of page size. 
The default of 4096 is the system page size of a i386.
Since Linux 2.6.11, the pipe maximum capacity is 65536 bytes")

(defvar *max-running-threads* 16
  "This is the maximum number of threads that may be running at the same time.
Threads creaded wile this limit is reached will be enqueued and run when the number decreases.

Making this number to high may have a drastic negative impact.")


(define-condition ecore-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (format stream "~a ~a~%" (type-of condition)
		     (message condition)))))

(define-condition discard (ecore-error) 
  ()
  (:documentation "- Inside a timer callback function, signal this condition to termiate the timer gracefully.

- For an EVENT-HANDLER callback, it will cease processing handlers for that particular event, so all handler set to handle that event type that have not already been called, will not be."))

(defgeneric ecore-pointer (ecore &optional error-if-null-p)
  (:documentation "When not null returns an Ecore_* pointer, it signals a ECORE-ERROR otherwise." ))
(defgeneric (setf ecore-pointer) (pointer ecore))

(defclass ecore ()
  ((pointer :initarg :pointer)
   (free-data-pointer-p :initarg :free-data-pointer-p)
   (data-pointer :initarg :data-pointer :reader ecore-data-pointer)
   (object-cb :initarg :object-cb :accessor object-cb)
   (data :initarg :data)
   (short-life-p :initarg :short-life-p))
  (:default-initargs :pointer nil 
		     :data nil 
		     :data-pointer nil
		     :free-data-pointer-p t		    
		     :object-cb nil
		     :short-life-p nil))

(defclass ecore-invalid () ())

(defmethod ecore-pointer ((ecore ecore) &optional (error-if-null-p t))
  (or (slot-value ecore 'pointer) (and error-if-null-p 
				       (signal 'ecore-error :message "Invalid ecore pointer"))))

(defmethod (setf ecore-pointer) (pointer (ecore ecore))
  (setf (slot-value ecore 'pointer) pointer
	;(gethash pointer *ecore-pointer-objects*) ecore
	))

(defgeneric do-hash-ecore-object (ecore pointer))

(defmethod do-hash-ecore-object ((ecore ecore) pointer)
  (setf (gethash (cffi-sys:pointer-address pointer) *ecore-objects*)
	ecore))

(defmethod initialize-instance :after ((ecore ecore) &key)
  (with-slots ((free-ecore-pointer-p free-ecore-pointer-p)
	       (data-pointer data-pointer)
	       (short-life-p short-life-p))
      ecore    
    (if data-pointer
	(setf free-ecore-pointer-p nil)
	(setf data-pointer (foreign-alloc :pointer 
					  :initial-element (null-pointer))))
    (do-hash-ecore-object ecore data-pointer)
    (when short-life-p
      (push-queue *short-life-ecore-objects* ecore))))

(defgeneric ecore-del (ecore)
  (:documentation "Removes an ecore object from the ecore main loop"))

(defmethod ecore-del ((may-be-null t))
  (declare (ignore may-be-null)))

(defmethod ecore-del ((ecore ecore))
  (with-slots ((free-data-pointer-p free-data-pointer-p)
	       (data-pointer data-pointer)
	       (pointer pointer))
      ecore
    (when (and data-pointer (not (null-pointer-p data-pointer)))
      (remhash (cffi-sys:pointer-address data-pointer) *ecore-objects*)
      (when free-data-pointer-p
	(foreign-free data-pointer)))
    (setf data-pointer nil)))



(defun main-loop-begin () (ffi-ecore-main-loop-begin))

(defun ecore-init () 
  (setf *ecore-objects* (make-hash-table)
	*short-life-ecore-objects* (make-queue)
	*thread-queue* (make-queue))
  (init-event-types)
  (ffi-ecore-init))

(defun ecore-shutdown ()
  (unless ecore.sys::*ecore-shutdown-functions*
    (ffi-ecore-shutdown)))



;;----------------------- CALBACKS HERE ---------------------------

(defcallback task-callback :int
    ((data :pointer))
  (let ((do-again 1)
	(*ecore-object* (ecore-object-from-data-pointer data)))
    (if *ecore-object*
	(handler-case
	    (let ((cb (slot-value *ecore-object* 'object-cb)))
	      (when cb
		(funcall cb)))
	  (ecore-error (e) 
	    (setf do-again 0)
	    (ecore-del *ecore-object*)
	    (and (not (typep e 'discard)) 
		 (progn (error e)))))
	(setf do-again 0))
    do-again))



