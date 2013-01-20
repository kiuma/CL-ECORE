;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-event.lisp $

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


(defclass ecore-event (ecore)
  ((event-name :reader event-name :initarg :event-name)))

(defclass event-handler (ecore)
  ((event-name :reader event-name :initarg :event-name)
   (event :reader ecore-event :initarg :event))
  (:default-initargs :event nil))

(defcallback event-end-callback :void
    ((user-data :pointer)
     (func-data :pointer))
  (declare (ignore user-data))
  (let* ((*ecore-object* (ecore-object-from-data-pointer func-data)))
    (when *ecore-object*
      (with-slots ((pointer pointer)
		   (object-cb object-cb))
	  *ecore-object*      
	(setf pointer nil)
	(when object-cb
	  (funcall object-cb))))
    (ecore-del *ecore-object*)))

(defmethod initialize-instance :after ((event ecore-event) &key)
  (with-slots ((pointer pointer)
	       (data-pointer data-pointer)
	       (event-name event-name))
      event
    (setf pointer (ffi-ecore-event-add
		   (event-type event-name)
		   data-pointer
		   (callback event-end-callback)
		   (null-pointer)))))


(defcallback event-handler-callback :int
    ((data :pointer)
     (type :int)
     (event-pointer :pointer))
  (declare (ignore type))
  (let ((*ecore-object* (ecore-object-from-data-pointer data))
	(continue 1))
    (when *ecore-object*
      (with-slots ((object-cb object-cb)
		   (event event))
	  *ecore-object*
	(handler-case
	    (progn
	      (setf event
		    (ecore-object-from-data-pointer event-pointer))
	      (when object-cb	    
		(funcall object-cb)))
	  (discard () 
	    (setf continue 0)))))
    continue))

(defmethod initialize-instance :after ((handler event-handler) &key)
  (with-slots ((pointer pointer)
	       (event-name event-name)
	       (data-pointer data-pointer))
      handler    
    (setf pointer (ffi-ecore-event-handler-add 
		   (event-type event-name)
		   (callback event-handler-callback)
		   data-pointer))))

(defun make-event-handler (event cb)
  "Creates an event handler that will havle the event with the given callback.

Inside the callback you can access the current event name with EVENT-NAME method of
the current event handler given by *ECORE-OBJECT* variable"
  (make-instance 'event-handler 
		 :event-name event
		 :object-cb cb))

(defmethod ecore-del :after ((handler event-handler))
  (with-slots ((pointer pointer))
      handler
    (when pointer
      (ffi-ecore-event-handler-del pointer)
      (setf pointer nil))))

(defun defevent (event-name)
  (unless (gethash event-name *event-types*)
	 (setf (gethash event-name *event-types*) 
	       (ffi-ecore-event-type-new))))

(defmethod event-type ((event ecore-event))
  (event-type (event-name event)))

(defmethod event-type ((event-name symbol))
  (or (gethash event-name *event-types*)
      (error 'ecore-error :message (format nil "Undefined event ~a" event-name))))

(defun make-event (event-name &optional data end-cb)
  (make-instance 'ecore-event
		 :event-name event-name
		 :object-cb end-cb
		 :data data))

(defmethod ecore-del :after ((event ecore-event))
  (with-slots ((pointer pointer))
      event
    (when pointer
      (ffi-ecore-event-del pointer)
      (setf pointer nil))))


(defclass event-filter (ecore) 
  ((event :reader ecore-event :initarg :event)
   (event-type :reader event-type :initarg :event-type))
  (:default-initargs :event nil :event-type nil))

(defcallback filter-callback :int
    ((data :pointer)
     (loop-data :pointer)
     (type :int)
     (event-pointer :pointer))
  (declare (ignore loop-data))
  (let ((*ecore-object* (ecore-object-from-data-pointer data)))
    (when *ecore-object*
      (with-slots ((event event)
		   (event-type event-type)
		   (object-cb object-cb))
	  *ecore-object*
	(setf event
	      (ecore-object-from-data-pointer event-pointer)
	      event-type type)
	(let ((continue 1))
	  (handler-case
	      (funcall object-cb)
	    (discard () 
	      (setf continue 0)))
	  continue)))))

(defmethod initialize-instance :after ((filter event-filter) &key)
  (with-slots ((pointer pointer)
	       (data-pointer data-pointer))
      filter
    (setf pointer (ffi-ecore-event-filter-add
		   (null-pointer)		     
		   (callback filter-callback)
		   (null-pointer)
		   data-pointer))))

(defun make-event-filter (filter-cb)
  "Add a filter the current event queue.

- FILTER-CB a callback that is fired before event handlers.
To discard the event, signal a DISCARD condition.

"
  (make-instance 'event-filter
		 :object-cb filter-cb))

(defmethod ecore-del :after ((filter event-filter))
  (with-slots ((pointer pointer))
      filter
    (when pointer
      (ffi-ecore-event-filter-del pointer)      
      (setf pointer nil))))
