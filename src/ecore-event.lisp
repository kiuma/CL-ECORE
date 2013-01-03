;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/ecore-event.lisp $

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

(defvar %event-types% (make-hash-table :test 'equal))
(defvar %events% (make-hash-table))

(defclass ecore-event (ecore)
  ((holder :initform nil)
   (type :reader event-type :initarg :type))
  (:default-initargs :type -1))

(defclass event-handler (ecore)
  ((callback :initarg :callback)))

(defmethod initialize-instance :after ((handler event-handler) &key type)
  (with-slots ((pointer pointer)	     
	       (callback callback))
      handler
    (macrolet ((def-event-handler-callback (func)
		 (let ((fname (intern  (symbol-name (gensym))))
		       (g-data (gensym))
		       (g-event-type (gensym))
		       (g-event (gensym))	
		       (g-func (gensym))
		       (g-continue (gensym)))
		   `(let ((,g-func ,func)) 
		      (defcallback ,fname :int
			  ((,g-data :pointer)
			   (,g-event-type :int)
			   (,g-event :pointer))
			(declare (ignore ,g-data))	
			(let ((*ecore-object* (gethash (cffi-sys:pointer-address ,g-event) %events%))
			      (,g-continue 1))
			  (handler-case
			      (progn		 
				(unless *ecore-object*			
				  (setf *ecore-object* (make-instance 'ecore-event :type ,g-event-type)))
				(funcall ,g-func))
			    (discard () (setf ,g-continue 0)))
			  ,g-continue))))))
      (let ((cb (def-event-handler-callback callback)))    
	(setf pointer
	      (foreign-funcall "ecore_event_handler_add"
			       :int type
			       :pointer (get-callback cb) 
			       :pointer (null-pointer) 
			       :pointer))))))

(defmacro make-event-handler (event-type callback)
  "Creates an event handler that will havle the event with the given callback.

Inside the callback you can access the current event type with EVENT-TYPE method of
the current event handler given by *ECORE-OBJECT* variable"
  `(make-instance 'event-handler 
		  :type ,event-type
		  :callback ,callback))

(defmethod ecore-del ((handler event-handler))
  (with-slots ((pointer pointer))
      handler
    (when pointer
      (foreign-funcall "ecore_event_handler_del" 
		       :pointer pointer 
		       :pointer)
      (setf pointer nil))))

(defgeneric event-add (event)
  (:documentation "Adds an event to the event queue."))

(defmacro defevent (event-name superclasses slots &rest rest)
  `(progn 
     (let ((key (intern (symbol-name ',event-name))))
       (unless (gethash key %event-types%)
	 (setf (gethash key %event-types%)
	       (foreign-funcall "ecore_event_type_new" :int))))
     (defclass ,event-name (ecore-event ,@superclasses)
       ,slots
       ,@rest)))

(defmethod event-type ((event ecore-event))
  (event-type (class-name (class-of event))))

(defmethod event-type ((event-name symbol))
  (or (gethash event-name %event-types%)
      (error 'ecore-error :message (format nil "Undefined event ~a" event-name))))

(defmethod event-add ((event ecore-event))
  (with-slots ((pointer pointer)
	       (holder holder))
      event
    (macrolet ((def-event-end-cb (event)
		 (let ((fname (intern  (symbol-name (gensym))))
		       (g-event (gensym))
		       (user-data (gensym))
		       (func-data (gensym)))
		   `(let ((,g-event ,event)) 
		      (defcallback ,fname :void
			  ((,user-data :pointer)
			   (,func-data :pointer))			
			(declare (ignore ,user-data))
			(setf (slot-value ,g-event 'pointer) nil
			      (slot-value ,g-event 'holder) nil)
			(when (not (null-pointer-p ,func-data))
			  (remhash (cffi-sys:pointer-address ,func-data)  %events%)
			  (foreign-free ,func-data)))))))
      (let ((end-cb (def-event-end-cb event))
	    (event-holder (cffi:foreign-alloc :long :initial-element 1)))
	(setf pointer (foreign-funcall "ecore_event_add"
				       :int (event-type event)
				       :pointer event-holder
				       :pointer (get-callback end-cb)
				       :pointer (null-pointer)
				       :pointer)
	      holder event-holder)	
	(setf (gethash (cffi-sys:pointer-address event-holder) %events%) event)))))

(defmethod ecore-del ((event ecore-event))
  (with-slots ((pointer pointer))
      event
    (when pointer
      (foreign-funcall "ecore_event_del"
		       :pointer pointer
		       :pointer))))


(defclass event-filter (ecore) 
  ())

(defun make-event-filter (&key before-event-cb after-event-cb)
  "Add a filter the current event queue.

- BEFORE-EVENT-CB a calllback that is fired before event handlers.
To discard the event, signal a DISCARD condition.

- AFTER-EVENT-CB a callback ther is fired after event handlers are done.

Current event is accessible throught the *ECORE-OBJECT* variable"

  (macrolet ((make-start-cb ()
	       (let ((fname (intern  (symbol-name (gensym))))
		     (g-data (gensym)))
		 `(defcallback ,fname :pointer
		      ((,g-data :pointer))
		    (declare (ignore ,g-data))
		    (foreign-alloc :long))))
	     (make-before-cb (func)
	       (let ((fname (intern  (symbol-name (gensym))))		     
		     (g-data (gensym))
		     (g-loop-data (gensym))
		     (g-event-type (gensym))
		     (g-event-pointer (gensym))		     
		     (g-func (gensym))
		     (g-continue (gensym)))
		 `(let ((,g-func ,func)) 
		    (defcallback ,fname :int
			((,g-data :pointer)
			 (,g-loop-data :pointer)
			 (,g-event-type :int)
			 (,g-event-pointer :pointer))			
		      (declare (ignore ,g-data))		     
		      (let ((*ecore-object* (gethash (cffi-sys:pointer-address ,g-event-pointer) %events%))
			    (,g-continue 1))
			(handler-case
			    (progn		 
			      (unless *ecore-object*		
				(setf *ecore-object* (make-instance 'ecore-event :type ,g-event-type)
				      (gethash (cffi-sys:pointer-address ,g-event-pointer) %events%) 
				      *ecore-object*))
			      
			      (setf (mem-ref ,g-loop-data :long )
				    (cffi-sys:pointer-address ,g-event-pointer))
			      (funcall ,g-func))
			  (discard () (setf ,g-continue 0)))
			,g-continue)))))
	     (make-after-cb (func)
		 (let ((fname (intern  (symbol-name (gensym))))		       
		       (user-data (gensym))
		       (func-data (gensym))
		       (g-func (gensym)))
		   `(let ((,g-func ,func)) 
		      (defcallback ,fname :void
			  ((,user-data :pointer)
			   (,func-data :pointer))			
			(declare (ignore ,user-data))			 
			(let ((*ecore-object* (gethash (mem-ref ,func-data :long) %events%)))
			  (funcall ,g-func)))))))
    (make-instance 'event-filter
		   :pointer
		   (foreign-funcall "ecore_event_filter_add"
				    :pointer (get-callback (make-start-cb))
				    :pointer (or (and before-event-cb (get-callback (make-before-cb before-event-cb))) (null-pointer))
				    :pointer (or (and after-event-cb (get-callback (make-after-cb after-event-cb))) (null-pointer))
				    :pointer))))

(defmethod ecore-del ((filter event-filter))
  (with-slots ((pointer pointer))
      filter
    (when pointer
      (foreign-funcall "ecore_event_filter_del" 
		       :pointer pointer 
		       :pointer)
      (setf pointer nil))))
