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

(defvar %event-types% (make-hash-table))
(defvar %events% (make-hash-table))

(defclass ecore-event ()
  ((pointer :initform nil)
   (type :reader event-type :initarg :type)))

(defclass event-handler ()
  ((pointer)
   (callback :initarg :callback)))

(defgeneric event-handler-del (event-handler)
  (:documentation "Removes an event handler, once this function

- EVENT-HANDLER The event handler"))

(defvar *event* nil
  "Varibale used to get the current event when inside an event-handler callback")

(defmacro def-event-handler-callback (func)
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
	 (let ((*event* (gethash ,g-event %events%))
	       (,g-continue 1))
	   (handler-case
	       (progn
		 #|
		 (unless *event*			
		   (setf *event* (make-instance 'ecore-event :type ,g-event-type)))	     
		 |#
		 (funcall ,g-func))
	     (discard () (setf ,g-continue 0)))
	   ,g-continue)))))



(defmethod initialize-instance :after ((handler event-handler) &key type)
  (with-slots ((pointer pointer)	     
	       (callback callback))
      handler
    (let ((cb (def-event-handler-callback callback)))    
      (setf pointer
	    (foreign-funcall "ecore_event_handler_add"
			     :int type
			     :pointer (get-callback cb) 
			     :pointer (null-pointer) 
			     :pointer))
      (format t "Handler initialized ~%"))))

(defmacro make-event-handler (event-type callback)
  "Creates an event handler that will havle the event with the given callback.

Inside the callback you can access the current event type with EVENT-TYPE method of
the current event handler given by *EVENT-HANDLER*"
  `(make-instance 'event-handler 
		  :type ,event-type
		  :callback ,callback))

(defmethod event-handler-del ((handler event-handler))
  (with-slots ((pointer pointer))
      handler
    (when pointer
      (foreign-funcall "ecore_event_handler_del" 
		       :pointer pointer 
		       :pointer)
      (setf pointer nil))))

(defgeneric event-add (event)
  (:documentation "Adds an event to the event queue."))

(defgeneric event-del (event)
  (:documentation "Deletes an event from the event queue."))

(defmethod initialize-instance :after ((event ecore-event) &key)
  (with-slots ((pointer pointer))
      event
    (setf pointer (foreign-funcall "ecore_event_add" 
				   :int (event-type event)
				   :pointer (null-pointer) 
				   :pointer (null-pointer) 
				   :pointer (null-pointer) 
				   :pointer))))

(defmacro defevent (event-name superclasses (slots) &rest rest)
  `(progn 
     (unless (gethash ',event-name %event-types%)
       (setf (gethash ',event-name %event-types%)
	     (foreign-funcall "ecore_event_type_new" :int)))
     (defclass ,event-name (ecore-event ,@superclasses)
       (,slots)
       ,@rest)))

(defmethod event-type ((event ecore-event))
  (gethash (class-name (class-of event)) %event-types%))

(defmethod event-type ((event-name symbol))
  (or (gethash event-name %event-types%) 
      (error 'ecore-error :message (format nil "Undefined event ~a" event-name))))

(defmethod event-add ((event ecore-event))
  (with-slots ((pointer pointer))
      event
    (macrolet ((def-event-end-cb (event)
		 (let ((fname (intern  (symbol-name (gensym))))
		       (g-event (gensym))
		       (user-data (gensym))
		       (func-data (gensym)))
		   `(let ((,g-event ,event)) 
		      (defcallback ,fname :int
			  ((,user-data :pointer)
			   (,func-data :pointer))			
			(declare (ignore ,user-data ,func-data))
			(remhash (slot-value ,g-event 'pointer) %events%))))))
      (let ((cb (def-event-end-cb event)))
	(setf pointer (foreign-funcall "ecore_event_add"
				       :int (event-type event)
				       :pointer (null-pointer)
				       :pointer (null-pointer);(get-callback cb)
				       
				       :pointer (null-pointer)
				       :pointer))	
	(setf (gethash pointer %events%) event)))))

(defmethod event-del ((event ecore-event))
  (with-slots ((pointer pointer))
      event
    (when pointer
      (foreign-funcall "ecore_event_del"
		       :pointer pointer
		       :pointer))))
