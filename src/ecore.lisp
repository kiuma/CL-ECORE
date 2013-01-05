;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/ecore.lisp $

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

(define-foreign-library libecore
  (:unix "libecore.so")
  (t (:default "libecore")))


(use-foreign-library libecore)
 
(defvar *ecore-objects-before* (make-hash-table))

(defvar *ecore-objects-after* (make-hash-table))

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

(defvar *thread-queue* (make-instance 'arnesi:queue))
#|
(defvar *callback-data*)
(defvar *callback-event-type*)
(defvar *callback-event*)

(defconstant event-signal-user 1)
(defconstant event-signal-hup 2)
(defconstant event-signal-exit 3)
(defconstant event-signal-power 4)
(defconstant event-signal-realtime 5)

(defmacro defecore-callback (fname &body body)
  (let ((data (gensym))
	(event-type (gensym))
	(event (gensym)))
    `(defcallback ,fname :int ((,data :pointer) (,event-type :int) (,event :pointer))
      (let ((*callback-data* ,data)
	    (*callback-event-type* ,event-type)
	    (*callback-event* ,event))
	,@body))))
|#  



(define-condition ecore-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (format stream "~a ~a~%" (type-of condition)
		     (message condition)))))

(define-condition discard (ecore-error) 
  ()
  (:documentation "- Inside a timer callback function, signal this condition to termiate the timer gracefully.

- For an EVENT-HANDLER callback, it will cease processing handlers for that particular event, so all handler set to handle that event type that have not already been called, will not be."))

(defclass ecore ()
  ((pointer :initarg :pointer)
   (delete-before-main-loop-quit :initarg :delete-before-main-loop-quit)
   (do-not-hash-p :initarg :do-not-hash)
   (data :initarg :data))
  (:default-initargs :pointer nil :data nil :do-not-hash nil :delete-before-main-loop-quit nil))

(defclass ecore-invalid () ())

(defmethod initialize-instance :after ((ecore ecore) &key)
  (with-slots ((before delete-before-main-loop-quit)
	       (do-not-hash-p do-not-hash-p))
      ecore    
    (unless do-not-hash-p
      (setf (gethash ecore (if before
			       *ecore-objects-before*
			       *ecore-objects-after*)) t))))

(defgeneric ecore-pointer (ecore)
  (:documentation "When not null returns an Ecore_* pointer, it signals a ECORE-ERROR otherwise." ))

(defmethod ecore-pointer ((ecore ecore))
  (or (slot-value ecore 'pointer) (signal 'ecore-error :message "Invalid ecore pointer")))

(defgeneric ecore-del (ecore)
  (:documentation "Removes an ecore object from the ecore main loop"))

#|
(defmethod ecore-del :after ((ecore ecore))
  (change-class ecore 'ecore-invalid))
|#

(defmethod ecore-del ((ecore ecore))
  (with-slots ((before delete-before-main-loop-quit)
	       (do-not-hash-p do-not-hash-p))
      ecore
    (unless do-not-hash-p
      (remhash ecore (if before
			 *ecore-objects-before*
			 *ecore-objects-after*)))))

(defmacro def-task-callback (func ecore)
  (let ((fname (intern  (symbol-name (gensym))))
	(data (gensym))
	(do-again (gensym))
	(e (gensym))
	(g-ecore (gensym))
	(g-func (gensym)))
    `(let ((,g-ecore ,ecore)
	   (,g-func ,func)) 
       (defcallback ,fname :int
	   ((,data :pointer))
	 (declare (ignore ,data))
	 (let ((,do-again 1))
	   (handler-case
	       (let ((*ecore-object* ,g-ecore))
		 (funcall ,g-func))
	     (ecore-error (,e) 
	       (setf ,do-again 0
		     (slot-value ,g-ecore 'pointer) nil) 
	       (and (not (typep ,e 'discard)) 
		    (progn (error ,e)))))
	   ,do-again)))))


(defctype eina-true :int 1)
(defctype eina-false :int 0)

(defcfun ("ecore_main_loop_begin" main-loop-begin) :void)

(defun ecore-init () 
  (foreign-funcall "ecore_init" :void))


(defun ecore-shutdown ()
  (loop for obj being the hash-key of *ecore-objects-before*
	do (ecore-del obj))
  (loop for obj being the hash-key of *ecore-objects-after*
	do (ecore-del obj))
  (foreign-funcall "ecore_shutdown" :void))

(defun ecore-loop-quit ()
  (loop for obj being the hash-key of *ecore-objects-before*
	do (ecore-del obj))
  (foreign-funcall "ecore_main_loop_quit" :void)
  #|(loop for obj being the hash-key of *ecore-objects-after*
	do (ecore-del obj))|#)

(defmacro in-ecore-loop (&body body)
  (let ((fname (gensym))
	(cb (gensym)))
    `(flet ((,fname (,cb)
	     (ecore-init)
	     (funcall ,cb)
	     (main-loop-begin)
	     (ecore-shutdown)))
      (,fname (lambda () ,@body)))))

