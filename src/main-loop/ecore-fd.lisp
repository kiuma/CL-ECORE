;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-fd.lisp $

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

(defgeneric fd-flags (handler)
  (:documentation "Returns the flags used to monitor the handler file descriptor.
May contain any of the following values :FD-READ :FD-WRITE :FD-ERROR"))

(defgeneric (setf fd-flags) (keys handler)
  (:documentation "Sets the flags used to monitor the handler file descriptor.
KEYS - A list that may contain any of the following values :FD-READ :FD-WRITE :FD-ERROR"))

(defclass ecore-fd-handler (ecore) 
  ((fd :initarg :fd)
   (flags :initarg :flags))
  (:default-initargs :fd 0 :flags 0))

(defun %keys-as-fd-flags% (&rest keys)
  (let ((result 0))
    (mapcar (lambda (k)
	      (case k
		(:fd-read (setf result (boole boole-ior result 1)))
		(:fd-write (setf result (boole boole-ior result 2)))
		(:fd-error (setf result (boole boole-ior result 4)))
		(otherwise (error 'ecore-error :message (format nil "Fd monitoring unsupported key ~a. Use one of :FD-READ :FD-WRITE :FD-ERROR." k))))) 
	    keys)
    result))

(defun %fd-flags-as-keys% (fd-flags)
  (let ((result ()))
    (when (> (boole boole-ior fd-flags 1) 0)
      (push :fd-read result))
    (when (> (boole boole-ior fd-flags 2) 0)
      (push :fd-write result))
    (when (> (boole boole-ior fd-flags 4) 0)
      (push :fd-error result))
    result))

(defmethod fd-flags ((fd-handler ecore-fd-handler))
  (%fd-flags-as-keys% (slot-value fd-handler 'flags)))

(defmethod (setf fd-flags) (keys (fd-handler ecore-fd-handler))
  (let ((flags (%keys-as-fd-flags% keys)))
    (ffi-ecore-main-fd-handler-active-set (ecore-pointer fd-handler) flags)
    (setf (slot-value fd-handler 'flags) flags)))

(defmethod ecore-del :after ((handler ecore-fd-handler))
  (with-slots ((pointer pointer))
      handler    
    (when (and pointer (not (null-pointer-p pointer)))     
      (ffi-ecore-main-fd-handler-del pointer))))

(defcallback fd-hanlder-callback :int
    ((data :pointer)
     (fd-handler-pointer :pointer))
  (declare (ignore fd-handler-pointer))
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

(defmethod initialize-instance :after ((handler ecore-fd-handler) &key)
  (with-slots ((fd fd)
	       (flags flags)
	       (data-pointer data-pointer))
      handler
    (setf (ecore-pointer handler) (ffi-ecore-main-fd-handler-add 
				   fd
				   flags
				   (callback fd-handler-callback)
				   data-pointer
				   (null-pointer)
				   (null-pointer)))))


(defun make-fd-handler (file-descriptor fd-handler-cb fd-keys)
  "Adds a callback for activity on the given file descriptor. 
This function should _NOT_ be used for monitoring \"normal\" files, like text files. 

FD-HANDLER-CB callback executed when and activity is handled (given by FD-KEYS)
FD-KEYS - A list that may contain any of the following values :FD-READ :FD-WRITE :FD-ERROR"
  (make-instance 'ecore-fd-handler
		 :fd file-descriptor
		 :object-cb fd-handler-cb
		 :flags (%keys-as-fd-flags% fd-keys)))
