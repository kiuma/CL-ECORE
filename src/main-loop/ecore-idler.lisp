;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-idler.lisp $

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


(defclass idler (ecore)
  ((type :initarg :type))
  (:default-initargs :type :enterer))

(defmethod initialize-instance :after ((idler idler) &key)
  (with-slots ((pointer pointer)
	       (data-pointer data-pointer)
	       (type type))
      idler
    (setf pointer (apply (case type
			   (:enterer #'ffi-ecore-idle-enterer-add)
			   (:exiter #'ffi-ecore-idle-exiter-add)
			   (t #'ffi-ecore-idler-add))
			 (list (callback task-callback) data-pointer)))))


(defun make-idler (job &key (idler-type :enterer))
  "Idler allows for callbacks to be called when the program isn't handling events, timers, pollers or fd handlers.

There are three types of idlers: Enterers, Exiters and Idlers(proper) that can be chosen with ILDER-TYPE keyword.

\(Idler callbacks are called when the main loop has called all enterer handlers. They are useful for interfaces that require polling and timers would be too slow to use.\)

JOB - The callback for the idler
IDLER-TYPE - May assume values of :ENTERER :EXITER or NIL
" 
  (make-instance 'idler
		 :type idler-type
		 :object-cb job))

(defmethod ecore-del :after ((idler idler))
  (with-slots ((pointer pointer)
	       (type type))
      idler
    (when pointer
      (case type 
	(:enterer (ffi-ecore-idle-enterer-del pointer))
	(:exiter (ffi-ecore-idle-exiter-del pointer))
	(t (ffi-ecore-idler-del pointer)))
      (setf pointer nil))))

