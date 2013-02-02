;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-helper.lisp $

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

(defvar *event-type-quit* 'ecore-quit-event)

(defun ecore-object-from-data-pointer (data-pointer)
  (gethash (cffi-sys:pointer-address data-pointer) *ecore-objects*))

(defun %ecore-loop-quit% ()
  (loop for key being the hash-key of *ecore-objects* 
	using (hash-value ecore-object)
	do (ecore-del ecore-object))
  (ffi-ecore-main-loop-quit))

(defun ecore-loop-quit (&optional as-signal-p)
  (if as-signal-p
      (make-event 'ecore-quit-event 
		  :end-cb (lambda () (%ecore-loop-quit%)))
      (%ecore-loop-quit%)))

(defun %in-ecore-loop% (func)
  (ecore-init)
  (mapcar #'funcall ecore.sys::*ecore-init-functions*)
  (make-idler (lambda ()
		(loop for q = *short-life-ecore-objects*
		      while (> (queue-size q) 0)
		      do (ecore-del (pop-queue q)))))
  (defevent *event-type-quit*)
  (funcall func)
  (main-loop-begin)
  (mapcar #'funcall ecore.sys::*ecore-shutdown-functions*)
  (ecore-shutdown))

(defmacro in-ecore-loop (&body body)
  `(%in-ecore-loop% (lambda () ,@body)))


;; ======================== EINA ======================================== ;;
#|
(in-package :ecore.sys)

(defun from-eina-list (eina-list transform)
  (loop for item = eina-list then (ecore::ffi-eina-list-next eina-list)
	while (not (null-pointer-p item))
	collect (funcall transform (foreign-slot-value item 'ecore::eina-list 'ecore::data))))
|#
