;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: tests/eore-thread-suite.lisp $

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

(in-package :ecore-tests)

(in-suite :ecore-thread)

(test (make-thread-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 
	  (make-thread
		 (lambda () 
		   (setf *thread-data* 1))
		 :on-end (lambda () 
			   (setf x *thread-data*)
			   (ecore-loop-quit))))	
	(is (= 1 x))
	(is (< 0 (elapsed start)))))

(test (thread-notify-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 
	  (make-thread
		 (lambda () 
		   (setf *thread-data* 1)
		   (ecore-notify *ecore-object*))
		 :on-end (lambda () 
			   (ecore-loop-quit))
		 :on-notify (lambda ()
			      (setf x *thread-data*))))	
	(is (= 1 x))
	(is (< 0 (elapsed start)))))


(test (thread-limit-test :compile-at :definition-time)
      (let ((x 0)
	    (concurrent-threads 0)
	    (pending-threads 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop
	 (dotimes (i (* 2 *max-running-threads*))
	   (make-thread
	    (lambda () 
	      (ecore-notify *ecore-object*))
	    :on-end (lambda () 
		      (incf x)
		      (when (= x (* 2 *max-running-threads*))
			(ecore-loop-quit)))
	    :on-notify (lambda ()
			 (setf concurrent-threads 
			       (max concurrent-threads (ecore-running-threads))
			       pending-threads 
			       (max pending-threads (ecore-pending-threads)))))))	
	(is (= (* 2 *max-running-threads*) x))
	(is (= *max-running-threads* concurrent-threads))
	(is (< 0 pending-threads))
	(is (< 0 (elapsed start)))))


