;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: tests/eore-timer-suite.lisp $

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

(in-suite :ecore-poller)

(test (make-poller-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 
	  (make-poller
		 (lambda () 
		   (incf x)
		   (ecore-loop-quit))
		 :interval 2))
	(is (=  1 x))
	(is (< 0 (elapsed start)))))

(test (del-poller-test :compile-at :definition-time)
  (let ((x 0)
	(y 0)
	(obj nil)
	(start (get-internal-real-time)))
    (in-ecore-loop 
      (let ((poller1 (make-poller
		      (lambda () 
			(incf x)))))
	(make-poller
	 (lambda ()
	   (setf obj *ecore-object*)
	   (incf y)
	   (when (= y 3)
	     (ecore-loop-quit))))
	(ecore-del poller1)))
    (is (=  x 0))
    (is (= y 3))
    (is (> (elapsed start) 0))
    (is (not (null obj)))))

(test (poller-interval-test :compile-at :definition-time)
  (let ((interval 0)
	(x 0)
	(start (get-internal-real-time)))
    (in-ecore-loop       
      (make-poller
       (lambda ()
	 (incf x)
	 (setf (poller-interval *ecore-object*) 2)
	 (when (= x 2)
	   (setf interval (poller-interval *ecore-object*))
	   (ecore-loop-quit)))))
    (is (=  2 x))
    (is (= 2 interval))
    (is (< 0 (elapsed start)))))
#||#
