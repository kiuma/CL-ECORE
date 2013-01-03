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

(in-suite :ecore-timer)

(test (make-etimer-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 
	  (make-etimer
		 (lambda () 
		   (incf x)
		   (when (= x 9)
		     (ecore-loop-quit)))
		 :timeout 0.1))
	(is (=  9 x))
	(is (> 1.1 (elapsed start)))))

(test (slow-down-timer :compile-at :definition-time)
  (flet ((delay (tmr seconds)
	   (timer-delay tmr seconds)))
    (let ((x 0)
	  (timer)
	  (start (get-internal-real-time)))
      (in-ecore-loop 
	(setf timer (make-etimer
		     (lambda () 
		       (incf x)
		       (when (= x 1)
			 (delay timer 0.5))
		       (when (= x 2)			 
			 (ecore-loop-quit)))
		     :timeout 0.1)))
      (is (> (elapsed start) 0.5)))))


(defun ecore-test-function (func)
  (let ((x 0)
	(timer)
	(func-test)
	(start (get-internal-real-time)))
    (in-ecore-loop 
      (setf timer (make-etimer
		   (lambda () 
		     (incf x)
		     (when (= x 1)
		       (setf func-test (funcall func timer)))
		     (when (= x 2)
		       (ecore-loop-quit)))
		   :timeout 0.1)))
    (when (and func-test (eql (type-of func-test) 'function))
      (funcall func-test))
    (is (> (elapsed start) 0))))

(test (timer-reset-call :compile-at :definition-time)
  (ecore-test-function (lambda (timer) (timer-reset timer))))

(test (timer-pending-call :compile-at :definition-time)
  (ecore-test-function (lambda (timer) (timer-pending timer))))

(test (timers-precision-call :compile-at :definition-time)
  (ecore-test-function (lambda (timer) 
			 (declare (ignore timer))
			 (timers-precision-setf 1)
			 (lambda () (is (= (timers-precision) 1))))))

(test (timer-freeze-and-thaw-call :compile-at :definition-time)
  (ecore-test-function (lambda (timer) 
			 (timer-freeze timer)
			 (timer-thaw timer))))

(test (timer-interval-call :compile-at :definition-time)
  (ecore-test-function (lambda (timer) 
			 (setf (timer-interval timer) 0.5)
			 (let ((interval (timer-interval timer))
			       (start-interval-change (get-internal-real-time)))
			   (lambda () 
			     (is (= interval 0.5))
			     (is (>= (elapsed start-interval-change) 0.5)))))))

(test (timer-del-call :compile-at :definition-time)
  (ecore-test-function (lambda (timer) 
			 (ecore-del timer)
			 (ecore-loop-quit)
			 (is (null (slot-value timer 'ecore::pointer))))))
