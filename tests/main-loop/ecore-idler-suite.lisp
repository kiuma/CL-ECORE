;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: tests/eore-idler-suite.lisp $

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

(in-suite :ecore-idler)

(test (make-idle-enterer-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 
	  (make-poller
		 (lambda () 
		   (incf x)		   
		   (signal 'discard)))
	  (make-idler (lambda () 
			(incf x)
			(when (> x 1)
			  (ecore-loop-quit)))))	
	(is (< 1 x))
	(is (< 0 (elapsed start)))))

(test (make-idle-exiter-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 
	 (make-idler (lambda () 
		       (incf x)) :idler-type :exiter)
	  (make-poller
		 (lambda () 
		   (incf x)		   
		   (signal 'discard)))
	  (make-idler (lambda () 
			(incf x)
			(when (> x 1)
			  (ecore-loop-quit)))))	
	(is (< 2 x))
	(is (< 0 (elapsed start)))))

(test (make-idler-test :compile-at :definition-time)
      (let ((x 0)
	    (start (get-internal-real-time)))
	(in-ecore-loop 	 
	 (make-idler
		 (lambda () 
		   (incf x)	
		   (when (= x 4)
		     (ecore-loop-quit)))
		 :idler-type nil))	
	(is (= 4 x))
	(is (< 0 (elapsed start)))))
