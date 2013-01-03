;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: tests/eore-event-suite.lisp $

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

(in-suite :ecore-event)

(test (exit-loop-on-user-event :compile-at :definition-time)
  (let ((event nil))
    (in-ecore-loop      
      (defevent quit-event () (x))
      (make-event-handler (event-type 'quit-event) 
			  (lambda ()
			    (setf event *ecore-object*)
			    (ecore-loop-quit)))
      (event-add (make-instance 'quit-event)))
    (is (not (null event))))) 

(test (event-filter-and-handler :compile-at :definition-time)
  (let ((filter-p nil)
	(event-p nil))
    (in-ecore-loop      
      (defevent quit-event () (x))
      (make-event-handler (event-type 'quit-event) 
			  (lambda ()			    
			    (setf event-p *ecore-object*)
			    (ecore-loop-quit)))
      (make-event-filter :before-event-cb (lambda () 
					    (when (and *ecore-object* 
						       (eql (event-type *ecore-object*)
							    (event-type 'quit-event)))
					      (setf filter-p t))))
      (event-add (make-instance 'quit-event)))
    (is (not (null event-p)))
    (is (not (null filter-p)))))


(test (event-filter-no-handler :compile-at :definition-time)
  (let ((filter-p nil)
	(event-p nil)
	(event-on-after-filter-p nil))
    (in-ecore-loop      
      (defevent quit-event () (x))
      (make-event-handler (event-type 'quit-event) 
			  (lambda ()			    
			    (setf event-p *ecore-object*)))
      (make-event-filter :before-event-cb (lambda () 
					    (when (and *ecore-object* 
						       (eql (event-type *ecore-object*)
							    (event-type 'quit-event)))
					      (setf filter-p t)
					      (signal 'discard)))
			 :after-event-cb (lambda () 
					   (when (and *ecore-object* 
						       (eql (event-type *ecore-object*)
							    (event-type 'quit-event)))
					      (setf event-on-after-filter-p t))
					   (ecore-loop-quit)))
      (event-add (make-instance 'quit-event)))
    (is (null event-p))
    (is (not (null filter-p)))
    (is (not (null event-on-after-filter-p)))))

