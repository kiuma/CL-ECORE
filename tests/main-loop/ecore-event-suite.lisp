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

(test (event-filter-and-handler :compile-at :definition-time)
  (let ((quit-loop-on-event nil))
    (in-ecore-loop
      (setf quit-loop-on-event t)
      (ecore-loop-quit t))
    (is-true quit-loop-on-event)))


(test (exit-loop-on-user-event :compile-at :definition-time)
  (let ((event nil))
    (in-ecore-loop
      (defevent 'test-quit-event)
      (make-event-handler 'test-quit-event
			  (lambda ()
			    (setf event *ecore-object*)
			    (ecore-loop-quit t)))
      (make-event 'test-quit-event))
    (is (not (null event)))))

(test (event-filter-and-handler :compile-at :definition-time)
  (let ((filter-p nil)
	(event-p nil))
    (in-ecore-loop
      (defevent 'test-quit-event)
      (make-event-handler 'test-quit-event
			  (lambda ()
			    (setf event-p t)
			    (ecore-loop-quit)))
      (make-event-filter (lambda ()
			   (when (and (ecore-event *ecore-object*)
				      (eql (event-type *ecore-object*)
					   (event-type 'test-quit-event))
				      (eql (event-type *ecore-object*)
					   (event-type (ecore-event *ecore-object*))))
			     (setf filter-p t))))
      (make-event 'test-quit-event))
    (is-true event-p)
    (is-true filter-p)))
