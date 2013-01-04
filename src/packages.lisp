;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/package.lisp $

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

(in-package #:cl-user)

(defpackage #:ecore
  (:use :cl :cffi)
  (:documentation "Ecore binding for CL")
  (:export #:*ecore-buffer-size*
	   #:in-ecore-loop
	   #:ecore-loop-quit
	   #:ecore-error
	   #:discard
	   #:ecore
	   #:ecore-del
	   #:*ecore-object*
	   ;; timer ----
	   #:etimer
	   #:timer-interval
	   #:timer-pending
	   #:timers-precision
	   #:timers-precision-setf
	   #:timer-freeze
	   #:timer-thaw
	   #:timer-reset
	   #:timer-delay
	   #:make-etimer
	   ;;events ----
	   #:defevent
	   #:make-event-handler
	   #:ecore-event
	   #:event-type
	   #:event-add
	   #:make-event-filter
	   ;;poller ----
	   #:make-poller
	   #:poller-interval
	   #:poll-interval
	   #:setf-poll-interval
	   ;;idler ----
	   #:make-idler
))
