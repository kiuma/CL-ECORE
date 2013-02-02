;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-poller.lisp $

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


(defclass epoller (ecore) ())

(defgeneric poller-interval (poller)
  (:documentation "Returns the interval, in ticks, that poller polls at."))

(defgeneric (setf poller-interval) (interval poller)
  (:documentation "This allows the changing of a poller's polling interval. It is useful when you want to alter a poll rate without deleting and re-creating a poller.

- INTERVAL The tick interval to set; must be a power of 2 and <= 32768."))



(defcenum :poller-type
  :poller-type-core)

(defmethod initialize-instance :after ((poller epoller) &key (interval 1))
  (with-slots ((data-pointer data-pointer))
      poller
    (setf (ecore-pointer poller) (ffi-ecore-poller-add :poller-type-core
						       interval
						       (callback task-callback)
						       data-pointer))))


(defun make-poller (job &key (interval 1))
  "Pollers are, in essence, callbacks that share a single timer per type. Because not all pollers need to be called at the same frequency the user may specify the frequency in ticks(each expiration of the shared timer is called a tick, in ecore poller parlance) for each added poller. Ecore pollers should only be used when the poller doesn't have specific requirements on the exact times to poll.

This architecture means that the main loop is only woken up once to handle all pollers of that type, this will save power as the CPU has more of a chance to go into a low power state the longer it is asleep for, so this should be used in situations where power usage is a concern.

This macro creates a poller to call the given function \(job\) at a particular tick interval. 

Returns an EPOLLER instance"     
  (make-instance 'epoller
		 :interval interval
		 :object-cb job))

(defmethod ecore-del :after ((poller epoller))
  (with-slots ((pointer pointer))
      poller
    (when pointer
      (ffi-ecore-poller-del pointer)
      (setf pointer nil))))

(defmethod poller-interval ((poller epoller))
  (ffi-ecore-poller-poller-interval-get (ecore-pointer poller)))

(defmethod (setf poller-interval) (interval (poller epoller))
  (if (> (ffi-ecore-poller-poller-interval-set (ecore-pointer poller) interval)
	 0)
      interval
      (error 'ecore-error :message "Unable to set interval: ~a for object ~a" interval poller)))


(defun poll-intreval ()
  "Gets the time(in seconds) between ticks for the given poller type."
  (ffi-ecore-poller-poll-interval-get :poller-type-core))

(defun setf-poll-intreval (poll-time)
  "Sets the time(in seconds) between ticks.
This will adjust the time between ticks of the given timer type defined by type to the time period defined by POLL-TIME."
  (ffi-ecore-poller-poll-interval-set :poller-type-core (coerce 'double poll-time))
  poll-time)

