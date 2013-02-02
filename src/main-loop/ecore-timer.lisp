;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-timer.lisp $

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

(defclass etimer (ecore) 
  ((timeout :initarg :timeout)
   (job :initarg :job
	:documentation "The job of this timer (as a callback function)")
   (loop-p :reader timer-loop-p :initarg :loop-p
	   :documentation "When true, the next timeout is relative to the end of the current timer workload"))
  (:default-initargs :loop-p nil))

(defgeneric timer-reset (etimer)
  (:documentation "Reset a timer to its full interval This doesn't affect the interval of a timer

- ETIMER a timer instance"))

(defgeneric timer-delay (etimer delay)
  (:documentation "Add some delay for the next occurrence of a timer.

This doesn't affect the interval of a timer.

- ETIMER a timer instance
- DELAY The delay to add to the next iteration."))

(defgeneric timer-freeze (etimer)
  (:documentation "Pauses a running timer.

The timer callback won't be called while the timer is paused. The remaining time until the timer expires will be saved, so the timer can be resumed with that same remaining time to expire, instead of expiring instantly.

- ETIMER a timer instance"))

(defgeneric timer-interval (etimer)
  (:documentation "Get the interval the timer ticks on."))

(defgeneric (setf timer-interval) (interval etimer)
  (:documentation "Change the interval the timer ticks of.

If set during a timer call, this will affect the next interval.

- ETIMER a timer instance
- INTERVAL the interval in seconds"))

(defgeneric timer-pending (etimer)
  (:documentation "Gets the pending time.

- ETIMER a timer instance"))

(defgeneric timer-thaw (etimer)
  (:documentation "Resumes a frozen (paused) timer.

The timer will be resumed from its previous relative position in time. That means, if it had X seconds remaining until expire when it was paused, it will be started now with those same X seconds remaining to expire again. But notice that the interval time won't be touched by this call or by TIMER_FREEZE.

- ETIMER a timer instance"))

(defun timers-precision ()
   "Retrieves the current precision used by timer infrastructure."
  (ffi-ecore-timer-precision-get))

(defun timers-precision-setf (precision)
  "Sets the precision to be used by timer infrastructure.

- PRECISION allowed introduced timeout delay, in seconds.

This sets the precision for all timers. The precision determines how much of an difference from the requested interval is acceptable. One common reason to use this function is to increase the allowed timeout and thus decrease precision of the timers, this is because less precise the timers result in the system waking up less often and thus consuming less resources.

Be aware that kernel may delay delivery even further, these delays are always possible due other tasks having higher priorities or other scheduler policies.

Example: We have 2 timers, one that expires in a 2.0s and another that expires in 2.1s, if precision is 0.1s, then the Ecore will request for the next expire to happen in 2.1s and not 2.0s and another one of 0.1 as it would before.

"
  (ffi-ecore-timer-precision-set (coerce precision 'double-float)))

(defmethod timer-freeze ((etimer etimer))
  (ffi-ecore-timer-freeze (ecore-pointer etimer)))

(defmethod timer-thaw ((etimer etimer))
  (ffi-ecore-timer-thaw (ecore-pointer etimer)))

(defmethod timer-pending ((etimer etimer))
  (ffi-ecore-timer-pending-get (ecore-pointer etimer)))

(defmethod timer-interval ((etimer etimer))
  (ffi-ecore-timer-interval-get (ecore-pointer etimer)))

(defmethod (setf timer-interval) (interval (etimer etimer))
  (ffi-ecore-timer-interval-set (ecore-pointer etimer) (coerce interval 'double-float)))

(defmethod timer-reset ((etimer etimer))
  (ffi-ecore-timer-reset (ecore-pointer etimer)))

(defmethod ecore-del :after ((etimer etimer))
  (with-slots ((pointer pointer))
      etimer
    (when pointer
      (ffi-ecore-timer-del pointer)
      (setf pointer nil))))

(defmethod timer-delay ((etimer etimer) delay)
  (ffi-ecore-timer-delay (ecore-pointer etimer) (coerce delay 'double-float)))

(defmethod initialize-instance :after ((etimer etimer) &key)
  (with-slots ((timeout timeout)
	       (data-pointer data-pointer)
	       (loop-p loop-p))
      etimer    
    (setf (ecore-pointer etimer)
	  (apply (if loop-p
		     #'ffi-ecore-timer-loop-add
		     #'ffi-ecore-timer-add)
		 (list (coerce timeout 'double-float)
		       (callback task-callback)
		       data-pointer)))))

(defun make-etimer (job &key (timeout 1) loop-p)
  "Creates a timer to call the given function in the given period of time.

- JOB A callback function. To stop the timer signal a LAST-ITERATION condition or call ECORE-DEL. 
- TIMEOUT timeout in seconds.
- LOOP-P when true, the next timeout is relative to the end of the current timer- BODY what will be called on timeout

Returns a ETIMER instance"   
  (make-instance 'etimer
		 :timeout timeout
		 :loop-p loop-p
		 :object-cb job))

