;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ffi.lisp $

;;; Copyright (c) 2013, Andrea Chiumenti.  All rights reserved.

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

(use-foreign-library libecore)
 
(defcfun ("ecore_main_loop_begin" ffi-ecore-main-loop-begin) :void)

(defcfun ("ecore_init" ffi-ecore-init) :void)

(defcfun ("ecore_shutdown" ffi-ecore-shutdown) :void)

(defcfun ("ecore_main_loop_quit" ffi-ecore-main-loop-quit) :void)

(defcfun ("ecore_main_loop_iterate" ffi-ecore-main-loop-iterate) :void)

(defcfun ("ecore_idle_enterer_add" ffi-ecore-idle-enterer-add) :pointer
    (calback :pointer)
    (data :pointer))

(defcfun ("ecore_idle_exiter_add" ffi-ecore-idle-exiter-add) :pointer
    (calback :pointer)
    (data :pointer))

(defcfun ("ecore_idler_add" ffi-ecore-idler-add) :pointer
    (calback :pointer)
    (data :pointer))

(defcfun ("ecore_idle_enterer_del" ffi-ecore-idle-enterer-del) :pointer
    (idler :pointer))

(defcfun ("ecore_idle_exiter_del" ffi-ecore-idle-exiter-del) :pointer
    (idler :pointer))

(defcfun ("ecore_idler_del" ffi-ecore-idler-del) :pointer
    (idler :pointer))

(defcfun ("ecore_job_add" ffi-ecore-job-add) :pointer
    (job-cb :pointer)
    (data :pointer))

(defcfun ("ecore_job_del" ffi-ecore-job-del) :pointer
    (job :pointer))

(defcfun ("ecore_event_handler_add" ffi-ecore-event-handler-add) :pointer
  (type :int)
  (callback :pointer)
  (data :pointer))

(defcfun ("ecore_event_handler_del" ffi-ecore-event-handler-del) :pointer
  (event-handler :pointer))

(defcfun ("ecore_event_type_new" ffi-ecore-event-type-new) :int)

(defcfun ("ecore_event_add" ffi-ecore-event-add) :pointer
  (event-type :int)
  (event-holder :pointer)
  (callback :pointer)
  (data :pointer))

(defcfun ("ecore_event_del" ffi-ecore-event-del) :pointer
  (event :pointer))

(defcfun ("ecore_event_filter_add" ffi-ecore-event-filter-add) :pointer
  (start-cb :pointer)
  (callback :pointer)
  (end-cb :pointer)
  (deta :pointer))

(defcfun ("ecore_event_filter_del" ffi-ecore-event-filter-del) :pointer
  (filter :pointer))

(defcfun ("ecore_pipe_add" ffi-ecore-pipe-add) :pointer
  (callback :pointer)
  (data :pointer))

(defcfun ("ecore_pipe_del" ffi-ecore-pipe-del) :pointer
  (pipe :pointer))

(defcenum :poller-type
  :poller-type-core)

(defcfun ("ecore_poller_add" ffi-ecore-poller-add) :pointer
  (poller-type :poller-type)
  (interval :int)
  (callback :pointer)
  (data :pointer))

(defcfun ("ecore_poller_del" ffi-ecore-poller-del) :pointer
  (poller :pointer))

(defcfun ("ecore_pipe_write" ffi-ecore-pipe-write) :int
  (pipe :pointer)
  (pipe-buffer :pointer)
  (array-size :unsigned-int))

(defcfun ("ecore_pipe_write_close" ffi-ecore-pipe-write-close) :void
  (pipe :pointer))

(defcfun ("ecore_pipe_read_close" ffi-ecore-pipe-read-close) :void
  (pipe :pointer))

(defcfun ("ecore_pipe_freeze" ffi-ecore-pipe-freeze) :void
  (pipe :pointer))

(defcfun ("ecore_pipe_thaw" ffi-ecore-pipe-thaw) :void
  (pipe :pointer))

(defcfun ("ecore_pipe_wait" ffi-ecore-pipe-wait) :int
  (pipe :pointer)
  (message-count :int)
  (wait :double))

(defcfun ("ecore_poller_poller_interval_get" 
	  ffi-ecore-poller-poller-interval-get) :int
  (poller :pointer))

(defcfun ("ecore_poller_poller_interval_set" 
	  ffi-ecore-poller-poller-interval-set) :int
  (poller :pointer)
  (interval :int))

(defcfun ("ecore_poller_poll_interval_get" 
	  ffi-ecore-poller-poll-interval-get) :double
  (poller-type :poller-type))

(defcfun ("ecore_poller_poll_interval_set" 
	  ffi-ecore-poller-poll-interval-set) :void
  (poller-type :poller-type)
  (interval :double))

(defcfun ("ecore_timer_precision_get" ffi-ecore-timer-precision-get) :double)

(defcfun ("ecore_timer_precision_set" ffi-ecore-timer-precision-set) :void
  (precision :double))

(defcfun ("ecore_timer_freeze" ffi-ecore-timer-freeze) :void
  (timer :pointer))

(defcfun ("ecore_timer_thaw" ffi-ecore-timer-thaw) :void
  (timer :pointer))

(defcfun ("ecore_timer_pending_get" ffi-ecore-timer-pending-get) :double
  (timer :pointer))

(defcfun ("ecore_timer_interval_get" ffi-ecore-timer-interval-get) :double
  (timer :pointer))

(defcfun ("ecore_timer_interval_set" ffi-ecore-timer-interval-set) :void
  (timer :pointer)
  (interval :double))

(defcfun ("ecore_timer_reset" ffi-ecore-timer-reset) :void
  (timer :pointer))

(defcfun ("ecore_timer_del" ffi-ecore-timer-del) :pointer
  (timer :pointer))

(defcfun ("ecore_timer_delay" ffi-ecore-timer-delay) :void
  (timer :pointer)
  (delay :double))

(defcfun ("ecore_timer_loop_add" ffi-ecore-timer-loop-add) :pointer
  (timeout :double)
  (callback :pointer)
  (data :pointer))

(defcfun ("ecore_timer_add" ffi-ecore-timer-add) :pointer
  (timeout :double)
  (callback :pointer)
  (data :pointer))


(defcfun ("ecore_main_fd_handler_add" ffi-ecore-main-fd-handler-add) :pointer
  (fd :int)
  (flags :int)
  (fd-cb :pointer)
  (data :pointer)
  (buf-func :pointer)
  (buf-data :pointer))

(defcfun ("ecore_main_fd_handler_del" ffi-ecore-main-fd-handler-del) :pointer
  (fd-handler :pointer))

(defcfun ("ecore_main_fd_handler_active_set" ffi-ecore-main-fd-handler-active-set) :void
  (fd-handler :pointer)
  (flags :int))

;; ---------------------- EINA -----------------------------------
(use-foreign-library libeina)

(defcstruct eina-list 
  "Type for a generic double linked list."
  (data :pointer)
  (next :pointer)
  (prev :pointer)
  (accounting :pointer))

(defcfun ("eina_list_next" ffi-eina-list-next) :pointer
  (eina-list :pointer))

