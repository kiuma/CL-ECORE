;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/con/ffi.lisp $

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

(in-package #:ecore-con)


(defcfun ("ecore_con_init" ffi-ecore-con-init) :void)

(defcfun ("ecore_con_shutdown" ffi-ecore-con-shutdown) :void)

(defcfun ("ecore_con_lookup" ffi-ecore-con-lookup) :int
  (name :string)
  (cb :pointer)
  (data :pointer))

(defcenum :ecore-con-type
  :con-local-user
  :con-local-system
  :con-local-abstract
  :con-remote-tcp
  :con-remote-mcast
  :con-remote-udp
  :con-remote-boradcast
  :con-remote-nodelay
  :con-remote-crok)

(defcenum :ecore-con-use
  (:con-use-ssl3 32)
  (:con-use-tls 64)
  (:con-use-mixed 96)
  (:con-load-cert 128)
  (:con-no-proxy 256))

(defcfun ("ecore_con_server_add" ffi-ecore-con-server-add) :pointer
  (con-type :int)
  (server :string)
  (data :pointer))

(defcfun ("ecore_con_server_del" ffi-ecore-con-server-del) :pointer
  (server :pointer))

(defcfun ("ecore_con_server_timeout_set" ffi-con-server-timeout-set) :void
  (server :pointer)
  (timeout :double))

(defcfun ("ecore_con_server_timeout_get" ffi-con-server-timeout-get) :double
  (server :pointer))

(defcfun ("ecore_con_server_clients_get" ffi-con-server-clients-get) :pointer
  (server :pointer))
