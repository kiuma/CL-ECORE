;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/con/ecore-con-server.lisp $

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

(defun %keys-as-con-use% (keys)
  (loop for key in keys
	for result = (foreign-enum-value :ecore-con-use key) 
	  then (boole boole-ior 
		      result
		      (foreign-enum-value :ecore-con-use key))
	finally (return result)))

(defun %con-use-as-keys% (value)
  (loop for key in (foreign-enum-keyword-list :ecore-con-use)
	when (boole boole-and  value (foreign-enum-value :ecore-con-type key)) 
	collect key))

(defgeneric con-server-timeout (server)
  (:documentation "Get the default time after which an inactive client will be disconnected."))

(defgeneric (setf con-server-timeout) (timeout server)
  (:documentation "Set the default time after which an inactive client will be disconnected.
A value of less than 1 disables the idle timeout"))

(defgeneric con-server-clients (server)
  (:documentation "Retrieves the current list of clients"))

(defclass con-server (ecore)
  ((port :initarg :port :reader con-server-port)
   (address :initarg :address :reader con-server-address)
   (con-type :initarg :con-type :reader con-server-type)
   (ssl3-p :initarg :ssl3 :reader con-server-ssl3-p)
   (tls-p :initarg :tls :reader con-server-tls-p)
   (load-cert-p :initarg :load-cert))
  (:default-initargs :con-type :con-remote-tcp
   :ssl3 nil :tls nil :load-cert nil))

(defmethod initialize-instance :after ((server con-server) &key)
  (with-slots ((con-type con-type)
	       (address address)
	       (port port)
	       (ssl3-p ssl3-p)
	       (tls-p tls-p)
	       (load-cert-p load-cert-p)
	       (no-proxy-p no-proxy-p))
      server
    (let ((con-use ()))
      (when ssl3-p
	(push :con-use-ssl3 con-use))
      (when tls-p
	(push :con-use-tls con-use))
      (when load-cert-p
	(push :con-load-cert con-use))
      (when no-proxy-p
	(push :con-no-proxy con-use))      
      (setf (ecore-pointer server)
	    (ffi-ecore-con-server-add
	     (boole boole-ior 
		    (%keys-as-con-use% con-use)
		    (foreign-enum-value con-type :ecore-con-type))
	     address
	     port)))))

(defmethod ecore-del :after ((server con-server))
  (let ((pointer (ecore-pointer server)))
    (when pointer
      (ffi-ecore-con-server-del pointer))))

(defun make-con-server (address port &key (con-type :con-remote-tcp)
				       ssl3-p tls-p)
  "Creates a server to listen for connections

- ADDRESS Name to associate with the socket. It is used when generating the socket name of a Unix socket, or for determining what host to listen on for TCP sockets. Nil will not be accepted. 
- PORT Number to identify socket. When a Unix socket is used, it becomes part of the socket name. When a TCP socket is used, it is used as the TCP port.
- CON-TYPE The connection type. Can be one of 
    :CON-LOCAL-USER -> Socket in ~/.ecore. 
    :CON-LOCAL-SYSTEM -> Socket in /tmp. 
    :CON-LOCAL-ABSTRACT -> Abstract socket
    :CON-REMOTE-TCP -> Remote server using TCP. 
    :CON-REMOTE-MCAST -> Remote multicast server. 
    :CON-REMOTE-UDP -> Remote server using UDP.
    :CON-REMOTE-BROADCAST -> Remote broadcast using UDP. 
    :CON-REMOTE-NODELAY -> Remote connection sending packets immediately. 
    :CON-REMOTE-CROK -> Remote connection sending data in large chunks.
- SSL3-P Use ssl v.3
- TLS-P use tls

"
  (make-instance 'con-server
		 :con-type con-type
		 :address address
		 :port port
		 :ssl3 ssl3-p
		 :tls tls-p))

(defmethod con-server-timeout ((server con-server))
  (ffi-con-server-timeout-get (ecore-pointer server)))

(defmethod (setf con-server-timeout) (timeout (server con-server))
  (ffi-con-server-timeout-set 
   (ecore-pointer server)
   (coerce timeout 'double-float)))

#|
(defmethod con-server-clients ((server con-server))
  (ffi-con-server-clients-get server))
|#
