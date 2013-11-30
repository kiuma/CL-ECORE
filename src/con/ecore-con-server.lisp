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

(defgeneric con-server-connected-p (server)
  (:documentation "Retrieves whether the given server is currently connected."))

(defgeneric con-server-name (server)
  (:documentation "Retrives the name of the server"))

(defgeneric con-server-port (server)
  (:documentation "Retrieves the port of the server"))

(defgeneric con-server-send (server buffer)
  (:documentation "Sends a buffer of bytes to the server"))


(defgeneric setf-con-server-client-limit (server client-limit &optional reject-excess-clients-p)
  (:documentation "Sets a limit on the number of clients that can be handled concurrently by the given server, and a policy on what to do if excess clients try to connect.

- SERVER the server.
- CLIENT-LIMIT The maximum number of clients to handle concurrently. -1 means unlimited (default). 0 effectively disables the server.
- REJECT-EXCESS-CLIENTS Set to 1 to automatically disconnect excess clients as soon as they connect if you are already handling client_limit clients. Set to 0 (default) to just hold off on the \"accept\(\)\" system call until the number of active clients drops. This causes the kernel to queue up to 4096 connections (or your kernel's limit, whichever is lower).

Beware that if you set this once ecore is already running, you may already have pending CLIENT_ADD events in your event queue. Those clients have already connected and will not be affected by this call. Only clients subsequently trying to connect will be affected."))

(defgeneric con-server-ip (server)
  (:documentation "Gets the IP address of a server that has been connected to."))

(defgeneric con-server-uptime (server)
  (:documentation "Check how long a server has been connected."))

(defgeneric con-server-flush (server)
  (:documentation "Flushes all pending data to the given server."))

(defgeneric ecore-con-send (eocre-con buffer ffi-function)
  (:documentation "Private internal method used by con-server and con-client used to sent a buffer with a given ffi function.
This method calls the non-blocking FFI-FUNCTION multiple times if BUFFER exceeds *CON-BUFFER-SIZE*"))

(defclass ecore-con (ecore) 
  ((ecore-buffer :accessor ecore-con-buffer :initform (foreign-alloc :char :count *con-buffer-size*))))

(defclass con-server (ecore-con) ())

(defun make-con-use (&key
			con-type		      
			ssl3-p
			tls-p
			load-cert-p
			no-proxy-p)
  (let ((con-use ()))
    (when ssl3-p
      (push :con-use-ssl3 con-use))
    (when tls-p
      (push :con-use-tls con-use))
    (when load-cert-p
      (push :con-load-cert con-use))
    (when no-proxy-p
      (push :con-no-proxy con-use))
    (boole boole-ior 
	   (%keys-as-con-use% con-use)
	   (foreign-enum-value con-type :ecore-con-type))))

(defmethod initialize-instance :after ((server con-server) &key ecore-pointer)
  (with-slots ((data-pointer data-pointer))
      server
    (ffi-con-server-data-set ecore-pointer data-pointer)
    (setf (ecore-pointer server) ecore-pointer)))

(defun make-con-server (address port &key 
				       (con-type :con-remote-tcp)
				       ssl3-p 
				       tls-p 
				       load-cert-p)
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
- LOAD-CERT-P Attempt to use the loaded certificate
"
  (let ((ecore-pointer (ffi-ecore-con-server-add
			(make-con-use :con-type con-type 
				      :ssl3-p ssl3-p 
				      :tls-p tls-p 
				      :load-cert-p load-cert-p)
			address
			port
			(null-pointer))))
    (and (not (null-pointer-p ecore-pointer)) 
	 (make-instance 'con-server :ecore-pointer ecore-pointer))))

(defun con-server-connect (address port &key 
					  (con-type :con-remote-tcp)
					  ssl3-p 
					  tls-p 
					  no-proxy-p)
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
- NO-PROXY-P Disable all type of proxy on the server
"
  (let ((ecore-pointer (ffi-ecore-con-server-connect
			(make-con-use :con-type con-type :ssl3-p ssl3-p :tls-p tls-p :no-proxy-p no-proxy-p)
			address
			port
			(null-pointer))))
    (and (not (null-pointer-p ecore-pointer)) 
	 (make-instance 'con-server :ecore-pointer ecore-pointer))))

;;(defun make-con-server-connection (address port &key (con-type :con-remote)))

(defmethod ecore-del :after ((ecore-con ecore-con))
  (foreign-free (con-server-ecore-buffer server)))

(defmethod ecore-del :after ((server con-server))
  (let ((pointer (ecore-pointer server)))   
    (when pointer
      (ffi-ecore-con-server-del pointer))))

(defmethod con-server-timeout ((server con-server))
  (ffi-con-server-timeout-get (ecore-pointer server)))

(defmethod (setf con-server-timeout) (timeout (server con-server))
  (ffi-con-server-timeout-set 
   (ecore-pointer server)
   (coerce timeout 'double-float)))

(defmethod con-server-connected-p ((server con-server))
  (> (ffi-con-server-connected-get (ecore-pointer server)) 0))

(defmethod initialize-instance :after ((client con-client) &key ecore-pointer)
  (with-slots ((data-pointer data-pointer))
      client
    (ffi-con-client-data-set ecore-pointer data-pointer)
    (setf (ecore-pointer client) ecore-pointer)))

(defmethod con-server-clients ((server con-server))
  (let ((clients-eina-list (ffi-con-server-clients-get (ecore-pointer server)))
	(result ()))
    (loop for eina-list = clients-eina-list then (ffi-eina-list-next eina-list)
	  while (not (null-pointer-p eina-list))
;	  when (not (null-pointer-p ))
	  do (let ((client-pointer (foreign-slot-value eina-list 'eina-list 'ecore::data)))
	       (when (not (null-pointer-p client-pointer))
		 (let ((data-pointer (ffi-con-client-data-get client-pointer)))
		   (push (or (and (not (null-pointer-p data-pointer)) 
				  (ecore-object-from-data-pointer data-pointer))
			     (make-instance 'con-client :ecore-pointer client-pointer))
			 result)))))
    result))

(defmethod con-server-name ((server con-server))
  (ffi-con-server-name-get server))

(defmethod con-server-port ((server con-server))
  (ffi-con-server-port-get server))

(defmethod ecore-con-send ((ecore-con ecore-con) buffer ffi-function)
  (with-slots ((ecore-pointer ecore-pointer) 
	       (ecore-buffer ecore-buffer))
      ecore-con
    (let ((size 0))
      (loop for ch across buffer
	    while (< size *con-buffer-size*)
	    do (progn
		 (incf size)
		 (setf (mem-aref ecore-buffer :char size) ch)))
      (funcall ffi-function ecore-pointer ecore-buffer size)
      (when (< size (length buffer))
	(ecore-con-send ecore 
			(make-array (- (length buffer) size) 
				    :displaced-to buffer 
				    :displaced-index-offset size)
			ffi-function)))))

(defmethod con-server-send ((server con-server) buffer)
  (ecore-con-send server buffer #'ffi-con-server-send))

(defmethod setf-con-server-client-limit ((server con-server) client-limit &optional reject-excess-clients-p)
  (with-slots ((ecore-pointer ecore-pointer))
      server
    (ffi-con-server-client-limit-set ecore-pointer client-limit
				     (if reject-excess-clients-p
					 1
					 0))))


(defmethod con-server-ip ((server con-server))
  (with-slots ((ecore-pointer ecore-pointer))
      server
    (ffi-con-server-ip-get ecore-pointer)))


(defmethod con-server-uptime ((server con-server))
  (with-slots ((ecore-pointer ecore-pointer))
      server
    (ffi-con-server-uptime-get ecore-pointer)))

(defmethod con-server-flush ((server con-server))
  (with-slots ((ecore-pointer ecore-pointer))
      server
    (ffi-con-server-flush ecore-pointer)))

