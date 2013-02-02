;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/conn/ecore-conn.lisp $

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

;(cc-flags "-I/usr/include/eina-1")

(include 
 "sys/socket.h" 
 "netinet/in.h"
 ;"eina_config.h"
 ;"eina/eina_list.h"
 )

(ctype sa-family-t "sa_family_t")

(constantenum address-family
    ((:af-inet "AF_INET" "PF_INET")
     :documentation "IPv4 Protocol family")
    ((:af-local "AF_UNIX" "AF_LOCAL" "PF_UNIX" "PF_LOCAL")
     :documentation "File domain sockets")
    ((:af-inet6 "AF_INET6" "PF_INET6")
     :documentation "IPv6 Protocol family")
    ((:af-packet "AF_PACKET" "PF_PACKET")
     :documentation "Raw packet access"
     :optional t))

(ctype in-port-t "in_port_t")
(ctype in-addr-t "in_addr_t")

(cunion in6-addr "struct in6_addr"
  "An IPv6 address."
  (addr8  "s6_addr"   :type :uint8 :count 16))

(cstruct sockaddr-in "struct sockaddr_in"
  "An IPv4 socket address."
  (family "sin_family" :type sa-family-t)
  (port   "sin_port"   :type in-port-t)
  (addr   "sin_addr"   :type in-addr-t))

(cstruct in-addr-struct "struct in_addr"
  (addr "s_addr" :type :uint32))

(cstruct sockaddr-in6 "struct sockaddr_in6"
  "An IPv6 socket address."
  (family   "sin6_family"   :type sa-family-t)
  (port     "sin6_port"     :type in-port-t)
  (flowinfo "sin6_flowinfo" :type :uint32)
  (addr     "sin6_addr"     :type in6-addr)
  (scope-id "sin6_scope_id" :type :uint32))

(constant (con-event-client-add "ecore_con_event_client_add") 
	  :type integer
	  :documentation "A client has connected to the server.")
(constant (con-event-client-del "ecore_con_event_client_del") 
	  :type integer
	  :documentation "A client has disconnected from the server.")
(constant (con-event-client-error "ecore_con_event_client_error") 
	  :type integer
	  :documentation "A client experienced an error.")
(constant (con-event-client-upgrade "ecore_con_event_client_upgrade") 
	  :type integer
	  :documentation "A client connection has been upgraded to SSL.")

(constant (con-event-server-add "ecore_con_event_server_add") 
	  :type integer
	  :documentation "A server was created.")
(constant (con-event-server-del "ecore_con_event_server_del") 
	  :type integer
	  :documentation "A server connection was lost. ")
(constant (con-event-server-error "ecore_con_event_server_error") 
	  :type integer
	  :documentation "A server experienced an error.")
(constant (con-event-server-upgrade "ecore_con_event_server_upgrade") 
	  :type integer
	  :documentation "A server connection has been upgraded to SSL.")

(constant (con-event-client-data "ecore_con_client_data")
	  :type integer
	  :documentation "A client connected to the server has sent data.")
(constant (con-event-server-data "ecore_con_server_data")
	  :type integer
	  :documentation "A server connection object has data.")

(constant (con-event-proxy-bind "ecore_con_event_proxy_bind")
	  :type integer
	  :documentation "A server connection has successfully negotiated an ip:port binding.")

(contant (con-event-url-data "ecore_con_event_url_data")
	 :type integer
	 :documentation "A URL object has data.")

(constant (con-event-url-complete "ecore_con_event_url_complete")
	  :type integer
	  :documentation "A URL object has completed its transfer to and from the server and can be reused.")

(constant (con-event-url-progress "ecore_con_event_url_progress")
	  :type integer
	  :documentation "A URL object has made progress in its transfer.")
