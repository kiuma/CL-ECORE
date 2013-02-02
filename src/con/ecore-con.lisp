;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/con/ecore-con.lisp $

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



(defclass lookup-info ()
  ((canon-name :initarg :canon-name :reader canon-name)
   (ip :initarg :ip :reader ip)
   (address-family :initarg :a-family :reader address-family)
   (port :initarg :port :reader port)
   (address :initarg :address :reader address)
   (flow-info :initarg :flow-info :reader flow-info)
   (scope-id :initarg :scope-id :reader scope-id))
  (:default-initargs :canon-name nil
   :ip nil
   :a-family nil
   :port nil
   :address nil
   :flow-info nil :scope-id nil))

(defclass ecore-con (ecore lookup-info) 
  ((lookup-address :initarg :lookup-address)))

(defmethod print-object ((info lookup-info) stream)
  (with-slots ((canon-name canon-name)
	       (ip ip)
	       (address-family address-family)
	       (port port)
	       (address address)
	       (flow-info flow-info)
	       (scope-id scope-id))
      info
    (format stream "#<~s
CANON-NAME: ~a
IP: ~a
ADDRESS-FAMILY: ~a
PORT: ~a
ADDRESS: ~a
FLOW-INFO: ~a
SCOPE-ID: ~a>" (class-name (class-of info)) canon-name ip address-family port address flow-info scope-id)))

(defun ecore-con-init () 
  (ffi-ecore-con-init))

(add-ecore-init-function #'ecore-con-init)

(defun ecore-con-shutdown () 
  (ffi-ecore-con-shutdown))

(add-ecore-shutdown-function #'ecore-con-shutdown)

(defgeneric copy-foreign-ipv4 (info pointer))

(defgeneric copy-foreign-ipv6 (info pointer))

(defmethod copy-foreign-ipv4 ((info lookup-info) pointer)
  (with-foreign-slots ((family port addr) pointer sockaddr-in)
    (with-slots ((info-address-family address-family)
		 (info-port port)
		 (info-address address))
	info
      (setf info-address-family (foreign-enum-keyword 'address-family family)
	    info-port port
	    info-address (loop for i from 0 to 3
			       collect (ldb (byte 8 (* i 8)) addr))))))

(defmethod copy-foreign-ipv6 ((info lookup-info) pointer)
  (with-foreign-slots ((family port addr flowinfo scope-id) pointer sockaddr-in6)
    (with-slots ((info-address-family address-family)
		 (info-port port)
		 (info-address address)
		 (info-flow-info flow-info)
		 (info-scope-id scope-id))
	info
      (setf info-address-family (foreign-enum-keyword 'address-family family)
	    info-port port
	    info-address (loop for i from 15 downto 0
			       collect (mem-aref addr :unsigned-char i))
	    info-flow-info flowinfo
	    info-scope-id scope-id))))

(defcallback con-dns-callback :void
    ((cb-canon-name :string)
     (cb-ip :string)
     (addr :pointer)
     (addr-len :int)
     (data :pointer))
  (let ((*ecore-object* (ecore-object-from-data-pointer data)))
    (with-slots ((canon-name canon-name)
		 (ip ip))
	*ecore-object*
      (setf ip cb-ip
	    canon-name cb-canon-name)
      (cond
	((= addr-len size-of-sockaddr-in) 
	 (copy-foreign-ipv4 *ecore-object* addr))
	((= addr-len size-of-sockaddr-in6) 
	 (copy-foreign-ipv6 *ecore-object* addr))
	(t (error 'ecore-error :message 
		  (format nil "Unkown size for sockaddr: ~d. Must be ~a" 
			  addr-len (list size-of-sockaddr-in size-of-sockaddr-in6)))))
      (funcall (object-cb *ecore-object*))
      (ecore-del *ecore-object*))))

(defmethod initialize-instance :after ((ecore-con ecore-con) &key)
  (with-slots ((lookup-address lookup-address))
      ecore-con
    (ffi-ecore-con-lookup lookup-address
			  (callback con-dns-callback)
			  (ecore-data-pointer ecore-con))))

(defun con-lookup (name lookup-cb)
  (make-instance 'ecore-con
		 :lookup-address name
		 :object-cb lookup-cb))
