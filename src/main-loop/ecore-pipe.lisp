;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/main-loop/ecore-pipe.lisp $

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

(defgeneric pipe-write (pipe byte-array)
  (:documentation "Writes an array of unsigned-byte-8 to an ecore pipe"))

(defgeneric pipe-read-close (pipe)
  (:documentation "Close the read end of an Ecore_Pipe object"))

(defgeneric pipe-write-close (pipe)
  (:documentation "Close the write end of an Ecore_Pipe object"))

(defgeneric pipe-freeze (pipe)
  (:documentation "Stop monitoring the pipe for reading"))

(defgeneric pipe-thaw (pipe)
  (:documentation "Start monitoring again the pipe for reading"))

(defgeneric pipe-wait (pipe message-count duration)
  (:documentation "Wait from another thread on the read side of a pipe.

- PIPE The pipe to wait for
- MESSAGE-COUNT The minimal number of messages to wait before exiting
- DURATION The amount of time in seconds to wait before exiting

RETURNS: The number of message catched during that wait call."))

(defclass ecore-pipe (ecore)
  ((buffer :initarg :buffer)))

(defcallback pipe-callback :void
    ((data :pointer)
     (c-buffer :pointer)
     (n-bytes :unsigned-int))
  (let ((*ecore-object* (ecore-object-from-data-pointer data)))
    (with-slots ((buffer buffer)
		 (object-cb object-cb))
	*ecore-object*
      (let ((*ecore-buffer* 
	      (loop for i from 0 below n-bytes
		    do (setf (aref buffer i) 
			     (mem-aref c-buffer :unsigned-char i))
		    finally (return (make-array n-bytes 
						:element-type '(unsigned-byte 8) 
						:displaced-to buffer)))))
	(funcall object-cb)))))

(defmethod initialize-instance :after ((pipe ecore-pipe) &key)
  (with-slots ((pointer pointer)
	       (data-pointer data-pointer))
      pipe
    (setf pointer (ffi-ecore-pipe-add (callback pipe-callback) data-pointer))))

(defun make-pipe (pipe-cb &optional (buffer-size *ecore-buffer-size*))
  "Create two file descriptors (sockets on Windows).

Add a callback that will be called when the file descriptor that is listened receives data. An event is also put in the event queue when data is received.

- PIPE-CB the callback function called when data is written to the pipe
- BUFFER-SIZE default size for read buffer"
  (make-instance 'ecore-pipe 
		 :buffer (make-array buffer-size :element-type '(unsigned-byte 8))
		 :object-cb pipe-cb))


(defmethod ecore-del :after ((pipe ecore-pipe))
  (with-slots ((pointer pointer))
      pipe
    (when pointer
      (ffi-ecore-pipe-del pointer)
      (setf pointer nil))))

(defmethod pipe-write ((pipe ecore-pipe) (byte-array array))  
  (unless (typep byte-array '(array (unsigned-byte 8)))
    (error 'ecore-error :message (format nil "~a is not of type ~a~%"
					 byte-array
					 '(array (unsigned-byte 8)))))
  (let ((array-size (length byte-array)))
    (with-foreign-object (pipe-buffer :unsigned-char array-size)
      (dotimes (i array-size)
	(setf (mem-aref pipe-buffer :unsigned-char i) (aref byte-array i)))
      (ffi-ecore-pipe-write (ecore-pointer pipe) pipe-buffer array-size))))


(defmethod pipe-read-close ((pipe ecore-pipe))
  (ffi-ecore-pipe-read-close (ecore-pointer pipe)))

(defmethod pipe-write-close ((pipe ecore-pipe))
  (ffi-ecore-pipe-write-close (ecore-pointer pipe)))

(defmethod pipe-freeze ((pipe ecore-pipe))
  (ffi-ecore-pipe-freeze (ecore-pointer pipe)))

(defmethod pipe-thaw ((pipe ecore-pipe))
  (ffi-ecore-pipe-thaw (ecore-pointer pipe)))

(defmethod pipe-wait ((pipe ecore-pipe) message-count duration)
  (ffi-ecore-pipe-wait (ecore-pointer pipe) 
		       message-count 
		       (coerce 'double-float duration)))
