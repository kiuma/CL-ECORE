;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/ecore-pipe.lisp $

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

(defclass ecore-pipe (ecore)
  ((callback :initarg :callback)
   (buffer :initarg :buffer)))


(defmethod initialize-instance :after ((pipe ecore-pipe) &key)
  (with-slots ((pointer pointer)	     
	       (callback callback)
	       (buffer buffer))
      pipe
    (macrolet ((def-pipe-callback (func pipe-buffer pipe)
		 (let ((fname (intern  (symbol-name (gensym))))
		       (g-data (gensym))
		       (g-buffer (gensym))
		       (g-nbytes (gensym))
		       (g-pipe-buffer (gensym))
		       (g-pipe (gensym))
		       (g-func (gensym))
		       (i (gensym)))
		   `(let ((,g-func ,func)
			  (,g-pipe-buffer ,pipe-buffer)
			  (,g-pipe ,pipe)) 
		      (defcallback ,fname :void
			  ((,g-data :pointer)
			   (,g-buffer :pointer)
			   (,g-nbytes :unsigned-int))
			(declare (ignore ,g-data))
			(let ((*ecore-object* ,g-pipe)
			      (*ecore-buffer* (loop for ,i from 0 below ,g-nbytes
						    do (setf (aref ,g-pipe-buffer ,i) 
							     (mem-aref ,g-buffer :unsigned-char ,i))
						    finally (return (make-array ,g-nbytes 
										:element-type '(unsigned-byte 8) :displaced-to ,g-pipe-buffer)))))
			  (funcall ,g-func)))))))
      (let ((cb (def-pipe-callback callback buffer pipe)))    
	(setf pointer
	      (foreign-funcall "ecore_pipe_add"
			       :pointer (get-callback cb) 
			       :pointer (null-pointer) 
			       :pointer))))))

(defmacro make-pipe (callback &optional (buffer-size *ecore-buffer-size*))
  "Create two file descriptors (sockets on Windows).

Add a callback that will be called when the file descriptor that is listened receives data. An event is also put in the event queue when data is received.

- CALLBACK the callback function called when data is written to the pipe
- BUFFER-SIZE default size for read buffer"
  `(make-instance 'ecore-pipe 
		  :buffer (make-array ,buffer-size :element-type '(unsigned-byte 8))
		  :callback ,callback))


(defmethod ecore-del :after ((pipe ecore-pipe))
  (with-slots ((pointer pointer))
      pipe
    (when pointer
      (foreign-funcall "ecore_pipe_del"
		       :pointer pointer
		       :pointer))))

(defgeneric pipe-write (pipe byte-array)
  (:documentation "Writes an array of unsigned-byte-8 to an ecore pipe"))

(defmethod pipe-write ((pipe ecore-pipe) (byte-array array))  
  (unless (typep byte-array '(array (unsigned-byte 8)))
    (error 'ecore-error :message (format nil "~a is not of type ~a~%"
					 byte-array
					 '(array (unsigned-byte 8)))))
  (let ((array-size (length byte-array)))
    (with-foreign-object (pipe-buffer :unsigned-char array-size)
      (dotimes (i array-size)
	(setf (mem-aref pipe-buffer :unsigned-char i) (aref byte-array i)))
      (foreign-funcall "ecore_pipe_write"
		       :pointer (ecore-pointer pipe)
		       :pointer pipe-buffer
		       :unsigned-int array-size))))
