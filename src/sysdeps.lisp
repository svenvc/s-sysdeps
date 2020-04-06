;;;; -*- mode: lisp -*-
;;;;
;;;; S-SYSDEPS is an abtraction layer over platform dependent functionality
;;;;
;;;; Copyright (C) 2004-2007,2020 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-sysdeps)

;; implementation using usockets and bordeaux-threads

;; managing processes

(defun multiprocessing-capable-p ()
  "Returns t when this implementation is multiprocessing capable"
  #+bordeaux-threads t
  #-bordeaux-threads nil)

(defun current-process ()
  "Return the object representing the current process"
  (bt:current-thread))

(defun kill-process (process)
  "Kill the process represented by the object process"
  (bt:destroy-thread process))

(defun run-process (name function &rest arguments)
  "Create and run a new process with name, executing function on arguments"
  (bt:make-thread #'(lambda () (apply function arguments)) :name name))

(defun all-processes ()
  "Return a list of all processes currently running"
  (bt:all-threads))

;; opening a client TCP/IP socket stream

(defun open-socket-stream (host port &key connect-timeout read-timeout write-timeout)
  "Create and open a bidirectional client TCP/IP socket stream to host:port"
  (declare (ignore connect-timeout write-timeout))
  (usocket:socket-stream
   (usocket:socket-connect host
			   port
			   :timeout read-timeout
			   ;; we must use :default on SBCL to get a bivalent stream
			   :element-type :default)))

;; accessing socket stream properties

(defun get-socket-stream-property (socket-stream property)
  "Get the value of a socket stream property, one of :remote-host :remote-port :local-host :local-port"
  (declare (ignore socket-stream))
  ; this only works while usocket:socket-server's handler is running
  (cond ((eql property :remote-host) usocket:*remote-host*)
	((eql property :remote-port) usocket:*remote-port*)
	(t nil)))

;; implementing a standard TCP/IP server

(defun start-standard-server (&key port name connection-handler)
  "Start a server process with name, listening on port, delegating to connection-handler with stream as argument"
  ;; note that in the handler, the thread is already spawned
  ;; both the thread and the stream will be closed after the handler returns
  (usocket:socket-server "localhost"
			 port
			 connection-handler
			 '()
			 :in-new-thread t
			 :multi-threading t
			 :name name
			 ;; we must use :default on SBCL to get a bivalent stream
			 :element-type :default))

(defun stop-server (name)
  "Stop a named server"
  (let ((thread (find name (bt:all-threads) :key #'bt:thread-name :test #'equal)))
    (when thread (bt:destroy-thread thread))
    name))

;; working with process locks

(defun make-process-lock (name)
  "Create a named process lock object"
  (bt:make-recursive-lock name))

(defmacro with-process-lock ((lock) &body body)
  "Execute body wih the process lock grabbed, wait otherwise"
  `(bt:with-recursive-lock-held (,lock) ,@body))

;;;; eof
