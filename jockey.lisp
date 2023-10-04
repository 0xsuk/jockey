(defpackage :jockey
  (:use
   :cl))

(in-package :jockey)

(cffi:defcallback process-callback :int ((nframes jack:nframes_t) (arg :pointer))
  (format t "process_callback")
  0)

(let ((client (jack:jack-client-open "my-client" 0 (cffi:null-pointer))))
  (jack:jack-set-process-callback client (cffi:callback process-callback) (cffi:null-pointer)))
