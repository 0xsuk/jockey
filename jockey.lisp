(in-package :jockey)

(defparameter *client* nil)
(defparameter *output-port* nil)

(defparameter *phase* 0.0)
(defparameter *sample-rate* 48000.0)

(cffi:defcallback process-callback :int ((nframes jack-nframes-t) (arg :pointer))
  (let ((out (jack-port-get-buffer *output-port* nframes)))
    (loop for i from 0 below nframes do
      (setf (cffi:mem-aref out 'jack-default-audio-sample-t i) (coerce (sin *phase*) 'jack-default-audio-sample-t))
      (incf *phase* (/ (* 2.0 pi 440.0) *sample-rate*))
      (when (>= *phase* (* 2.0 pi))
        (decf *phase* (* 2.0 pi))))
    0))

(defun start-jack ()
  (if *client*
      (close-jack))
  (setq *client* (jack-client-open "my-client" 0 (cffi:null-pointer)))
  (if (cffi:null-pointer-p *client*)
      (error "client not initialied"))

  (jack-set-process-callback *client* (cffi:callback process-callback) 0)

  (setq *output-port* (jack-port-register *client* "output" +jack-default-audio-type+ +jack-port-is-output+ 0))
  
  (if (cffi:null-pointer-p *output-port*)
      (error "output port registeration failed"))
  
  (unless (= 0 (jack-activate *client*))
    (format t "failed to activate client"))
  )

(defun close-jack ()
  (jack-client-close *client*)
  (setq *client* nil) 
  )
