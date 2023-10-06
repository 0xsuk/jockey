(in-package :jockey)

(defparameter *client* nil)
(defparameter *output-port* nil)

(defparameter *sample-rate* 48000.0)

(defparameter *pcm-data* nil)
(defparameter *pcm-index* 0)
(cffi:defcallback process-callback :int ((nframes jack-nframes-t) (arg :pointer))
  (let ((out (jack-port-get-buffer *output-port* nframes)))
    (loop for i from 0 below nframes do
      (let ((sample (aref *pcm-data* (mod *pcm-index* (length *pcm-data*)))))
        (setf (cffi:mem-aref out 'jack-default-audio-sample-t i)
              (coerce sample 'jack-default-audio-sample-t)))
      (incf *pcm-index*)
      ))
  0
  )

(defun read-16-bit-le (stream)
  "read 16 bits in little endian, only for UNSIGNED"
  (let ((byte1 (read-byte stream))
        (byte2 (read-byte stream)))
    (logior (ash byte2 8) byte1)))

(defun normalize-16-bit (value)
  (/ (- value 32768) 32768.0))

(defun read-pcm-file (filename)
  (with-open-file (stream filename
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((file-size (file-length stream))
           (sample-count (/ file-size 4)) ; 1 sample = 2 bytes, and also consider stereo
           (samples-left (make-array sample-count :element-type 'single-float))
           (samples-right (make-array sample-count :element-type 'single-float)))
      (loop for i from 0 below sample-count do
        (let ((sample-left (normalize-16-bit (read-16-bit-le stream)))
              (sample-right (normalize-16-bit (read-16-bit-le stream))))
          (setf (aref samples-left i) sample-left)
          (setf (aref samples-right i) sample-right)))
      samples-left))
  )


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
