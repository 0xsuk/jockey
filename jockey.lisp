(in-package :jockey)

(defparameter *client* nil)
(defparameter *output-port* nil)

(defparameter *sample-rate* 44100.0)

(defparameter *0-left* nil)
(defparameter *0-right* nil)
(defparameter *0-length* 0)
(defparameter *0-index* 0)
(defparameter *0-speed* 1.0d0)

(defparameter *1-left* nil)
(defparameter *1-right* nil)
(defparameter *1-length* 0)
(defparameter *1-index* 0)
(defparameter *1-speed* 1.0d0)

(defun normalize-16-bit (value)
  (/ (- value 32768) 32768.0)) ; (expt 2 15) = 32768

(defun _process-callback (nframes)
  "makeing process-callback lisp function to so that hotfix can be made"
  (let ((out (jack-port-get-buffer *output-port* nframes)))
    (loop for i from 0 below nframes do
      (let* ((position (floor *0-index*))
             (sample (aref *0-left* (mod position *0-length*))))
        (setf (cffi:mem-aref out 'jack-default-audio-sample-t i) (normalize-16-bit sample)))
      (incf *0-index* *0-speed*)
      (when (>= *0-index* *0-length*)
        (setq *0-index* 0))
          ))
  )
; takes 60 to 200 micro seconds
(cffi:defcallback process-callback :int ((nframes jack-nframes-t) (arg :pointer))
  (_process-callback nframes)
  0
  )

(defun read-16-bit-le (stream)
  "read 16 bits in little endian, only for UNSIGNED"
  (let ((byte1 (read-byte stream))
        (byte2 (read-byte stream)))
    (logior (ash byte2 8) byte1)))

(defun get-pcm-data (filename)
  (with-open-stream (stream
                     (uiop:process-info-output
                      (uiop:launch-program (list "ffmpeg"
                                                 "-i" filename
                                                 "-f" "u16le"
                                                 "-ar" "44100"
                                                 "-")
                                           :output :stream
                                           :element-type '(unsigned-byte 8))))
    (let ((left-samples (make-array 0 :adjustable t :fill-pointer t :element-type '(unsigned-byte 16)))
          (right-samples (make-array 0 :adjustable t :fill-pointer t :element-type '(unsigned-byte 16))))
      (handler-case
          (loop do
            (let ((left-sample (read-16-bit-le stream))
                  (right-sample (read-16-bit-le stream)))
              (vector-push-extend left-sample left-samples)
              (vector-push-extend right-sample right-samples)
              ))
        (end-of-file () (format t "END OF STREAM")))
      (values left-samples right-samples)
      )))

(defun set-pcm-data (track filename)
  (multiple-value-bind (left right) (get-pcm-data filename)
    (cond
      ((= track 0) (setq *0-left* left
                         *0-right* right
                         *0-length* (length left))
                   )
      ((= track 1) (setq *1-left* left
                         *1-right* right
                         *1-length* (length right))))
    ))

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
