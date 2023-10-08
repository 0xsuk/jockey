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
  (/ (- value 32768) 32768.0)) ; (expt 2 15) = 32768

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
    (let ((left-samples (make-array 131072 :adjustable t :element-type 'jack-default-audio-sample-t))
          (right-samples (make-array 131072 :adjustable t :element-type 'jack-default-audio-sample-t)))
      (handler-case
          (loop
            with i = 0 do
              (let ((left-sample (normalize-16-bit (read-16-bit-le stream)))
                    (right-sample (normalize-16-bit (read-16-bit-le stream))))
                (when (>= i (length left-samples))
                  (adjust-array left-samples (+ 131072 (length left-samples)))
                  (adjust-array right-samples (+ 131072 (length right-samples))))
                (setf (aref left-samples i) left-sample)
                (setf (aref right-samples i) right-sample)
                (incf i)))
        (end-of-file () (format t "END OF STREAM")))
      left-samples
      )))

(defun set-pcm-data (filename)
  (setq *pcm-data* (get-pcm-data filename))
  (setq *pcm-data-length* (length *pcm-data*)))

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
