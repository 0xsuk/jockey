(in-package :jockey)

(defparameter *client* nil)
(defparameter *output-port* nil)

(defparameter *sample-rate* 44100.0)

(defparameter left-deck nil)
(defparameter right-deck nil)

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

(defconstant +chunk-size+ (* 2048 1024))
(defconstant +maximum-chunks+ 32)

(defun make-chunk ()
  (cffi:foreign-alloc :unsigned-short :count +chunk-size+)) ; intializing zero is time costy

(deftype chunks-t () '(simple-array cffi:foreign-pointer (#.+maximum-chunks+)))

(defun make-chunks ()
  (make-array +maximum-chunks+ :element-type 'cffi:foreign-pointer :initial-element (cffi:null-pointer)))

; track is 32 chunks
; each chunk is (* 2048 1024 ) sample, each sample is unsigned 16 bit
; track that exceeds 32 chunks will be partially loaded
; total sample (left + right) is 2^28, each 2^27
; length, index is a measure of a single channel
(defstruct track
  (left-chunks (make-chunks) :type chunks-t)
  (right-chunks (make-chunks) :type chunks-t)
  (chunk-length 0 :type (unsigned-byte 6)) ; maximum-chunks 32 = 2^5
  (length 0 :type (unsigned-byte 28)) ; measures a single channel
  (index 0d0 :type double-float)
  (speed 0d0 :type double-float)
  (next nil :type (or null track)) ; support multi track load on a single deck
  )

;; (defun free-chunks (track)
  ;; (let ((chunk-length (/ (track-length track) ))))
  ;; (track-left-chunks track))

(defun normalize-16-bit (value)
  (/ (- value 32768) 32768.0)) ; (expt 2 15) = 32768


(declaim (inline get-position))
(defun get-position (track)
  (floor (track-index track)))

(declaim (inline get-index-in-chunk))
(defun get-index-in-chunk (position)
  (let ((index-of-chunk (floor (/ position +chunk-size+))))
    (- position (* +chunk-size+ index-of-chunk)))
  )

(declaim (inline get-index-of-chunk))
(defun get-index-of-chunk (position)
  (floor (/ position +chunk-size+))
  )

(declaim (inline get-sample))
(defun get-sample (track)
  (let* ((position (get-position track))
         (index-of-chunk (get-index-of-chunk position))
         (index-in-chunk (get-index-in-chunk position))
         (chunk (aref (track-left-chunks track) index-of-chunk))
         )
    (cffi:mem-aref chunk :unsigned-short index-in-chunk)
    ))

(declaim (inline reset-index))
(defun reset-index (track)
  (setf (track-index track) 0d0))

(defun _process-callback (nframes)
  "makeing process-callback lisp function to so that hotfix can be made"
  (loop
    with out = (jack-port-get-buffer *output-port* nframes)
    for i from 0 below nframes do
      (let* ((left-sample (get-sample left-deck)))
        (setf (cffi:mem-aref out 'jack-default-audio-sample i)
              (normalize-16-bit left-sample)
              )
        (incf (track-index left-deck) (track-speed left-deck))
        (when (>= (track-index left-deck) (track-length left-deck))
          (reset-index left-deck)))))

; takes 60 to 200 micro seconds
(cffi:defcallback process-callback :int ((nframes jack-nframes-t) (arg :pointer))
  (declare (ignore arg))
  (_process-callback nframes)
  0
  )

(defun read-16-bit-le (stream)
  "read 16 bits in little endian, only for UNSIGNED"
  (let ((byte1 (read-byte stream))
        (byte2 (read-byte stream)))
    (logior (ash byte2 8) byte1)))

(defmacro with-pcm-stream ((stream filename) &body body)
  `(with-open-stream (,stream
                      (uiop:process-info-output
                       (uiop:launch-program (list "ffmpeg"
                                                  "-i" ,filename
                                                  "-f" "u16le"
                                                  "-ar" "44100"
                                                  "-")
                                            :output :stream
                                            :element-type '(unsigned-byte 8))))
     ,@body))

(defun get-pcm-data (filename)
  "convert audio file into pcm"
  (with-pcm-stream (stream filename)
    (let ((track (make-track))
          (index-of-chunk 0)
          (left-chunk (make-chunk))
          (right-chunk (make-chunk))
          (chunk-length 1)
          (index-in-chunk 0))
      (handler-case
          (loop do
            (when (>= index-of-chunk +maximum-chunks+)
              (format t "Track is full size")
              (return))
            
            (setf (cffi:mem-aref left-chunk :unsigned-short index-in-chunk)
                  (read-16-bit-le stream))
            (setf (cffi:mem-aref right-chunk :unsigned-short index-in-chunk)
                  (read-16-bit-le stream))
            (incf index-in-chunk)
            
            (when (>= index-in-chunk +chunk-size+)
              (incf (track-length track) index-in-chunk)
              (setf (aref (track-left-chunks track) index-of-chunk)
                    left-chunk)
              (setf (aref (track-right-chunks track) index-of-chunk)
                    right-chunk)
              
              (incf index-of-chunk)
              (setf left-chunk (make-chunk)
                    right-chunk (make-chunk))
              (incf chunk-length)
              (setf index-in-chunk 0)
              ))
        (end-of-file ()
          (setf (track-chunk-length track) chunk-length)
          (incf (track-length track) (1+ index-in-chunk))
          (setf (aref (track-left-chunks track) index-of-chunk)
                left-chunk)
          (setf (aref (track-right-chunks track) index-of-chunk)
                right-chunk)
          (format t "END OF STREAM")))
      track)
    ))

(defun scratch (sec times)
  (let ((old *0-speed*))
    (loop repeat times do
      (setq *0-speed* .5d0)
      (sleep sec)
      (setq *0-speed* -.5d0)
      (sleep sec))
    (setq *0-speed* old)
    )
  )

(defun set-pcm-data (track filename)
  (sb-ext:gc :full t)
  (handler-case
      (cond
        ((= track 0)
         (multiple-value-setq (*0-left* *0-right*) (get-pcm-data filename))
         (setq *0-length* (length *0-left*))
         )
        ((= track 1)
         (multiple-value-setq (*1-left* *1-right*) (get-pcm-data filename))
         (setq *1-length* (length *1-left*))
         ))
    (storage-condition ()
      (format t "File is too large"))))

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

(defun start-alsa ()
  (let ((err 0)
        (playback-handle (cffi:null-pointer))
        (hw-params (cffi:null-pointer)))
    (setf err (snd-pcm-open playback-handle
                            "default"
                            1
                            0))
    (when (< err 0)
      (error "cannot open audio device"))
    (snd-pcm-hw-params-malloc hw-params)
    (snd-pcm-hw-params-any playback-handle hw-params)
    (snd-pcm-hw-params-set-access playback-handle hw-params :snd-pcm-access-rm)
    
    ))
