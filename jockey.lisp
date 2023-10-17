(in-package :jockey)

(defparameter *client* nil)
(defparameter *output-port* nil)

(defparameter *sample-rate* 44100.0)

(defparameter left-deck nil)
(defparameter right-deck nil)

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
; track is mono channel
(defstruct track
  (chunks (make-chunks) :type chunks-t)
  (chunk-length 0 :type (unsigned-byte 6)) ; maximum-chunks 32 = 2^5
  (length 0 :type (unsigned-byte 28)) ; measures a single channel
  (index 0d0 :type double-float)
  (speed 0d0 :type double-float)
  (next nil :type (or null track)) ; support multi track load on a single deck
  )

;; (defun free-chunks (track)
  ;; (let ((chunk-length (/ (track-length track) ))))
  ;; (track-chunks track))

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
         (chunk (aref (track-chunks track) index-of-chunk))
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
      (let* ((sample (get-sample left-deck)))
        (setf (cffi:mem-aref out 'jack-default-audio-sample i)
              (normalize-16-bit sample)
              )
        (incf (track-index left-deck) (track-speed left-deck))
        (when (>= (track-index left-deck) (track-length left-deck))
          (reset-index left-deck)))))

; takes 60 to 200 micro seconds
(cffi:defcallback process-callback :int ((nframes jack-nframes) (arg :pointer))
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
                                                  "-ac" "1"
                                                  "-")
                                            :output :stream
                                            :element-type '(unsigned-byte 8))))
     ,@body))

(defun get-pcm-data (filename)
  "convert audio file into pcm"
  (with-pcm-stream (stream filename)
    (let ((track (make-track))
          (index-of-chunk 0)
          (chunk (make-chunk))
          (chunk-length 1)
          (index-in-chunk 0))
      (handler-case
          (loop do
            (when (>= index-of-chunk +maximum-chunks+)
              (format t "Track is full size")
              (return))
            
            (setf (cffi:mem-aref chunk :unsigned-short index-in-chunk)
                  (read-16-bit-le stream))
            (incf index-in-chunk)
            
            (when (>= index-in-chunk +chunk-size+)
              (incf (track-length track) index-in-chunk)
              (setf (aref (track-chunks track) index-of-chunk)
                    chunk)
              
              (incf index-of-chunk)
              (setf chunk (make-chunk))
              (incf chunk-length)
              (setf index-in-chunk 0)
              ))
        (end-of-file ()
          (setf (track-chunk-length track) chunk-length)
          (incf (track-length track) (1+ index-in-chunk))
          (setf (aref (track-chunks track) index-of-chunk)
                chunk)
          (format t "END OF STREAM")))
      track)
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

(defun alsa-start ()
  (let ((err 0)
        (pcm (cffi:foreign-alloc :pointer)))
    (setq err (snd-pcm-open pcm
                            "plughw:CARD=PCH,DEV=0"
                            0
                            0))
    (when (< err 0)
      (error "cannot open audio device"))

    (snd-pcm-set-params pcm
                        :snd-pcm-format-s16-le
                        :snd_pcm_access_rw_interleaved
                        1
                        44100
                        0
                        1000)
    
    ;; (let* ((buffer-size 44100)
          ;; (buffer (cffi:foreign-alloc :unsigned-short :count buffer-size)))

      ;; (cffi:foreign-free buffer))

    (snd-pcm-drain pcm)
    (snd-pcm-close pcm)
    ))

