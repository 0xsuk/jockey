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
(defstruct. track
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


(defun-inline get-position (track)
  (floor (track-index track)))

(defun-inline get-index-in-chunk (position)
  (let ((index-of-chunk (floor (/ position +chunk-size+))))
    (- position (* +chunk-size+ index-of-chunk)))
  )

(defun-inline get-index-of-chunk (position)
  (floor (/ position +chunk-size+))
  )

(defun-inline get-sample (track)
  (let* ((position (get-position track))
         (index-of-chunk (get-index-of-chunk position))
         (index-in-chunk (get-index-in-chunk position))
         (chunk (aref (track-chunks track) index-of-chunk))
         )
    (cffi:mem-aref chunk :unsigned-short index-in-chunk)
    ))

(defun-inline reset-index (track)
  (setf (track-index track) 0d0))

(defun _process-callback (nframes)
  "makeing process-callback lisp function to so that hotfix can be made"
  (with-track left-deck
    (loop
      with out = (jack-port-get-buffer *output-port* nframes)
      for i from 0 below nframes do
        (let* ((sample (get-sample left-deck)))
          (setf (cffi:mem-aref out 'jack-default-audio-sample i)
                (normalize-16-bit sample)
                )
          (incf left-deck.index left-deck.speed)
          (when (>= left-deck.index left-deck.length)
            (if left-deck.next
                (setf left-deck left-deck.next)
                (reset-index left-deck)))))))

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
      (with-track track
        (handler-case
            (loop do
              (when (>= index-of-chunk +maximum-chunks+)
                (format t "Track is full size")
                (return))
              
              (setf (cffi:mem-aref chunk :unsigned-short index-in-chunk)
                    (read-16-bit-le stream))
              (incf index-in-chunk)
              
              (when (>= index-in-chunk +chunk-size+)
                (incf track.length index-in-chunk)
                (setf (aref track.chunks index-of-chunk)
                      chunk)
                
                (incf index-of-chunk)
                (setf chunk (make-chunk))
                (incf chunk-length)
                (setf index-in-chunk 0)
                ))
          (end-of-file ()
            (setf track.chunk-length chunk-length)
            (incf track.length (1+ index-in-chunk))
            (setf (aref track.chunks index-of-chunk)
                  chunk)
            (format t "END OF STREAM"))))
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

(defmacro with-pointers (pairs &body body)
  "each pair is (var :type).
body can contain -ref macro, that gets mem-ref of the var.

(with-pointer ((a-ptr :int))
  (setf a (-ref a-ptr)))
"
  `(let ,(mapcar (lambda (pair)
                   (unless (and (listp pair) (second pair))
                     (error (format nil "~A should be (variable :type)" pair)))
                   `(,(first pair) (cffi:foreign-alloc ,(second pair))))
          pairs)
     (macrolet ((-ref (var)
                  (let* ((pair (or (assoc var ',pairs)
                                   (error (format nil "~A is not valid variable" var))))
                         (type (cadr pair)))
                    `(cffi:mem-ref ,var ,type)
                    )))
       ,@body)))

(defun pc ()
  (alsa-start "plughw:CARD=PCH,DEV=0" 20000))

(defconstant +dphase+ (/ (* 2 pi 440) 44100))
(defconstant +2pi+ (* 2 pi))
(defparameter *phase* 0)
;; (defun alsa-callback (buffer period-size)
;;         )
;;   )

(defun alsa-start (device latency &optional (format :snd-pcm-format-u16-le) (access :snd_pcm_access_rw_interleaved))
  (let ((err 0)
        pcm
        buffer-size
        period-size)
    (unwind-protect
         (progn 
           (with-pointers ((pcm& :pointer))
             (setf err (snd-pcm-open pcm&
                                     device
                                     0
                                     0))
             (when (< err 0)
               (error (snd-strerror err)))
             (setf pcm (-ref pcm&))
             )
           
           (snd-pcm-set-params pcm
                               format
                               access
                               1
                               44100
                               0
                               latency)
           
           
           (with-pointers ((buffer-size& :int)
                           (period-size& :int))
             (snd-pcm-get-params pcm buffer-size& period-size&)
             (setf buffer-size (-ref buffer-size&)
                   period-size (-ref period-size&)))
           
           (format t "~A and ~A~%" buffer-size period-size)
           
           (loop
             with buffer = (cffi:foreign-alloc :unsigned-short :count period-size) do
               (loop for i from 0 to period-size
                     do
                        (setf (cffi:mem-aref buffer :unsigned-short i)
                              (round (* 32767.5 (1+ (sin *phase*)))))
                        (incf *phase* +dphase+)
                        (if (>= *phase* +2pi+)
                            (decf *phase* +2pi+)))
               (setf err (snd-pcm-writei pcm buffer period-size))
               
               (when (= err 32)
                 (princ "underrun")
                 (snd-pcm-prepare pcm))
               (when (< err 0)
                 (error (format nil "Serious error: ~A" (snd-strerror err))))
               
             finally (cffi:foreign-free buffer))
           )
      (setf err (snd-pcm-drain pcm))
      (when (< err 0)
        (error (format nil "drain failed: ~A" (snd-strerror err))))
      (snd-pcm-close pcm)
      
      )))


