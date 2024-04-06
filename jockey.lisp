
(in-package :jockey)

(defparameter *client* nil)
(defparameter *output-port* nil)

(defparameter *sample-rate* 44100)

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

(defparameter *period-size* 512)

(defconstant +dphase+ (/ (* 2 pi 440) 44100))
(defconstant +2pi+ (* 2 pi))
(defparameter *phase* 0)

(defun alsa-setup (handle& buffer-size& period-size&)
  (cffi:with-foreign-object (hw-params&& :pointer) ; pointer to pointer
    (symbol-macrolet ((hw-params& (cffi:mem-ref hw-params&& :pointer)))
      (snd-pcm-hw-params-malloc hw-params&&)
      (snd-pcm-hw-params-any handle& hw-params&)
      (snd-pcm-hw-params-set-access handle& hw-params& :snd_pcm_access_rw_interleaved)
      (snd-pcm-hw-params-set-format handle& hw-params& 2)
      (snd-pcm-hw-params-set-rate handle& hw-params& *sample-rate* 0)
      (snd-pcm-hw-params-set-channels handle& hw-params& 2)
      (snd-pcm-hw-params-set-period-size-near handle& hw-params& period-size& (cffi:null-pointer))
      (snd-pcm-hw-params-set-buffer-size-near handle& hw-params& buffer-size&)
      (snd-pcm-hw-params handle& hw-params&)
      (snd-pcm-get-params handle& buffer-size& period-size&)
      (snd-pcm-hw-params-free hw-params&))))

(defvar handle-ref)

(defvar buffer-ref nil)

(defun start ()
  (alsa-start "hw:CARD=PCH,DEV=0"))

(defun fill-buffer (buffer&)
  (loop for i from 0 to *period-size*
        do
           (let ((sample (round (* 32767 (sin *phase*)))))
             (setf (cffi:mem-aref buffer& :short (* 2 i))
                   sample)
             (setf (cffi:mem-aref buffer& :short (1+ (* i 2)))
                   sample))
           (incf *phase* +dphase+)
           (if (>= *phase* +2pi+)
               (decf *phase* +2pi+))
        )
  )

(defun alsa-start (device)
  (let ((err 0))
    (cffi:with-foreign-object (handle&& :pointer)
      (setf handle-ref handle&&)
      (unwind-protect
           (progn 
             (setf err (snd-pcm-open handle&&
                                     device
                                     0
                                     0))
             (when (< err 0)
               (error (snd-strerror err)))
             
             (cffi:with-foreign-objects ((buffer-size& :int)
                                         (period-size& :int))
               (setf (cffi:mem-ref period-size& :int) *period-size*)
               (setf (cffi:mem-ref buffer-size& :int) (* *period-size* 2))
               (format t "want: ~A and ~A~%" (cffi:mem-ref buffer-size& :int) (cffi:mem-ref period-size& :int))
               (alsa-setup (cffi:mem-ref handle&& :pointer) buffer-size& period-size&)
               (setf *period-size* (cffi:mem-ref period-size& :int))

               
               (format t "got: ~A and ~A~%" (cffi:mem-ref buffer-size& :int) *period-size*))
             
             (loop
               with buffer& = (cffi:foreign-alloc :short :count (* 2 *period-size*)) do
                 (fill-buffer buffer&)
                 (setf err (snd-pcm-writei (cffi:mem-ref handle&& :pointer) buffer& *period-size*))
                 
                 (when (= err 32)
                   (princ "underrun")
                   (snd-pcm-prepare (cffi:mem-ref handle&& :pointer)))
                 (when (< err 0)
                   (error (format nil "Serious error: ~A" (snd-strerror err))))
               ))
        
        (format t "closing")
        (unwind-protect 
             (setf err (snd-pcm-drain (cffi:mem-ref handle&& :pointer)))
          (format  t "tried"))
        (when (< err 0)
          (error (format nil "drain failed: ~A" (snd-strerror err))))
        (snd-pcm-close (cffi:mem-ref handle&& :pointer))
        (format t "closed")
        )
      )))


