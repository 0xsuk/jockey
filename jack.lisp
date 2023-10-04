(defpackage :jack
  (:use
   :cl
   :cffi)
  (:export
   :nframes_t
   :port_t
   :jack-set-process-callback
   :jack-client-open
   ))

(in-package :jack)

(defctype size_t :unsigned-int)
(defctype nframes_t :uint32)
(defctype port_t :pointer)

(define-foreign-library libjack
  (:unix (:or "libjack.so" "libjack.so.0"))
  (t (:default "libjack")))

(use-foreign-library libjack)

(defcfun "jack_set_process_callback" :int
  (client :pointer)
  (process_callback :pointer)
  (arg :int))

(defcfun "jack_client_open" :pointer
  (client-name :string)
  (options :int)
  (status :pointer))
