(defpackage :jack
  (:use
   :cl
   :cffi))

(in-package :jack)

(defctype size_t :unsigned-int)
(defctype jack_nframes_t :uint32)
(defctype jack_port_t :pointer)

(defcfun "jack_set_process_callback" :int
  (client :pointer)
  (process_callback :pointer)
  (arg :int))
