(in-package :jockey)

(cffi:defctype size-t :unsigned-int)
(cffi:defctype jack-nframes-t :uint32)
(cffi:defctype jack-port-t :pointer)
(cffi:defctype jack-default-audio-sample :float)
(deftype jack-default-audio-sample () 'single-float)
(defconstant +jack-default-audio-type+ "32 bit float mono audio")
(defconstant +jack-port-is-output+ 2)

(cffi:define-foreign-library libjack
  (:unix (:or "libjack.so" "libjack.so.0"))
  (t (:default "libjack")))

(cffi:use-foreign-library libjack)

(cffi:defcfun "jack_set_process_callback" :int
  (client :pointer)
  (process_callback :pointer)
  (arg :int))

(cffi:defcfun "jack_port_register" :pointer
  (client :pointer)
  (port-name :string)
  (port-type :string)
  (flags :ulong) 
  (buffer-size :ulong))

(cffi:defcfun "jack_port_get_buffer" :pointer
  (port :pointer)
  (nframes jack-nframes-t))

(cffi:defcfun "jack_client_open" :pointer
  (client-name :string)
  (options :int)
  (status :pointer))

(cffi:defcfun "jack_activate" :int
  (client :pointer))

(cffi:defcfun "jack_client_close" :int
  (client :pointer))
