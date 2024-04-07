(in-package :jockey)

(cffi:defctype size :unsigned-int)
(cffi:defctype jack-nframes :uint32)
(cffi:defctype jack-port :pointer)
(cffi:defctype jack-default-audio-sample :float)
(deftype jack-default-audio-sample () 'single-float)
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
  (nframes jack-nframes))

(cffi:defcfun "jack_client_open" :pointer
  (client-name :string)
  (options :int)
  (status :pointer))

(cffi:defcfun "jack_activate" :int
  (client :pointer))

(cffi:defcfun "jack_client_close" :int
  (client :pointer))


(cffi:define-foreign-library libasound
  (:unix (:or "libasound.so" "libasound.so.2"))
  (t (:default "libasound")))

(cffi:use-foreign-library libasound)

(cffi:defctype snd-pcm-sframes :long)
(cffi:defctype snd-pcm-uframes :unsigned-long)

(cffi:defcenum snd-pcm-stream
  (:snd-pcm-stream-playback 0)
  :snd-pcm-stream-capture)

(cffi:defcenum snd-pcm-format
  (:snd-pcm-format-s8 0)
  (:snd-pcm-format-u8 1)
  (:snd-pcm-format-s16-le 2)
  (:snd-pcm-format-u16-le 4))

(cffi:defcenum snd-pcm-access
  (:snd-pcm-acces-mmap-interleaved 0)
  :snd_pcm_access_mmap_noninterleaved
  :snd_pcm_access_mmap_complex
  :snd_pcm_access_rw_interleaved
  :snd_pcm_access_rw_noninterleaved)

(cffi:defcfun "snd_pcm_open" :int
  (pcm :pointer)
  (name :string)
  (stream snd-pcm-stream)
  (mode :int))

(cffi:defcfun "snd_strerror" :string
  (errnum :int))

(cffi:defcfun "snd_pcm_hw_params_malloc" :int
  (ptr :pointer))

(cffi:defcfun "snd_pcm_hw_params_any" :int
  (pcm :pointer)
  (params :pointer))

(cffi:defcfun "snd_pcm_hw_params_set_access" :int
  (pcm :pointer)
  (params :pointer)
  (access snd-pcm-access))

(cffi:defcfun "snd_pcm_hw_params_set_format" :int
  (pcm :pointer)
  (params :pointer)
  (val :int) ; enum
  )

(cffi:defcfun "snd_pcm_hw_params_set_rate_near" :int
  (pcm :pointer)
  (params :pointer)
  (val :pointer)
  (dir :pointer))

(cffi:defcfun "snd_pcm_hw_params_set_rate" :int
  (pcm :pointer)
  (params :pointer)
  (val :unsigned-int)
  (dir :int))

(cffi:defcfun "snd_pcm_hw_params_set_channels" :int
  (pcm :pointer)
  (params :pointer)
  (val :unsigned-int))

(cffi:defcfun "snd_pcm_hw_params_set_period_size_near" :int
  (pcm :pointer)
  (params :pointer)
  (val :pointer)
  (dir :pointer))

(cffi:defcfun "snd_pcm_hw_params_set_buffer_size_near" :int
  (pcm :pointer)
  (params :pointer)
  (val :pointer))

(cffi:defcfun "snd_pcm_hw_params" :int
  (pcm :pointer)
  (params :pointer))

(cffi:defcfun "snd_pcm_hw_params_free" :void
  (obj :pointer))

(cffi:defcfun "snd_pcm_writei" snd-pcm-sframes
  (pcm :pointer)
  (buffer :pointer)
  (size snd-pcm-uframes))

(cffi:defcfun "snd_pcm_set_params" :int
  (pcm :pointer)
  (format snd-pcm-format)
  (access snd-pcm-access)
  (channels :unsigned-int)
  (rate :unsigned-int)
  (soft-resample :int)
  (latency :unsigned-int)
  )

(cffi:defcfun "snd_pcm_get_params" :int
  (pcm :pointer)
  (buffer_size :pointer)
  (period_size :pointer))

(cffi:defcfun "snd_pcm_prepare" :int
  (pcm :pointer))

(cffi:defcfun "snd_pcm_drain" :int
  (pcm :pointer))

(cffi:defcfun "snd_pcm_close" :int
  (pcm :pointer))

(cffi:defcfun "snd_pcm_recover" :int
  (pcm :pointer)
  (err :int)
  (silent :int))
