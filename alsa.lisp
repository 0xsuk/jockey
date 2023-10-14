(in-package :jockey)

(cffi:define-foreign-library libasound
  (:unix (:or "libasound.so" "libasound.so.2"))
  (t (:default "libasound")))

(cffi:use-foreign-library libasound)

(cffi:defctype snd-pcm-sframes :long)
(cffi:defctype snd-pcm-uframes :unsigned-long)

(cffi:defcenum snd-pcm-stream
  (:snd-pcm-stream-playback 0)
  :snd-pcm-stream-capture)

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

(cffi:defcfun "snd_pcm_hw_params_set_channels" :int
  (pcm :pointer)
  (params :pointer)
  (val :unsigned-int))

(cffi:defcfun "snd_pcm_hw_params" :int
  (pcm :pointer)
  (params :pointer))

(cffi:defcfun "snd_pcm_hw_params_free" :void
  (obj :pointer))

(cffi:defcfun "snd_pcm_writei" snd-pcm-sframes
  (pcm :pointer)
  (buffer :pointer)
  (size snd-pcm-uframes))

(cffi:defcfun "snd_pcm_close" :int
  (pcm :pointer))
