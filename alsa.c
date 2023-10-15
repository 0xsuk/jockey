// Do complicated pointer business for lisp

#include <alsa/asoundlib.h>

int initialize_params (snd_pcm_t *pcm) {
  return snd_pcm_set_params(pcm,
                            SND_PCM_FORMAT_U16_LE,
                            SND_PCM_ACCESS_RW_INTERLEAVED,
                            1,
                            44100,
                            0, //no resampling
                            1000); // latency 1ms
}

char *device = "plughw:CARD=PCH,DEV=0";            /* playback device */
snd_pcm_format_t format = SND_PCM_FORMAT_U16_LE;
snd_pcm_access_t access_ = SND_PCM_ACCESS_RW_INTERLEAVED;
unsigned int channels = 1;
unsigned int sample_rate = 44100;
int soft_resample = 0;
unsigned int latency = 1000;
#define buffer_length 16384
unsigned short buffer[buffer_length];              /* some random data */
 
void set_device (char * dev) {
  device = dev;
}

int main(void)
{
  int err;
  unsigned int i;
  snd_pcm_t *handle;
  snd_pcm_sframes_t frames;
  
  for (i = 0; i < buffer_length; i++)
    buffer[i] = random() & 0xffff;
 
  if ((err = snd_pcm_open(&handle, device, SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
    printf("Playback open error!: %s\n", snd_strerror(err));
    return -1;
  }
  if ((err = initialize_params(handle)) < 0) {   /* 0.5sec */
    printf("Playback open error: %s\n", snd_strerror(err));
    exit(EXIT_FAILURE);
  }
 
  printf("frames = %ld\n", frames);
  for (i = 0; i < 16; i++) {
    frames = snd_pcm_writei(handle, buffer, buffer_length);
    printf("frames = %ld\n", frames);
    if (frames < 0)
      frames = snd_pcm_recover(handle, frames, 0);
    if (frames < 0) {
      printf("snd_pcm_writei failed: %s\n", snd_strerror(frames));
      break;
    }
    if (frames > 0 && frames < (long)sizeof(buffer))
      printf("Short write (expected %li, wrote %li)\n", (long)sizeof(buffer), frames);
  }
 
  /* pass the remaining samples, otherwise they're dropped in close */
  err = snd_pcm_drain(handle);
  if (err < 0)
    printf("snd_pcm_drain failed: %s\n", snd_strerror(err));
  snd_pcm_close(handle);
  return 0;
}
