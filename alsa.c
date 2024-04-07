// Do complicated pointer business for lisp

#include <alsa/asoundlib.h>
#include <math.h>
#include <chrono>

char *device = "hw:CARD=PCH,DEV=0";            /* playback device */
 
static void setup_alsa_params(snd_pcm_t* handle, snd_pcm_uframes_t* buffer_size, snd_pcm_uframes_t* period_size) {
  snd_pcm_hw_params_t* hw_params;
  snd_pcm_hw_params_malloc(&hw_params);
  snd_pcm_hw_params_any(handle, hw_params);
  snd_pcm_hw_params_set_access(handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  snd_pcm_hw_params_set_format(handle, hw_params, SND_PCM_FORMAT_S16_LE);
  snd_pcm_hw_params_set_rate(handle, hw_params, 44100, 0);
  snd_pcm_hw_params_set_channels(handle, hw_params, 2);
  snd_pcm_hw_params_set_period_size_near(handle, hw_params, period_size, 0); //first set period
  snd_pcm_hw_params_set_buffer_size_near(handle, hw_params, buffer_size); //then buffer. (NOTE: when device is set to default, buffer_size became period_size*3 although peirod_size*2 is requested)
  snd_pcm_hw_params(handle, hw_params);
  snd_pcm_get_params(handle, buffer_size, period_size);
  snd_pcm_hw_params_free(hw_params);
}

void finyl_setup_alsa(snd_pcm_t** handle, snd_pcm_uframes_t* buffer_size, snd_pcm_uframes_t* period_size) {
  int err;
  if ((err = snd_pcm_open(handle, device, SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
    printf("Playback open error!: %s\n", snd_strerror(err));
    exit(1);
  }
  setup_alsa_params(*handle, buffer_size, period_size);
  printf("buffer_size %ld, period_size %ld\n", *buffer_size, *period_size);
}


void cleanup_alsa(snd_pcm_t* handle) {
  int err = snd_pcm_drain(handle);
  if (err < 0) {
    printf("snd_pcm_drain failed: %s\n", snd_strerror(err));
  }
  snd_pcm_close(handle);
  printf("alsa closed\n");
}



int main() {
  snd_pcm_t* handle;
  snd_pcm_uframes_t period_size = 32;
  snd_pcm_uframes_t buffer_size = period_size * 2;

  finyl_setup_alsa(&handle, &buffer_size, &period_size);

  short buffer[period_size*2];
  
  for (int i = 0; i<period_size*2; i++) {
    buffer[i] = 0;
  }
  

  double phase = 0;
  double dphase = (2.0 * M_PI * 220) / 44100.0;
  while (1) {

    auto start = std::chrono::system_clock::now();
    
    for (int i = 0; i<period_size; i++) {
      short sample = round(32767 * sin(phase));
      buffer[i*2] = sample;
      buffer[i*2+1] = sample;
      phase += dphase;
      if (phase >= M_PI*2) {
        phase -= M_PI*2;
      }
    }
    
    int err = snd_pcm_writei(handle, buffer, period_size);
    if (err == -EPIPE) {
      printf("Underrun occurred: %s\n", snd_strerror(err));
      snd_pcm_prepare(handle);
    } else if (err == -EAGAIN) {
      printf("eagain\n");
    } else if (err < 0) {
      printf("error %s\n", snd_strerror(err));
      break;
    }

    auto msec = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now()-start).count();

    printf("msec: %ld\n", msec);
  }

  cleanup_alsa(handle);
  
}
