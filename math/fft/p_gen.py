import numpy as np
import wave
import struct
import matplotlib.pyplot as plt
from operator import add

freq_one = 440.0
num_samples = 44100*2
sample_rate = 44100.0
amplitude = 12800

file = "440_gen.wav"

s1 = [np.sin(2 * np.pi * freq_one * x/sample_rate) * amplitude for x in range(num_samples)]

sine_one = np.array(s1)

nframes = num_samples
comptype = "NONE"
compname="not compressed"
nchannels = 1
sampwidth = 2

wav_file = wave.open(file, 'w')
wav_file.setparams((nchannels, sampwidth, int(sample_rate), nframes, comptype, compname))

for s in sine_one:
    wav_file.writeframes(struct.pack('h', int(s)))
