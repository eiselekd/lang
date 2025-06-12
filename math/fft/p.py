import numpy as np
import matplotlib.pyplot as plt
import scipy.io.wavfile as wave

infile = "440_gen.wav"
rate, data = wave.read(infile)

data = np.array(data)

data_fft = np.fft.fft(data[0:500])
frequencies = np.abs(data_fft)

plt.subplot(4,1,1)
plt.plot(data[:800])
plt.title("Original wave: " + str(frequencies[5]))

plt.subplot(4,1,2)
plt.plot(frequencies)
plt.title("Fourier transform results")

plt.xlim(0, 250)

shift = 80
data_fft2 = np.fft.fft(data[shift:500+shift])
frequencies2 = np.abs(data_fft2)

plt.subplot(4,1,3)
plt.plot(data[shift:800+shift])
plt.title("Original wave 2: " + str(frequencies2[5]))

plt.subplot(4,1,4)
plt.plot(frequencies2)
plt.title("Fourier transform results 2")
plt.xlim(0, 250)


plt.tight_layout()

plt.show()
