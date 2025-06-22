import numpy as np
import matplotlib.pyplot as plt

def windowCreate(width):
    return np.zeros(width)

def windowApply(data, w):
    o = np.zeros(data.size);
    wc = w.size//2
    for i in range(data.size):
        for j in range(0,w.size):
            datapos = i + j - wc;
            if (datapos >= 0 and datapos < data.size):
                o[i] += data[datapos] * w[j]
    return o

def windowPlot(impulse, impulseResponse, w):
    fig, ((ax0, ax1), (ax2, ax3)) = plt.subplots(2, 2)

    ax = ax0
    ax.plot(impulse, 'bo-')
    
    ax = ax1
    ax.plot(impulseResponse, 'ro-')

    fft_impulse = np.fft.fft(impulse)
    fft_impulseResponse = np.fft.fft(impulseResponse)
    ax2.plot(w, 'bo-')    
    ax3.set_title('FFT Output')
    ax3.plot(np.abs(fft_impulse[:fft_impulse.shape[0]//2]))
    ax3.plot(np.abs(fft_impulseResponse[:fft_impulseResponse.shape[0]//2]))