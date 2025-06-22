import numpy as np
import matplotlib.pyplot as plt
import sklearn.preprocessing

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
    fig, ((ax0, ax1), (ax2, ax3)) = plt.subplots(2, 2, figsize=(20, 10))
    

    ax = ax0
    ax.plot(impulse, 'bo-')
    
    ax = ax1
    ax.plot(impulseResponse, 'ro-')

    fft_impulse = np.fft.fft(impulse)
    fft_impulseResponse = np.fft.fft(impulseResponse)
    ax2.plot(w, 'bo-')    

    fft_impulse_abs = np.abs(fft_impulse[:fft_impulse.shape[0]//2])
    fft_impulseResponse_abs = np.abs(fft_impulseResponse[:fft_impulseResponse.shape[0]//2])
    fft_impulse_abs = fft_impulse_abs / np.amax(fft_impulse_abs);
    fft_impulseResponse_abs = fft_impulseResponse_abs / np.amax(fft_impulseResponse_abs);


    fft_impulse_db = 20 * np.log10(fft_impulse_abs)
    fft_impulseResponse_db = 20 * np.log10( fft_impulseResponse_abs)

    ax3.set_title('FFT Output')
    ax3.set_ylabel('Magnitude (dB)')
    ax3.plot(fft_impulse_db)
    ax3.plot(fft_impulseResponse_db)
