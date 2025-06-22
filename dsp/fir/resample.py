from window import windowCreate, windowApply, windowPlot  
from resample_data import fir_interpolator_1, fir_interpolator_2
import ipywidgets as widgets
import numpy as np
import matplotlib.pyplot as plt
import ipywidgets as widgets

frameSize = 20000
impulse = np.zeros(frameSize)
impulse[0] = 1;
#[64 * 12]


@widgets.interact(height=widgets.FloatSlider(min=-1.0,   max=1.0, step=0.01, value=0.8), 
                  width =widgets.IntSlider  (min=-1.0,   max=30, step=2, value=5),
                  plateau =widgets.IntSlider  (min=-0.0, max=30, step=2, value=3))
def execute(height=0.8, width=5, plateau=3):

    
    
    impulseResponse = windowApply(impulse, fir_interpolator_1)
    
    windowPlot(impulse, impulseResponse, fir_interpolator_1)
