import numpy as np

def create_bins(x, breaks):
    """ mirror of the R implementation, create bins given breaks """
    if not isinstance(breaks, list):
        raise Exception('Breaks are not a list')
    return np.split(x, breaks)
    
