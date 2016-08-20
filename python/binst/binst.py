import pandas as pd

def create_bins(x, breaks):
    """ mirror of the R implementation, create bins given breaks """
    if not isinstance(breaks, list):
        raise Exception('Breaks are not a list')
        
    breaks = sorted([float("-inf"), float("inf")] + breaks)
    return pd.cut(x, breaks, labels=range(len(breaks)-1))
