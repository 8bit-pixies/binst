from nose.tools import assert_equal
from binst.binst import create_bins

def test_createbins():
    import numpy as np
    x = np.arange(8.0)
    cb = create_bins(x, [3, 5, 6, 10])
    assert_equal(len(cb.categories), 5)
