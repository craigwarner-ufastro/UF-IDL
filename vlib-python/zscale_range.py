"""
ZSCALE_RANGE

PURPOSE:
    Computes the best levels cuts for displaying astronomical images using IRAF zscale algorithm.

DESCRIPTION:
    Returns the best low_cut and high cut values for the
    visualization of an image, on the base of the IRAF Zscale algorithm.

CATEGORY:
    Image Visualization.

MODIFICATION HISTORY:
    Feb 2004 - Gianluca Li Causi, INAF - Rome Astronomical Observatory
               licausi@mporzio.astro.it
               http://www.mporzio.astro.it/~licausi/
    Translated to Python from IDL
"""

import numpy as np
from scipy import stats


def zscale_range(im, contrast=1.0, points=100):
    """
    Computes optimal display cuts using IRAF zscale algorithm.
    
    Parameters
    ----------
    im : array_like
        1-D or 2-D input image array
    contrast : float, optional
        Contrast for the Zscale algorithm (default: 1.0)
    points : int, optional
        Number of sample points to use for range determination (default: 100)
    
    Returns
    -------
    numpy.ndarray
        2-element array of the form [low_cut, high_cut]
    
    Notes
    -----
    Makes a linear fit to the sorted values of the sample points and takes 
    the minimum and maximum values of the fit.
    """
    
    # Convert input to numpy array
    im = np.asarray(im)
    
    # Set default contrast if needed
    if contrast <= 0:
        contrast = 1.0
    
    # Handle constant images
    if np.min(im) == np.max(im):
        return np.array([np.min(im), np.max(im)])
    
    # Get image dimensions
    shape = im.shape
    ndim = im.ndim
    
    while True:  # Equivalent to GOTO loop in IDL
        if ndim == 1:
            lx = shape[0]
            np_points = min(points, lx)
            xx = np.linspace(0, lx-1, np_points).astype(int)
            imp = im[xx]
            
        elif ndim == 2:
            lx, ly = shape
            np_points = min(points, min(lx, ly))
            
            # Create sampling grid
            xx_1d = np.linspace(0, lx-1, np_points).astype(int)
            yy_1d = np.linspace(0, ly-1, np_points).astype(int)
            xx, yy = np.meshgrid(xx_1d, yy_1d, indexing='ij')
            
            imp = im[xx, yy].flatten()
        else:
            raise ValueError("Input array must be 1-D or 2-D")
        
        # Check if sampled points have variation
        if np.min(imp) != np.max(imp):
            break
        
        # Increase sampling if no variation found
        points = points * 5
    
    # Sort the intensity values
    intens = np.sort(imp)
    x = np.arange(len(intens))
    
    # Iterative linear fitting with outlier rejection
    niter = 3
    soglia = 3.0  # threshold in sigma units
    
    # Initial linear fit
    slope, intercept, r_value, p_value, std_err = stats.linregress(x, intens)
    yfit = slope * x + intercept
    
    # Iterative fitting with outlier rejection
    for i in range(1, niter):
        diff = np.abs(intens - yfit)
        if len(diff) <= 1:
            break
        
        sig = np.std(diff)
        ok = diff < soglia * sig
        
        if np.sum(ok) <= 1:
            break
        
        x = x[ok]
        intens = intens[ok]
        slope, intercept, r_value, p_value, std_err = stats.linregress(x, intens)
        yfit = slope * x + intercept
    
    # Calculate zscale cuts
    zmed = (np.min(yfit) + np.max(yfit)) / 2.0
    xmed = (np.min(x) + np.max(x)) / 2.0
    
    zmax = min(zmed + slope * (np.max(x) - xmed) / contrast, np.max(im))
    zmin = max(zmed + slope * (np.min(x) - xmed) / contrast, np.min(im))
    
    # Handle edge case where cuts are equal
    if zmin == zmax:
        zmin = np.min(im)
        zmax = np.max(im)
    
    return np.array([zmin, zmax])


# Example usage and test function
if __name__ == "__main__":
    # Test with a simple 2D array
    test_image = np.random.randn(100, 100) * 100 + 1000
    
    # Add some structure
    test_image[40:60, 40:60] += 500
    
    cuts = zscale_range(test_image)
    print(f"Image range: [{np.min(test_image):.2f}, {np.max(test_image):.2f}]")
    print(f"Zscale cuts: [{cuts[0]:.2f}, {cuts[1]:.2f}]")
    
    # Test with different parameters
    cuts_high_contrast = zscale_range(test_image, contrast=0.5)
    print(f"High contrast cuts: [{cuts_high_contrast[0]:.2f}, {cuts_high_contrast[1]:.2f}]")
