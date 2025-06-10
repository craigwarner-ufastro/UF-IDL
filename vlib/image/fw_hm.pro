;+
; NAME:
;	FW_HM
; PURPOSE:
;	Directly call the function FullWid_HalfMax to
;       return the full-width-half-max (FWHM) around the peak (maximum)
;       in a 1D profile, 2D image (e.g. star), or 3D volumetric-data.
;       FWHM is determined for each dimension independently by:
;       Linear interpolations (default), or fitting Gaussian functions,
;       or by fitting modified Lorentzian functions, to each profiles.
; CALLING EXAMPLE:
;       fwhm_xy = fw_hm( data, CENTROID=cxy, /GAUSSIAN_FIT )
; INPUTS:
;       data = 1D, 2D, or 3D array (e.g. spectrum, image, or data cube).
; KEYWORDS (in):
;      /GAUSSIAN_FIT : nonlinear Least-squares fit of Gaussian function to 
;                     profile in each dimension to get FWHM,
;                     (default is to estimate FWHM by Linear interpolation).
;      /LORENTZIAN_FIT : non-Lin-Lsq fit modified Lorentzian (Cauchy) function
;                     to profile in each dimension to estimate FWHM, and
;                     other parameters, giving more freedom than Gaussian.
;	/SKY_FIT : include a constant param (sky offset) in non-Lin-Lsq fits.
;       MAX_ITER_FIT = maximum # of iterations to use in Lsq fits, default=10.
;      /AVERAGE : return the average FWHM, instead of for each dimension.
; KEYWORDS (out):
;       FIT_PARAMETERS = an array containing the parameters determined by the
;              Lsq fits to profiles, thus a matrix if data is > 1 dimensional.
;       CENTROID = array, the centroid coordinate of profile in each dim,
;                     if either /GAUSSIAN_FIT or /LORENTZIAN_FIT requested.
;                     Convention is that center of pixel is at 0.5 + pixel#.
;       PEAK_INDEX = array, the pixel # at maximum of profile in each dim.
;       RANGE_MINMAX = array containing the minimum, maximum value of data.
; RESULT:
;       Function returns an array giving FWHM for each dimension,
;       or if /AVER set, a scalar giving the average FWHM around the peak.
; EXTERNAL CALLS:
;	function FullWid_HalfMax
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1993.
;-

function FW_HM, data, AVERAGE=aver, CENTROID=cntrd, INNER_MAX=inner, $
                PEAK_INDEX=peak, RANGE_MINMAX=range, $
		FIT_PARAMETERS=fit_params, SIGMA_PARAMS=sig_par,$
		GAUSSIAN_FIT=fit_gaussian, SKY_FIT=sky_fit, $
                LORENTZIAN_FIT=fit_Lorentz, MOFFAT_FIT=fit_Moffat, $
		MINVAL_FIT=fit_minval, MAX_ITER_FIT=maxit, CHISQ=chisq

return, FullWid_HalfMax( data, AVERAGE=aver, CENTROID=cntrd, INNER_MAX=inner, $
				PEAK_INDEX=peak, RANGE_MINMAX=range, $
				GAUSSIAN_FIT=fit_gaussian, SKY_FIT=sky_fit, $
				LORENTZIAN_FIT=fit_Lorentz, MOFFAT_FIT=fit_Moffat, $
				FIT_PARAMETERS=fit_params, SIGMA_PAR=sig_par, $
				MINVAL_FIT=fit_minval, MAX_ITER_FIT=maxit, CHISQ=chisq )
end
