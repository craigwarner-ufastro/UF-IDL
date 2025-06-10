;+
; NAME:
;       FullWid_HalfMax
; PURPOSE:
;       Return the full-width-half-max (FWHM) around the peak (maximum)
;       in a 1D profile, 2D image (e.g. star), or 3D volumetric-data.
;       FWHM is determined for each dimension independently by:
;       Linear interpolations (default), or fitting Gaussian functions,
;       or by fitting modified Lorentzian functions, to each profiles.
; CALLING:
;       fwhm_xy = FullWid_HalfMax( data, CENTROID=cxy, /GAUSSIAN )
; INPUTS:
;       data = 1D, 2D, or 3D array (e.g. spectrum, image, or data cube).
; KEYWORDS (in):
;	/GAUSSIAN : nonlinear Least-squares fit of Gaussian function to 
;                     profile in each dimension to get FWHM,
;                     (default is to estimate FWHM by Linear interpolation).
;	/LORENTZIAN : non-Lin-Lsq fit modified Lorentzian (Cauchy) function
;                     to profile in each dimension to estimate FWHM, and
;                     other parameters, giving more freedom than Gaussian.
;	/MOFFAT : non-Lin-Lsq fit of Moffat function, also better than Gaussian.
;	/SKY_FIT : include a constant param (sky offset) in non-Lin-Lsq fits.
;	/AVERAGE : return the average FWHM, instead of one for each dimension.
;	/INNER_MAX : find maximum value within inner 80% of data (ignore edges).
;       MAX_ITER_FIT = maximum # of iterations to use in Lsq fits, default=77.
; KEYWORDS (out):
;       FIT_PARAMETERS = an array containing the parameters determined by the
;              Lsq fits to profiles, thus a matrix if data is > 1 dimensional.
;	SIGMA_PARAMS = array containing standard deviation of the parameters.
;       CENTROID = array, the centroid coordinate of profile in each dim,
;                     if either /GAUSSIAN or /LORENTZIAN requested.
;                     Convention is that center of pixel is at 0.5 + pixel#.
;       PEAK_INDEX = array, the pixel # at maximum of profile in each dim.
;       RANGE_MINMAX = array containing the minimum, maximum value of data.
; RESULT:
;       Function returns an array giving FWHM for each dimension,
;       or if /AVER set, a scalar giving the average FWHM around the peak.
; EXTERNAL CALLS:
;       function fitPSF
; PROCEDURE:
;       Simply find maximum point of data and
;       call function FullWid_HalfMax recursively for profile in each dimension.
; HISTORY:
;       Written: Frank Varosi NASA/GSFC 1992.
;	F.V. 1994, /SKY_FIT option for Non-Lin-Lsq fitting to model sky offset.
;	F.V. 2007 at UFastro: moved non-lin-lsq fitting to fitPSF.pro
;	F.V. 2017 at UFastro: removed requirement that max of data > 0.
;-

function FullWid_HalfMax, data, AVERAGE=aver, CENTROID=cntrd, INNER_MAX=inner, $
				PEAK_INDEX=peak, RANGE_MINMAX=range, $
				GAUSSIAN_FIT=fit_gaussian, SKY_FIT=sky_fit, $
				LORENTZIAN_FIT=fit_Lorentz, MOFFAT_FIT=fit_Moffat, $
				MINVAL_FIT=fit_minval, MAX_ITER_FIT=maxit, CHISQ=chisq, $
				FIT_PARAMETERS=fit_params, SIGMA_PARAMS=sig_par
sd = size( data )

if (sd[0] LT 1) OR (sd[0] GT 3) then begin
	message,"expecting 1D or 2D or 3D array",/INFO
	return,(0)
   endif

if min( [sd(1:sd[0])] ) LT 5 then begin
	message,"data array size is too small",/INFO
	return, replicate( 0, sd[0] )
   endif

if N_elements( peak ) NE sd[0] then begin

	if keyword_set( inner ) then begin
		xL = sd[1]/10
		xH = sd[1]-xL-1
		yL = sd[2]/10
		yH = sd[2]-xL-1
		maxd = float( max( data(xL:xH,yL:yH), imax, MIN=mind ) )
		w = where( data EQ maxd, nmax )
		imax = w[0]
	 endif else maxd = float( max( data, imax, MIN=mind ) )

	if (maxd EQ mind) then begin
		message,"data should be non-constant",/INFO
		return, replicate( 0, sd[0] )
	   endif

	range = [ mind, maxd ]
   endif

CASE sd[0] OF

3: BEGIN
	if N_elements( imax ) EQ 1 then peak = [ imax MOD sd[1], $
						(imax/sd[1]) MOD sd[2], $
						 imax/( sd[1] * sd[2] ) ]
	cntrd = peak
	px = peak[0]
	py = peak[1]
	pz = peak[2]

	fwhmx = FullWid_HalfMax( data[*,py,pz], CENTROID=cx, PEAK=px, SKY=sky_fit, $
			GAUSS=fit_gaussian, LORENTZ=fit_Lorentz, MOF=fit_Moffat, $
			MIN=fit_minval, FIT_PAR=parmx, SIG=sigx, MAX_IT=maxit )

	fwhmy = FullWid_HalfMax( reform( data[px,*,pz] ), CENT=cy, PEAK=py, SKY=sky_fit, $
			GAUSS=fit_gaussian, LORENTZ=fit_Lorentz, MOF=fit_Moffat, $
			MIN=fit_minval, FIT_PAR=parmy, SIG=sigy, MAX_IT=maxit )

	fwhmz = FullWid_HalfMax( reform( data[px,py,*] ), CENT=cz, PEAK=pz, SKY=sky_fit, $
			GAUSS=fit_gaussian, LORENTZ=fit_Lorentz, MOF=fit_Moffat, $
			MIN=fit_minval, FIT_PAR=parmz, SIG=sigz, MAX_IT=maxit )

	fwhm = [fwhmx,fwhmy,fwhmz]

	if (N_elements( parmx ) GT 0) AND $
	   (N_elements( parmy ) GT 0) AND $
	   (N_elements( parmz ) GT 0) then begin
		cntrd = [cx,cy,cz]
		fit_params = transpose( [[parmx],[parmy],[parmz]] )
		sig_par = transpose( [[sigx],[sigy],[sigz]] )
	   endif
     END

2: BEGIN
	if keyword_set( fit_gaussian ) OR $
	   keyword_set( fit_moffat ) OR $
	   keyword_set( fit_Lorentz ) then begin

		psfit = fitPSF( data, GAUSSIAN=fit_gaussian, LORENTZIAN=fit_Lorentz, $
                                MOFFAT=fit_Moffat, SKY_FIT=sky_fit, $
				INNER_MAX=inner, MINVAL_FIT=fit_minval, MAX_ITER_FIT=maxit )

		fwhm = [psfit.fwhmx, psfit.fwhmy]
		cntrd = [psfit.cx, psfit.cy]
		peak = fix( cntrd )
		fit_params = psfit.params
		sig_par = psfit.errors
		chisq = psfit.chisq

	 endif else begin

		if N_elements( imax ) EQ 1 then peak = [ imax MOD sd[1], imax/sd[1] ]
		cntrd = peak
		px = peak[0]
		py = peak[1]

		fwhmx = FullWid_HalfMax( data[*,py], CENTROID=cx, PEAK=px, SKY=sky_fit, $
                                         MIN=fit_minval, FIT_PAR=parmx, SIG=sigx, MAX_IT=maxit )

		fwhmy = FullWid_HalfMax( reform( data[px,*] ), CENT=cy, PEAK=py, SKY=sky_fit, $
                                         MIN=fit_minval, FIT_PAR=parmy, SIG=sigy, MAX_IT=maxit )

		fwhm = [fwhmx,fwhmy]

		if (N_elements( parmx ) GT 0) AND $
		   (N_elements( parmy ) GT 0) then begin
			cntrd = [cx,cy]
			fit_params = transpose( [[parmx],[parmy]] )
			sig_par = transpose( [[sigx],[sigy]] )
		   endif
	 endelse
     END

1: BEGIN
	if keyword_set( fit_gaussian ) OR $
	   keyword_set( fit_moffat ) OR $
	   keyword_set( fit_Lorentz ) then begin

		psfit = fitPSF( data, GAUSSIAN=fit_gaussian, MOFFAT=fit_Moffat, $
                                LORENTZIAN=fit_Lorentz, SKY_FIT=sky_fit, $
                                INNER_MAX=inner, MINVAL_FIT=fit_minval, MAX_ITER_FIT=maxit )

		fwhm = psfit.fwhm
		cntrd = psfit.cntrd
		peak = fix( cntrd )
		fit_params = psfit.params
		sig_par = psfit.errors

	 endif else begin

		if N_elements( imax ) EQ 1 then peak = imax
		peak = peak[0]
		cntrd = peak
		if N_elements( maxd ) NE 1 then maxd = data[ peak ]
		prof = float( data )/maxd
		wp = where( prof LT 0.5, nw )

		if (nw GT 1) then begin
			wpL = where( wp LT peak, nwL )
			if (nwL GT 0) then xL = wp[wpL[nwL-1]]
			wpG = where( wp GT peak, nwG )
			if (nwG GT 0) then xG = wp[wpG[0]]
		   endif

		if (N_elements( xL ) EQ 1) AND (N_elements( xG ) EQ 1) then begin

			pi = [xL,xL+1,xG-1,xG]
			pf = prof[ pi ]
			pfd = pf[1:*]-pf
			pfd[1]=1
			hx = (0.5-pf) * (pi[1:*]-pi)/pfd + pi
			fwhm = hx[2] - hx[0]

		  endif else fwhm = N_elements( data )/2
	   endelse

	return, fwhm
     END

else: BEGIN
	message,"expecting 1D or 2D or 3D array",/INFO
	return,(0)
	END
ENDCASE


 if keyword_set( aver ) then  return, total( fwhm )/N_elements( fwhm ) $
			else  return, fwhm
end
