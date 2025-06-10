;+
; NAME:
;       fitPSF
; PURPOSE:
;       Peform non-linear least-squares fitting of chosen PSF model to
;       a 1D profile or 2D image (e.g. star).  Model can be either
;	modified (generalized) Lorentzian, Gaussian, or Moffat function.
;	Function returns a structure variable containing all fit parameters.
; CALLING:
;       modelParamStructure = fitPSF( data, [ /LOR, /GAU, MOF ]  )
; INPUTS:
;       data = 1D or 2D array (e.g. spectrum or image).
; KEYWORDS (in):
;	/LORENTZIAN_FIT : non-Lin-Lsq fit of modified Lorentzian (Cauchy) function
;                     giving usually better fit than Gaussian or Moffat.
;	/GAUSSIAN_FIT : nonlinear Least-squares fit of Gaussian function to data.
;	/MOFFAT_FIT : non-Lin-Lsq fit of Moffat function, also better than Gaussian.
;	/AIRY_FIT : non-Lin-Lsq fit of Airy diffraction pattern (with centrol obscuration).
;	/SKY_FIT : fit for constant param (sky offset) in non-Lin-Lsq fits.
;	/INNER_MAX : find maximum value within inner 80% of data (ignore edges).
;       MAX_ITER_FIT = maximum # of iterations to use in Lsq fits, default=33.
;	DOMAIN_FIT_FACTOR = factor of f.w.h.m. to specify size of fit domain (default=7)
;	NAME = optional string naming the data.
;	WAVELEN = wavelength of data in microns (floating point,  optional).
; KEYWORDS (out):
;       FIT_PARAMETERS = an array containing the parameters resulting from fits.
;	SIGMA_PARAMS = array containing standard deviation of the parameters.
; RESULT:
;	Function returns a structure variable containing all parameter input and results.
; EXTERNAL CALLS:
;       function gaussian
;       function Lorentzian
;       function Moffat
;       function Airy
;       pro non_Lin_Lsq
; PROCEDURE:
;       Find maximum point of data to use as initial centroid, get emprical FWHM to
;	use as intial parameters guess, then use pro non_Lin_Lsq.
; HISTORY:
;       Written: Frank Varosi at UF Astron. dept. 2007.
;       F.V. @ UFAstro. 2016: param min threshold must be > 0 for Lorentzian fits.
;       F.V. @ UFAstro. 2017: improved param min & max thresholds for all fits.
;-

function fitPSF, data, AIRY=fit_Airy, LORENTZIAN=fit_Lorentz, MOFFAT=fit_Moffat, $
                 GAUSSIAN=fit_gaussian, SKY_FIT=sky_fit, INNER_MAX=inner, TOLERANCE=ftol, $
                 MINVAL_FIT=fit_minval, MAX_ITER_FIT=maxit, NAME=name, WAVELEN=waveLen, $
                 INFO_FIT=infofit, DOMAIN_FIT_FACTOR=domainfac, POISSON_WEIGHTING=poiswt

sd = size( data )
ndim = sd[0]

if (ndim LT 1) OR (ndim GT 2) then begin
	message,"expecting 1D or 2D array",/INFO
	return,(0)
   endif

if min( [sd[1:ndim]] ) LT 5 then begin
	message,"data array size is too small",/INFO
	return,(0)
   endif

if keyword_set( poiswt ) then begin
	if( poiswt gt 0.9 ) then poiswt = 1e-4  ;;default min threshold for Poisson weighting.
   endif

if N_elements( name ) ne 1 then name = "?"
if N_elements( waveLen ) ne 1 then waveLen = -1.0

if keyword_set( sky_fit ) then begin
   pfmin = 0.1
   pfmax = 2.3
endif else begin
   pfmin = 0.5
   pfmax = 1.5
endelse

CASE ndim OF

2: BEGIN
	;; get initial guesses for 2-D standard params:

	fwhmi = fullwid_halfmax( data, CENTROID=cntrds, PEAK=peaks )

	if N_elements( domainfac ) ne 1 then domainfac = 3
	domainfac = domainfac > 0.5
	np = fix( domainfac * fwhmi ) > 3
	Lp = sd[1:2] - 1
	rp = ( [ [peaks - np] , [peaks + np + 1] ] > 0 ) < [[Lp],[Lp]]
	datpsf = float( data[ rp[0,0]:rp[0,1], rp[1,0]:rp[1,1] ] )
	maxd = max( datpsf, mp )
	datpsf = datpsf/maxd
	sds = size( datpsf )
	peaks = [ mp MOD sds[1], mp/sds[1] ]
        sdm = sds[1:2]-1

	if keyword_set( fit_gaussian ) then begin
		fit_func = "Gaussian"
		fwfac = 2 * sqrt( 2 * aLog(2) )
		sigmas = fwhmi/fwfac
		fit_params = [ 1, peaks, sigmas ]
                parfac = [ maxd, replicate(1,4) ]
                pkerr = ceil(fwhmi) > 2
                pmins = [ pfmin, (peaks-pkerr)>1,       sigmas/4 ]
                pmaxs = [ pfmax, (peaks+pkerr)<sdm, sigmas*4 ]
	 endif else if keyword_set( fit_Airy ) then begin
		fit_func = "Airy"
		fit_params = [ 1, peaks, 1.0, 0.12 ]
		pfix = 4
		fwfac = 2.
		parfac = [ maxd, replicate(1,5) ]
                pkerr = ceil(fwhmi) > 2
                pmins = [ pfmin, (peaks-pkerr)>1,       0.1, 0 ]
                pmaxs = [ pfmax, (peaks+pkerr)<sdm, 10, 0.5]
	  endif else if keyword_set( fit_Moffat ) then begin
		fit_func = "Moffat"
		radii = (fwhmi/2.0) > 1.0
		beta = 2.0
		alphas = radii/sqrt(2^(1./beta) - 1)
		fit_params = [ 1, peaks, alphas, beta ]
		fwfac = 2.
		parfac = [ maxd, replicate(1,5) ]
                pkerr = ceil(radii) > 2
		pmins = [ pfmin, (peaks-pkerr)>1,   alphas/3, 0.05 ]
		pmaxs = [ pfmax, (peaks+pkerr)<sdm, alphas*3, 9.0 ]
	   endif else begin
		fit_func = "Lorentzian"
		radii = (fwhmi/2.0) > 1.0
		power = 2
		pscales = 1e-3/fwhmi
		fit_params = [ 1, peaks, radii, pscales, power ]
		fwfac = 2.0
		parfac = [ maxd, replicate(1,7) ]
                pkerr = ceil(radii) > 2
		pmins = [ pfmin, (peaks-pkerr)>1,   radii/3, 1e-9,  1e-9,  0.1 ]
		pmaxs = [ pfmax, (peaks+pkerr)<sdm, radii*3, 0.12,  0.12,  7.0 ]
             endelse

        if !DEBUG then begin
              print, fit_func
              print, fit_params, pmins, pmaxs
              if( !DEBUG gt 2 ) then stop
           endif
           
	if keyword_set( sky_fit ) then begin
		fit_params = [ fit_params, 0 ]
		parfac = [ parfac, maxd ]
		pmins = [ pmins, -1.2 ]
		pmaxs = [ pmaxs,  1.2 ]
	   endif

	if N_elements( maxit ) NE 1 then maxit=77
	sig_par = fltarr( N_elements( fit_params ) )

	sdpsf = size( datpsf )
	xin = gridxy( sdpsf[1], sdpsf[2] )
        chisq = -1.0
	if (!DEBUG GT 2) then stop

	non_Lin_Lsq, xin, datpsf, fit_params, sig_par, chisq, FUN=fit_func, MAX_IT=maxit, TOL=ftol, $
			  PAR_FIX=pfix, PAR_MIN=pmins, PAR_MAX=pmaxs, INFO=infofit, POISS=poiswt

	if keyword_set( fit_Moffat ) then begin
		binv = 1/fit_params[5]
		fwhms = 2 * fit_params[3:4] * sqrt( 2^binv - 1 )
	 endif else if keyword_set( fit_Airy ) then begin
		fwhms = fwhmi
	  endif else fwhms = fit_params[3:4] * fwfac

	fit_params[1] = fit_params[1] + rp[0,0]
	fit_params[2] = fit_params[2] + rp[1,0]
	fit_params = parfac * fit_params
	sig_par = parfac * sig_par
	skyfit = 0.0
	if keyword_set( sky_fit ) then skyfit = fit_params[ N_elements(fit_params) - 1 ]

	psfgenfit = { model:fit_func, name:name, waveLen:waveLen, $
			cx:fit_params[1]+0.5, cy:fit_params[2]+0.5, $
			fwhmx:fwhms[0], fwhmy:fwhms[1], $
			skyfit:skyfit, $
			params:fit_params, $
			errors:sig_par, chisq:chisq, fitdomain:rp }

	if keyword_set( fit_gaussian ) then begin

		psfmodfit = create_struct( psfgenfit, "sigx", fit_params[3], $
							"sigy", fit_params[4] )

	 endif else if keyword_set( fit_Airy ) then begin

		psfmodfit = create_struct( psfgenfit, "pscale", fit_params[3], $
							"obscure", fit_params[4] )

	  endif else if keyword_set( fit_Moffat ) then begin

		psfmodfit = create_struct( psfgenfit, "beta", fit_params[5], $
							"alphax", fit_params[3], $
							"alphay", fit_params[4] )
	   endif else begin

		psfmodfit = create_struct( psfgenfit, "power", fit_params[7], $
							"radx", fit_params[3], $
							"rady", fit_params[4], $
							"psx", fit_params[5], $
							"psy", fit_params[6] )
	    endelse

	return, psfmodfit
     END

1: BEGIN
	;; get initial guesses for 1-D standard params:

	fwhmi = fullwid_halfmax( data, CENTROID=cntrd, PEAK=peak )

	L = N_elements( data )-1
	if N_elements( domainfac ) ne 1 then domainfac = 3
	domainfac = domainfac > 1
	np = fix( domainfac * fwhmi ) > 3
	rp = ( [ peak - np , peak + np + 1 ] > 0 ) < L
	prof = float( data[rp[0]:rp[1]] )
	maxd = max( prof, mp )
	prof = prof/maxd

	if N_elements( fit_minval ) EQ 1 then begin
		fit_mins = fit_minval/maxd
		w = where( prof GE fit_mins, nw )
		if (nw LT 5) then begin
			message,"need 5 values > specified minval",/INFO
			print," minval =",fit_mins," max-profile =",m
			return,fwhmi
		  endif else if (nw LT N_elements( prof )) then begin
			rp = rp(0) + [w(0),w(nw-1)]
			prof = prof( w(0):w(nw-1) )
			m = max( prof, mp )
		   endif
	   endif

        npt = N_elements( prof )

        if keyword_set( fit_gaussian ) then begin
		fit_func = "gaussian"
		fwfac = 2 * sqrt( 2 * aLog(2) )
		sigma = fwhmi/fwfac
		fit_params = [ 1, mp, sigma ]
		parfac = [ maxd, 1, 1 ]
                pkerr = ceil(fwhmi) > 2
                pmins = [ pfmin, (mp-pkerr)>1,       sigma/4 ]
                pmaxs = [ pfmax, (mp+pkerr)<(npt-2), sigma*4 ]
	 endif else if keyword_set( fit_Airy ) then begin
		fit_func = "Airy"
		fit_params = [ 1, mp, 1.0, 0.12 ]
		fwfac = 2.
		parfac = [ maxd, 1, 1, 1 ]
                pkerr = ceil(fwhmi) > 2
		pmins = [ pfmin, (mp-pkerr)>1,       0.1, 0 ]
                pmaxs = [ pfmax, (mp+pkerr)<(npt-2), 10, 0.5]
		pfix = 3
	  endif else if keyword_set( fit_moffat ) then begin
		fit_func = "moffat"
		radius = (fwhmi/2.0) > 1.0
		beta = 2.0
		alpha = radius/sqrt(2^(1./beta) - 1)
		fit_params = [ 1, mp, alpha, beta ]
		fwfac = 2.
		parfac = [ maxd, 1, 1, 1 ]
                pkerr = ceil(radius) > 2
                pmins = [ pfmin, (mp-pkerr)>1,       alpha/3, 0.05 ]
                pmaxs = [ pfmax, (mp+pkerr)<(npt-2), alpha*3, 9.0 ]
	   endif else begin
		fit_func = "Lorentzian"
		radius = (fwhmi/2.0) > 1.0
		power = 2
		pscale = 1e-3/fwhmi
		fit_params = [ 1, mp, radius, pscale, power ]
		fwfac = 2.
		parfac = [ maxd, 1, 1, 1, 1 ]
                pkerr = ceil(radius) > 2
		pmins = [ pfmin, (mp-pkerr)>1,       radius/3, 1e-9, 0.1 ]
                pmaxs = [ pfmax, (mp+pkerr)<(npt-2), radius*3, 0.12, 7.0 ]
	    endelse

        if !DEBUG then begin
              print, fit_func
              print, fit_params, pmins, pmaxs
              if( !DEBUG gt 2 ) then stop
           endif
           
	if keyword_set( sky_fit ) then begin
		fit_params = [ fit_params, 0 ]
		parfac = [ parfac, maxd ]
                pmins = [ pmins, -1.2 ]
                pmaxs = [ pmaxs,  1.2 ]
             endif

	if N_elements( maxit ) NE 1 then maxit=77
	sig_par = fltarr( N_elements( fit_params ) )
        chisq = -1.0
	if (!DEBUG GT 2) then stop

	if (prof[(mp-1)>0] gt 1.e-9) OR $
	   (prof[(mp+1)<(npt-1)] gt 1.e-9) then begin

		non_Lin_Lsq, indgen(npt), prof, fit_params, sig_par, chisq, FUNC=fit_func, MAX_IT=maxit, $
                             PAR_FIX=pfix, PAR_MIN=pmins, PAR_MAX=pmaxs, INFO=infofit, POIS=poiswt, TOL=ftol

		if keyword_set( fit_Moffat ) then begin
			binv = 1/fit_params[3]
			fwhm = 2 * fit_params[2] * sqrt( 2^binv - 1 )
		 endif else if keyword_set( fit_Airy ) then begin
			fwhms = fwhmi
		  endif else fwhm = fit_params[2] * fwfac

	  endif else begin

		fwhm = 0.5
		fit_params(2) = fwhm/fwfac
	   endelse

	fit_params[1] = fit_params[1] + rp[0]
	fit_params = parfac * fit_params
	sig_par = parfac * sig_par
	skyfit = 0.0
	if keyword_set( sky_fit ) then skyfit = fit_params[ N_elements(fit_params) - 1 ]

	psfgenfit = { model:fit_func, name:name, waveLen:waveLen, $
			cntrd:fit_params[1]+0.5, fwhm:fwhm, $
			skyfit:skyfit, $
			params:fit_params, $
			errors:sig_par, chisq:chisq, fitdomain:rp }

	if keyword_set( fit_gaussian ) then begin

		psfmodfit = create_struct( psfgenfit, "sigma", fit_params[2] )

	 endif else if keyword_set( fit_Airy ) then begin

		psfmodfit = create_struct( psfgenfit, "pscale", fit_params[2], $
							"obscure", fit_params[3] )

	  endif else if keyword_set( fit_Moffat ) then begin

		psfmodfit = create_struct( psfgenfit, "alpha", fit_params[2], $
							"beta", fit_params[3] )
	   endif else begin

		psfmodfit = create_struct( psfgenfit, "radius", fit_params[2], $
							"pscale", fit_params[3], $
							"power", fit_params[4] )
	    endelse

	return, psfmodfit
     END

else: BEGIN
	message,"expecting 1D or 2D array",/INFO
	return,(0)
	END
ENDCASE
end
