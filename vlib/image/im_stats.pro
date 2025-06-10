;+
; NAME:
;	im_stats
;
; PURPOSE:
;	Compute and return statistics of an image in structured variable,
;	including:  Gaussian noise st.dev., sky (background) Level,
;	            FWHM, centroid, min, max, rms, average of image.
;
; CALLING:
;	stat_struct = im_stats( image, /GET_FWHM )
;
; INPUTS:
;	image = 2D array
;
; KEYWORDS:
;	BOX_SIZE = width of moving box in which to compute local variances,
;	           then used to estimate most probable standard deviation.
;
;	NOISE_MODEL = string: "GAUSSIAN", the default, or "POISSON"
;	NAME = string, used in returned structure to name the image.
;	/PRINT_STATS causes results of analysis to be briefly printed.
;
;	GET_FWHM = estimated Full Width Half Max by one of 3 methods:
;	           1 : Linear interpolation,
;	           2 : nonlinear Lsq fit of Gaussian function,
;	           3 : nonlinear Lsq fit of generalized Lorentzian function.
;		(if NAME contains the string "star" then GET_FWHM is set).
;
;	N_STRUCT = # of IM_STATS structure element to replicate and return,
;	                overrides all other inputs.
;
; OUTPUT:
;	Returns a structure containing results of statistical analysis.
;
; EXTERNAL CALLS:
;	function sky_noise
;	function N_struct
;	function filter_image
;	function FullWid_HalfMax
;
; COMMON BLOCKS:
;	common im_stats, struct
; PROCEDURE:
;	Call functions sky_noise and FullWid_HalfMax.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	F.V.1994, set keyword /SKY_FIT in function FullWid_HalfMax.
;	F.V.2011, changed RMS calc to include only pixels > (sky + noise).
;	F.V.2013, added Ncoadds to structure, for back-computing noise/frame.
;-

function im_stats, image, NOISE_MODEL=noise_model, BOX_SIZE=box_size, $
                   GET_FWHM=get_fwhm, NAME=name, SMOOTH_WIDTH=smwid, $
				N_STRUCT=nstr, PRINT_STATS=print
  common im_stats, struct

	if N_struct( struct ) NE 1 then begin
		struct = { IM_STATS_v5, $
                           name:"",		$
                           mjd: 0.0d0, $
                           expTime:0.0,         $
                           sigma_noise:0.0,	$
                           gaussian:0,		$
                           poisson:0,		$
                           sky_Level:0.0,	$
                           fitmethod:"",	$
                           fwhm:0.0,		$
                           fwhm_xy:fltarr(2),	$
                           cent_xy:fltarr(2),	$
                           peak_xy:intarr(2),	$
                           size_xy:intarr(2),	$
                           arcs_pixel:fltarr(2),$
                           min:0.0, max:0.0,	$
                           average:0.0, $
                           rms:0.0,     $
                           Object:"",   $
                           Filter:"",   $
                           ReadMode:"", $
                           Ncoadds:1L,  $
                           FrameTime:0.0, ChopPA:0.0, ChopThrow:0.0 }
                message,"defined structure {IM_STATS_v5}",/INFO
	   endif

	if keyword_set( nstr ) then return, replicate( {IM_STATS_v5}, nstr )

	stats = struct
	sim = size( image )

	if (sim[0] NE 2) then begin
		message,"first arg. must be an image",/INFO
		return, stats
	  endif

	stats.size_xy = sim[1:2]
	stats.average = total( image )/N_elements( image )

	if N_elements( name ) EQ 1 then stats.name = name
	if N_elements( noise_model ) NE 1 then noise_model = "GAUSSIAN"

	CASE strupcase( noise_model ) OF

		"GAUSSIAN": BEGIN
				sigma = sky_noise( image, sky, BOX=box_size )
				stats.gaussian=1
			    END

		"POISSON": BEGIN
				sigma=0
				sky=0
				stats.poisson=1
			   END

		"BOTH": BEGIN
				sigma = sky_noise( image, sky, BOX=box_size )
				stats.gaussian=1
				stats.poisson=1
			    END

		else: BEGIN
				sigma=0
				sky=0
			END
	  ENDCASE

	stats.sigma_noise = sigma
	stats.sky_Level = sky
        w = where( image gt (sky + sigma), nw )

        if( nw gt 0 ) then begin
           imw = image[w]
           stats.rms = sqrt( total( imw*imw )/nw )
        endif

	if keyword_set( get_fwhm ) OR $
	  (strpos( strupcase( stats.name ),"STAR") GE 0) then begin

		if N_elements( get_fwhm ) NE 1 then get_fwhm=1

                if keyword_set( smwid ) then begin

                   stats.fwhm_xy = FullWid_HalfMax( filter_image( image,/ALL,/ITER,SM=smwid ), $
                                                    CENT=cent, /SKY_FIT, $
                                                    PEAK=peak, RANGE=range, $
                                                    MINVAL_FIT=(sigma+sky), $
                                                    GAUSSIAN_FIT=(get_fwhm EQ 2), $
                                                    LORENTZIAN=(get_fwhm EQ 3),$
                                                    MOFFAT_FIT=(get_fwhm EQ 4) )
                endif else begin

                   stats.fwhm_xy = FullWid_HalfMax( image, CENT=cent, /SKY_FIT, $
                                                    PEAK=peak, RANGE=range, $
                                                    MINVAL_FIT=(sigma+sky), $
                                                    GAUSSIAN_FIT=(get_fwhm EQ 2), $
                                                    LORENTZIAN=(get_fwhm EQ 3),$
                                                    MOFFAT_FIT=(get_fwhm EQ 4) )
                endelse

		stats.fwhm = total( stats.fwhm_xy )/2

                if N_elements( range ) eq 2 then begin
                   stats.min = range[0]
                   stats.max = range[1]
                endif

                if N_elements( peak ) eq 2 then begin
                   stats.peak_xy = peak
                   stats.cent_xy = cent
                   px = peak[0]
                   py = peak[1]
                endif

                CASE get_fwhm OF
                   2:    stats.fitmethod = "Gaussian"
                   3:    stats.fitmethod = "Lorentzian"
                   4:    stats.fitmethod = "Moffat"
                   else: stats.fitmethod = "Empirical"
                ENDCASE

	 endif else begin

		stats.max = max( median( image, 3 ), imax, MIN=minim )
		stats.min = minim
		px = imax MOD sim[1]
		py = imax/sim[1]
		stats.peak_xy = [px,py]
	   endelse

	if keyword_set( print ) then begin

		if (stats.sigma_noise GT 0) then begin
			SNR = stats.rms / stats.sigma_noise
			SNRmax = (stats.max-stats.sky_Level)/stats.sigma_noise
		  endif else begin
			SNR = 1
			SNRmax = 1
		   endelse

		if (stats.fwhm GT 0) then begin
			print," estimated     st.dev.    SNR      SNR     FWHM"
			print," sky Level     noise      rms      max    x    y"
			print, stats.sky_Level, stats.sigma_noise, SNR, SNRmax,$
				stats.fwhm_xy, FORMAT = "(2G10.3,2F9.1,2F7.2)"
		  endif else begin
			print," estimated     st.dev.    SNR      SNR"
			print," sky Level     noise      rms      max"
			print, stats.sky_Level, stats.sigma_noise, SNR, SNRmax,$
						FORMAT = "(2G10.3,2F9.1)"
		   endelse
	   endif

return, stats
end
