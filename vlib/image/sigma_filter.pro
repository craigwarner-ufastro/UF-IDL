;+
; NAME:
;	sigma_filter
; PURPOSE:
;	Replace outlier (bad) pixels in an image with appropriate values,
;	or do the opposite: keep the outlier pixels and smooth all others.
;	Computes the mean and standard deviation of pixels in a box centered at 
;	each pixel of the image, but excluding the center pixel. If the center 
;	pixel value exceeds some number of standard deviations from the mean,
;	it is flagged for replacment by the mean (or median) in box,
;	or if /KEEP is set they are kept as is and rest of image is smoothed.
;	Note option to process pixels on the edges (/ALL_PIX).
;	However, the newer pro outlier_filter is faster on large images.
; CALLING:
;	Result = sigma_filter( image, box_width, N_sigma=(#), /ALL,/MON )
; INPUTS:
;	image = 2-D array (matrix).
;	box_width = width of square moving box, in # pixels (default = 3)
; KEYWORDS:
;	N_sigma = # standard deviations flagging outliers, floating point,
;			recommend > 2, default = 4.  For gaussian noise:
;			N_sigma = 2 smooths 5% of pixels, = 3 does 1%.
;	RADIUS = another way to specify box radius, so box_width = 2*radius+1.
;	/ALL_PIXELS causes computation to include edges of image,
;	/MEDIAN_MEAN cause the median filter of image to be used as
;			the estimator for the mean (instead of smooth).
;	/ITERATE causes sigma_filter to be applied recursively (max = 20 times)
;		until no more pixels change (only allowed when N_sigma >= 2).
;	/KEEP performs opposite task: pixels with values outside of specified
;		deviation are not changed, pixels within deviation are smoothed.
;	/MONITOR prints information about % pixels replaced.
; KEYWORD OUTPUTS (optional):
;	N_CHANGE = # of pixels changed (replaced with neighborhood mean).
;	VARIANCE = image of pixel neighborhood variances * (N_sigma)^2,
;	DEVIATION = image of pixel deviations from neighborhood means, squared.
; EXTERNAL CALLS:
;	function filter_image
;	function sigma_filter
; PROCEDURE:
;	Based on the LEE (Computer Graphics 197?) technique.
;	Compute mean over moving box-cars using smooth, subtract center values,
;	compute variance using smooth on deviations from mean,
;	check where pixel deviation from mean is within variance of box,
;	replace those pixels in smoothed image (mean) with orignal values,
;	return the resulting partial mean image.
; HISTORY:
;	Written, 1991, Frank Varosi, HSTX @ NASA/GSFC
;	F.V.1992, added optional keywords /ITER,/MONITOR,VAR=,DEV=,N_CHANGE=.
;	F.V.2016, do NOT use variable name "mean" since it is a function.
;	F.V.2017, added keyword option THRESHOLD_KEEP = do not change if mean > THRESH
;	F.V.2024, added option to filter a stack of images.
;-

function sigma_filter, image, box_width, N_SIGMA=Nsigma, ALL_PIXELS=all,   $
                       ITERATE=iterate, MONITOR=monitor, THRESHOLD_KEEP=threshkeep, $
                       KEEP_OUTLIERS=keepbad, RADIUS=radius, MEDIAN_MEAN=mm, $
                       N_CHANGE=nchange, VARIANCE_IMAGE=imvar, DEVIATION_IMAGE=imdev

	szim = size( image )

        if szim[0] eq 3 then begin
           imgsfilt = image
           for i=0,szim[3]-1 do begin
              imgsfilt[*,*,i] = sigma_filter( image[*,*,i], box_width, N_SIG=Nsigma,$
                                              ITER=iterate,MON=monitor,THRES=threshkeep,$
                                              KEEP=keepbad, RAD=radius, MED=mm, ALL=all,$
                                              N_CHANGE=nchange, VARIANCE=imvar, DEV=imdev )
           endfor
           return, imgsfilt
        endif else if szim[0] NE 2 then begin
           print,"syntax:    Result = sigma_filter( image, box_width )"
           print,"keywords:	N_SIGMA = # of standard deviations."
           return,0
        endif

	if N_elements( radius ) EQ 1 then  box_width = 2*radius+1  else begin
		if N_elements( box_width ) NE 1 then box_width=3
		box_width = 2*(fix( box_width )/2) + 1	;make sure width is odd.
	   endelse

	if (box_width LT 3) then return,image
	bw2 = box_width^2

	if keyword_set( mm ) then begin
           imgfilt = filter_image( image, MEDIAN=box_width, ALL=all )
        endif else imgfilt=( filter_image( image,SMO=box_width,ALL=all )*bw2 - image )/(bw2-1)

	if N_elements( Nsigma ) NE 1 then Nsigma=4
	if (Nsigma LE 0) then return, imgfilt

	imdev = (image - imgfilt)^2
	fact = float( Nsigma^2 )/(bw2-2)
	imvar = fact*( filter_image( imdev,SMO=box_width,ALL=all )*bw2 - imdev )

	if N_elements( threshkeep ) eq 1 then begin
           wok = where( (imdev LE imvar) OR (imgfilt ge threshkeep), nok )
        endif else if keyword_set( keepbad ) then begin
           wok = where( imdev GE imvar, nok )
        endif else  wok = where( imdev LE imvar, nok )

	npix = N_elements( image )
	nchange = npix - nok
	if keyword_set( monitor ) then print, nchange*100./npix, Nsigma, $
                                       FORM="(F6.2,' % of pixels replaced, N_sigma=',F3.1)"

	if (nok EQ npix) then return,image

	if (nok GT 0) then imgfilt[wok] = image[wok]

        if keyword_set( iterate ) AND (Nsigma GE 2) then begin
           if( iterate gt 1 ) then maxiter = iterate else maxiter=10
           iter=1
           while (nchange GT 0) AND (iter LT maxiter) do begin
              iter = iter+1
              imgfilt = sigma_filter( imgfilt, box_width, N_SIGMA=Nsigma, ALL=all, $
                                      N_CH=nchange, KEEP=keepbad, MON=monitor, THR=threshkeep )
           endwhile
        endif

return, imgfilt
end
