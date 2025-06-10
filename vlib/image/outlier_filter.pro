;+
; NAME:
;	outlier_filter
;
; PURPOSE:
;	Replace outlier (bad) pixels in an image with the local mean or median.
;	Outliers are replaced if value exceeds N_SIGMA times local standard
;	deviation (assuming that good data pixels do not deviate that much).
;	This procedure uses the histogram of image with reverse indices
;	to home in on pixels that have values outside the distribution
;	of most pixel values, and so is faster and usually better than
;	function sigma_filter, except when bad pixel values are within the
;	distribution of good pixels.
;
; CALLING:
;	outlier_filter, image, box_width, N_SIGMA=#
;
; INPUTS:
;	image = 2-D array (this input will be modified).
;
;	box_width = width of square box in which to test statistics of
;		outlier candidate pixels, units in # pixels (default=3).
;
; KEYWORDS:
;
;	N_SIGMA = number of standard deviations flagging outliers (float),
;			minimum = 1, default = 5. 
;
;	STOP = integer, the checking/replacing of outliers will stop after this
;		number of candidates are found to be NOT outliers (default=7).
;
;	RADIUS = specify test box size with radius, so box_width = 2*radius+1.
;
;	/MEDIAN : causes the median of values in test box to be used as
;		the replacement value, and in estimation of standard deviation.
;
;	/NO_MIN_OUT : outlier pixels related to minimum extremes of image
;		are NOT checked or replaced. This takes precedence over /NO_MAX.
;
;	/NO_MAX_OUT : outlier pixels related to maximum extremes of image
;		are NOT checked or replaced.
;
;	/MONITOR : prints information about % of pixels replaced.
;
; OUTPUT:
;	image = the input array is modified by replacing outliers as found.
;
; KEYWORD OUTPUT:
;
;	N_CHANGE = total # of pixels changed, optional output.
;
; NOTES:
;	For a flat gaussian noise image:
;		N_sigma = 2 replaces about 5% of pixels,
;		N_sigma = 3 replaces about 1% of pixels,
;		N_sigma = 4 replaces about 0.5% of pixels, and so on.
;	Setting /MEDIAN causes less pixels to be replaced and at same time
;	is then more successful at replacing adjacent pairs of outlier pixels.
;	The algorithm is semi-iterative since the image is modified during
;	the processing, thus changing the local deviations in test boxes,
;	and this feature actually helps to remove adjacent outliers.
;	Increasing box_width (or RADIUS) will usually replace more pixels.
;	Setting STOP= larger values causes more pixels to be checked and so
;	takes longer time, and results in more pixels being replaced,
;	whereas STOP=0 will replace only the maximum and/or minimum pixels
;	(if they are outliers, and if they occur first in reverse indices).
;
; PROCEDURE:
;	Possible outliers are found using the reverse indices from the
;	histogram of image. Starting from the maximum bin and working down,
;	each outlier candidate is checked by finding the mean (or median) and
;	standard deviation of pixels in box centered at candidate pixel
;	of the image (excluding that pixel), and if the center value exceeds
;	specified number of standard deviations from the mean (or median),
;	it is replaced by the mean (or median) in box. The process is repeated
;	starting at the minimum bin and working to more positive values.
;	This method of using histograms is faster than function sigma_filter
;	for large images, and is more successful at removing outliers.
; HISTORY:
;	Written, 1997, Frank Varosi, HSTX @ NASA/GSFC
;	FV, 8/97, for case of int/Long image, histogram binsize must be >= 1.
;	FV, 2017, UF, handle stupid/rare case of constant image.
;-

pro outlier_filter, image, box_width, N_SIGMA=Nsigma, N_CHANGE=nchange, $
			MONITOR=monitor, RADIUS=radius, MEDIAN=mm, STOP=stop, $
				NO_MIN_OUT=no_min, NO_MAX_OUT=no_max
	sim = size( image )
	npix = sim[sim[0]+2]
	imvtype = sim[sim[0]+1]

	if sim[0] NE 2 then begin
		print,"syntax:	outlier_filter, image, box_width, N_SIGMA="
		return
	   endif

	if N_elements( radius ) EQ 1 then  box_width = 2*radius+1  else begin
		if N_elements( box_width ) NE 1 then box_width=3
		box_width = 2*(fix( box_width )/2) + 1	;make sure width is odd.
	   endelse

	if (box_width LT 3) then return
	if N_elements( Nsigma ) NE 1 then Nsigma = 5
	Nsigma = Nsigma > 1

	minv = min( image, MAX=maxv )
        range = maxv - minv
        if( range LE 0 ) then return

        if( imvtype LE 3 or imvtype GE 12 ) then begin ;case of int/Long image.
           if( range LT 2 ) then return
        endif

	binsize = ( float( maxv ) - minv )/1000
	if (imvtype LE 3 or imvtype GE 12 ) then binsize = binsize > 1
	hx = histogram( image, BINS=binsize, REV=revix, MIN=minv, MAX=maxv )
	nbin = N_elements( hx )
	count = 0		;next, try to get better resolution on histogram

	while (hx[0] ne 1) and (hx[nbin-1] ne 1) and (count LT 4) do begin
		count = count+1
		nbin = 2*nbin
		binsize = ( float( maxv ) - minv )/nbin
		if( imvtype LE 3 or imvtype GE 12 ) then begin		;case of int/Long image.
			if( binsize LT 1 ) then count=4
			binsize = binsize > 1
		   endif
		hx = histogram( image, BIN=binsize, REV=revix,MIN=minv,MAX=maxv)
		nbin = N_elements( hx )
		if !DEBUG then begin
			help,nbin
			print,hx[0],hx[nbin-1]
		   endif
	 endwhile

	wp = where( (hx GT 0) and (hx LT 32000), nwh )
        if( nwh LE 0 ) then return

	kstart = [ nwh-1, 0 ]
	kincr = [ -1, 1 ]

	nx = sim[1]
	Lx = nx-1
	Ly = sim[2]-1
	brad = fix( box_width/2 )
	nchange = 0L
	if N_elements( stop ) ne 1 then stop = 7

;first pass is to start from max value and work down replacing outliers...
;second pass starts from min value and works up replacing outliers...

if keyword_set( no_min ) then begin
	pass0 = 0
	pass1 = 0
 endif else if keyword_set( no_max ) then begin
	pass0 = 1
	pass1 = 1
 endif else begin
	pass0 = 0
	pass1 = 1
  endelse

for pass = pass0, pass1 do begin

	k = kstart[pass]
	kinc = kincr[pass]
	nok = 0L
	if !DEBUG then help,k,kinc

	Repeat Begin

	    rk = revix[ revix[wp[k]] : revix[wp[k]+1]-1 ]
	    rx = rk MOD nx
	    ry = rk/nx
	    xmin = ( rx - brad ) > 0
	    xmax = ( rx + brad ) < Lx
	    ymin = ( ry - brad ) > 0
	    ymax = ( ry + brad ) < Ly
	    k = k + kinc

	    for i=0,N_elements( rk )-1 do begin

		p = rk[i]
		ibox = image(xmin[i]:xmax[i],ymin[i]:ymax[i])
		nbox = N_elements( ibox )-1

		if keyword_set( mm ) then mean = median( ibox ) $
				else mean = ( total( ibox ) - image[p] )/nbox

		varp = (image[p] - mean)^2
		vari = ( total( (ibox - mean)^2 ) - varp )/nbox

		if varp GE Nsigma*vari then begin
			image[p] = mean
			nchange = nchange + 1
		 endif else nok = nok+1
	     endfor

	 endrep until (nok GE stop) or (k LT 0) or (k GE nwh)

	if !DEBUG then help,k,nok,nchange
   endfor

	if keyword_set( monitor ) then $
		print, nchange*100./npix, box_width, Nsigma, $
		FORM="(F6.2,' % of pixels replaced, box=',I1,', N_sigma=',F4.1)"
end
