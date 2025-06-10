;+
; NAME:
;	find_outliers
;
; PURPOSE:
;	Find outlier (bad) pixels in an image and return array of subscripts.
;	Outliers are flagged if value exceeds N_SIGMA times local standard
;	deviation (assuming that good data pixels do not deviate that much).
;	This procedure uses the histogram of image with reverse indices
;	to home in on pixels that have values outside the distribution
;	of most pixel values, and so is faster and usually better than
;	function sigma_filter, except when bad pixel values are within the
;	distribution of good pixels.
;
; CALLING:
;	subscripts = find_outliers( image, box_width, N_SIGMA=# )
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
;	STOP = integer, the checking for outliers will stop after this
;		number of candidates are found to be NOT outliers (default=9).
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
;	N_FOUND = total # of pixels found to be outliers, optional output.
;
; NOTES:
;	For a flat gaussian noise image:
;		N_sigma = 2 finds about 5% of pixels to be outliers,
;		N_sigma = 3 finds about 1% of pixels to be outliers,
;		N_sigma = 4 finds 0.5% of pixels to be outliers, and so on.
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
;	the subscript is added to array of subscripts. The process is repeated
;	starting at the minimum bin and working to more positive values.
;	This method of using histograms is faster than function sigma_filter
;	for large images, and is more successful at finding outliers.
; HISTORY:
;	Written, 1997, Frank Varosi, HSTX @ NASA/GSFC
;	FV, 8/97, for case of int/Long image, histogram binsize must be >= 1.
;-

function find_outliers, image, box_width, N_SIGMA=Nsigma, N_FOUND=nfound, $
			MONITOR=monitor, RADIUS=radius, MEDIAN=mm, STOP=stop, $
			NO_MIN_OUT=no_min, NO_MAX_OUT=no_max, NON_ZERO=nonzero

	sim = size( image )
	npix = sim(sim(0)+2)
	imvtype = sim(sim(0)+1)

	if sim(0) NE 2 then begin
		print,"syntax:"
		print,"	subscripts = find_outliers( image, box_width, N_SIGMA=)"
		return,0
	   endif

	if N_elements( radius ) EQ 1 then  box_width = 2*radius+1  else begin
		if N_elements( box_width ) NE 1 then box_width=3
		box_width = 2*(fix( box_width )/2) + 1	;make sure width is odd.
	   endelse

	if (box_width LT 3) then return,0
	if N_elements( Nsigma ) NE 1 then Nsigma = 5
	Nsigma = Nsigma > 1

	minv = min( image, MAX=maxv )
	binsize = ( float( maxv ) - minv )/1000
	if (imvtype LE 3) then binsize = binsize > 1
	hx = histogram( image, BINS=binsize, REV=revix, MIN=minv, MAX=maxv )
	nbin = N_elements( hx )
	count = 0		;next, try to get better resolution on histogram

	while (hx(0) ne 1) and (hx(nbin-1) ne 1) and (count LT 4) do begin
		count = count+1
		nbin = 2*nbin
		binsize = ( float( maxv ) - minv )/nbin
		if (imvtype LE 3) then begin		;case of int/Long image.
			if( binsize LT 1 ) then count=4
			binsize = binsize > 1
		   endif
		hx = histogram( image, BIN=binsize, REV=revix,MIN=minv,MAX=maxv)
		nbin = N_elements( hx )
		if !DEBUG then begin
			help,nbin
			print,hx(0),hx(nbin-1)
		   endif
	 endwhile

	wp = where( (hx GT 0) and (hx LT 32000), nwh )
	kstart = [ nwh-1, 0 ]
	kincr = [ -1, 1 ]

	nx = sim(1)
	Lx = nx-1
	Ly = sim(2)-1
	brad = fix( box_width/2 )
	nfound = 0L
	if N_elements( stop ) ne 1 then stop = 9

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

	k = kstart(pass)
	kinc = kincr(pass)
	nok = 0L
	if !DEBUG then help,k,kinc

	Repeat Begin

	  rk = revix( revix(wp(k)) : revix(wp(k)+1)-1 )
	  rx = rk MOD nx
	  ry = rk/nx
	  xmin = ( rx - brad ) > 0
	  xmax = ( rx + brad ) < Lx
	  ymin = ( ry - brad ) > 0
	  ymax = ( ry + brad ) < Ly
	  k = k + kinc

	  for i=0,N_elements( rk )-1 do begin

		p = rk(i)
		ibox = image(xmin(i):xmax(i),ymin(i):ymax(i))
		if keyword_set( nonzero ) then ibox = ibox( where( ibox ) )
		nbox = N_elements( ibox )-1

		if keyword_set( mm ) then mean = median( ibox ) $
				else mean = ( total( ibox ) - image(p) )/nbox

		varp = (image(p) - mean)^2
		vari = ( total( (ibox - mean)^2 ) - varp )/nbox

		if varp GE Nsigma*vari then begin
			if N_elements( wbad ) LE 0 then wbad=p else wbad=[wbad,p]
			nfound = nfound + 1
		 endif else nok = nok+1

	   endfor

	 endrep until (nok GE stop) or (k LT 0) or (k GE nwh)

	if !DEBUG then help,k,nok,nfound
   endfor

	if keyword_set( monitor ) then $
		print, nfound*100./npix, box_width, Nsigma, $
		FORM="(F6.2,' % of pixels found bad, box=',I1,', N_sigma=',F4.1)"

return, wbad
end
