;+
; NAME:
;	filter_image
; PURPOSE:
;	Computes the average and/or median of pixels in moving box,
;	replacing center pixel with the computed average and/or median,
;		(using the IDL smooth or median functions).
;	The main reason for using this function is the options to
;	also process the pixels at edges and corners of image, and,
;	to apply iterative smoothing to approximate convolution with Gaussian,
;	and/or to convolve image with a Gaussian kernel.
;
; CALLING:
;	Result = filter_image( image, SMOOTH=box_width, /MEDIAN, /ALL )
; INPUT:
;	image = 2-D array (matrix)
;
; KEYWORDS:
;	SMOOTH = width of square box for moving average, in # pixels.
;	/SMOOTH  means use box width = 3 pixels for smoothing.
;
;	MEDIAN = width of square moving box for median filter, in # pixels.
;	/MEDIAN  means use box width = 3 pixels for median filter.
;
;	/ALL_PIXELS causes the edges of image to be filtered as well,
;		accomplished by reflecting pixels adjacent to edges outward.
;
;	/ITERATE : apply smooth(image,3) iteratively for a count of
;		[ (box_width-1)/2 = radius ] times, when box_width >= 5.
;		This is equivalent to convolution with a Gaussian PSF
;		of FWHM = 2 * sqrt( radius ) as radius gets large.
;		Note that /ALL_PIXELS is automatically applied,
;		giving better results in the iteration limit.
;		(also, MEDIAN keyword is ignored when /ITER is specified).
;		Can also specify FWHM = # pixels when using /ITER.
;
;	FWHM_GAUSSIAN = Full-width half-max of Gaussian to convolve with image. 
;			FWHM can be a single number (circular beam),
;			or 2 numbers giving axes of elliptical beam.
;		However, if /ITERATE is set then convolution with a Gaussian
;			is approximated by iteration of smooth( image, 3 ).
;
;	/NO_FT_CONVOL causes the convolution to be computed directly,
;		with IDL function convol.
;		The default is to use FFT when factors of size are all LE 13.
;		(note that external function convolve handles both cases)
; RESULT:
;	Function returns the smoothed, median filtered, or convolved image.
;	If both SMOOTH and MEDIAN are specified, median filter is applied first.
;
; EXAMPLES:
;	To apply 3x3 moving median filter and
;	then 3x3 moving average, both applied to all pixels:
;
;		Result = filter_image( image, /SMOOTH, /MEDIAN, /ALL )
;
;	To iteratively apply 3x3 moving average filter for 4 = (9-1)/2 times,
;	thus approximating convolution with Gaussian of FWHM = 2*sqrt(4) = 4 :
;
;		Result = filter_image( image, SMOOTH=9, /ITER )
;
;	To convolve all pixels with Gaussian of FWHM = 3.7 x 5.2 pixels:
;
;		Result = filter_image( image, FWHM=[3.7,5.2], /ALL )
;
; EXTERNAL CALLS:
;	function psf_gaussian
;	function convolve
;	function smooth_iter
;	pro factor		;all these called only if FWHM_GAUSS is specified.
; PROCEDURE:
;	If /ALL_PIXELS or /ITERATE keywords are set then
;	create a larger image by reflecting the edges outward,
;	then call the IDL median and/or smooth function on the larger image,
;	and just return the central part (the orginal size image).
; HISTORY:
;	Written, 1991, Frank Varosi, NASA/GSFC.
;	FV, 1992, added /ITERATE option.
;	FV, 1993, added FWHM_GAUSSIAN= option.
;	FV, 1999, option FWHM_GAUSSIAN=#pixels can be used with /ITER approx.
;	FV, 2012, mod to use FFT if # pixels size of Gaussian PSF > 33.
;	FV, 2020, use function smooth_iter if /ITERATE_SMOOTH is requested.
;-

function filter_image, image, SMOOTH=width_smooth, ITERATE_SMOOTH=iterate, $
			      MEDIAN=width_median, ALL_PIXELS=all_pixels, $
			      FWHM_GAUSSIAN=fwhmg, NO_FT_CONVOL=no_ft
	sim = size( image )
	Lx = sim[1]-1
	Ly = sim[2]-1

	if (sim(0) NE 2) OR (sim(4) LE 4) then begin
		message,"input must be an image (a matrix)",/INFO
		return,image
	   endif

	if keyword_set( iterate ) then begin
		nit = iterate > 1
		if keyword_set( width_smooth ) then nit = fix( width_smooth/2 )
		if keyword_set( fwhmg ) then nit = round( ((fwhmg+0.5)/2)^2 )
		return, smooth_iter( image, nit )
	   endif

	box_wid = 0
	if keyword_set( width_smooth ) then box_wid = width_smooth > 3
	if keyword_set( width_median ) then box_wid = (width_median > box_wid)>3

	if keyword_set( fwhmg ) then begin
		npix = ( 3 * fwhmg[ 0: ((N_elements( fwhmg )-1) < 1) ] ) > 3
		npix = 2 * fix( npix/2 ) + 1	 ;make # pixels odd.
		box_wid = box_wid > max( [npix] )
	   endif

	if (box_wid LT 3) then return, image

	if keyword_set( all_pixels ) then begin
		
		box_wid = fix( box_wid )
		radius = (box_wid/2) > 1
		Lxr = Lx+radius
		Lyr = Ly+radius
		rr = 2*radius
		imf = fltarr( sim[1]+rr, sim[2]+rr )
		imf[radius,radius] = image		; reflect edges outward
							; to make larger image.
		imf[  0,0] = rotate( imf[radius:rr,*], 5 )	;Left
		imf[Lxr,0] = rotate( imf[   Lx:Lxr,*], 5 )	;right
		imf[0,  0] = rotate( imf[*,radius:rr], 7 )	;bottom
		imf[0,Lyr] = rotate( imf[   *,Ly:Lyr], 7 )	;top

	  endif else begin

		radius=0
		imf = image
	   endelse

	if keyword_set( width_median ) then imf = median( imf, width_median>3 )
	if keyword_set( width_smooth ) then imf = smooth( imf, width_smooth>3 )

	if keyword_set( fwhmg ) then begin

		if N_elements( no_ft ) NE 1 then begin
                   if( box_wid gt 32 ) then no_ft = 0 else begin
                      sim = size( imf )
                      factor,sim[1],pfx,nfx
                      factor,sim[2],pfy,nfy
                      no_ft = max( [pfx,pfy] ) GT 13
                   endelse
                endif

                if N_elements( npix ) LT 2 then npix = [npix,npix]

		imf = convolve( imf, psf_gaussian( NP=npix,FWHM=fwhmg,/NORM ), NO_FT=no_ft )
	  endif

return, imf[ radius : Lx+radius, radius : Ly+radius ]
end
