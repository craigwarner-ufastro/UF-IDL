;+
; NAME:
;	correl_images
; PURPOSE:
;	Computes the 2-D cross-correlation function of two images for
;	a range of (x,y) shifting of one image relative to the other.
;	Optimal (x,y)-offsets can then be found using pro CorrMat_Analyze.
; CALLING:
;		corrmat = correl_images( image_A, image_B )
; INPUTS:
;	image_A, image_B = the two images to be cross-correlated,
;			image_B is shifted relative to image_A.
; KEYWORDS:
;	XSHIFT = the + & - shift to be applied in X direction, default=7.
;	YSHIFT = the Y direction + & - shifting, default=7.
;	XOFFSET_B = initial X pixel offset of image_B relative to image_A.
;	YOFFSET_B = Y pixel offset, defaults are (0,0).
;	REDUCTION = optional reduction factor causes computation of
;		Low resolution correlation of bin averaged images,
;		thus faster. Can be used to get approximate optimal
;		(x,y) offset of images, and then called for successive
;		lower reductions in conjunction with pro CorrMat_Analyze
;		until REDUCTION=1, getting offset up to single pixel.
;	MAGNIFICATION = option causes computation of high resolution
;		correlation of magnified images, thus much slower.
;		Shifting distance is automatically = 2 + Magnification,
;		and optimal pixel offset should be known and specified.
;		Optimal offset can then be found to fractional pixels
;		using pro CorrMat_Analyze.
;	/NUMPIX	 causes the number of pixels for each correlation
;		to be saved in a second image, concatenated to the
;		correlation image, then function result is fltarr( Nx, Ny, 2 ).
;	/MONITOR causes the progress of computation to be briefly printed.
; OUTPUTS:
;	Result is the cross-correlation function, returned as a matrix.
; EXTERNAL CALLS:
;	function  round_off( number )	to round off fractions.
; PROCEDURE:
;	Loop over all possible (x,y) shifts, compute overlap and correlation
;	for each shift. Correlation set to zero when there is no overlap.
; HISTORY:
;	Written: Frank Varosi, NASA/GSFC, 1991.
;-

function correl_images, image_A, image_B, XSHIFT = x_shift, YSHIFT = y_shift, $
			XOFFSET_B = x_offset, YOFFSET_B = y_offset, $
			REDUCTION = reducf, MAGNIFICATION = Magf, $
			NUMPIX=numpix, MONITOR=monitor

  common correl_images, use_full_mean

	simA = size( image_A )
	simB = size( image_B )

	if (simA(0) LT 2) OR (simB(0) LT 2) then begin
		message,"first two arguments must be images",/INFO
		return,[-1]
	   endif

	if N_elements( x_offset ) NE 1 then x_offset=0
	if N_elements( y_offset ) NE 1 then y_offset=0

	if N_elements( x_shift ) NE 1 then x_shift = 7
	if N_elements( y_shift ) NE 1 then y_shift = 7
	x_shift = abs( x_shift )
	y_shift = abs( y_shift )

	if keyword_set( reducf ) then begin

		reducf = fix( reducf ) > 1
		if keyword_set( monitor ) then $
				print,"Reduction = ",strtrim( reducf, 2 )
		sA = simA/reducf
		LA = sA * reducf -1	;may have to drop edges of images.
		sB = simB/reducf
		LB = sB * reducf -1

		return, $
		correl_images( rebin( image_A(0:LA(1),0:LA(2)), sA(1), sA(2) ),$
			       rebin( image_B(0:LB(1),0:LB(2)), sB(1), sB(2) ),$
				XSHIFT = x_shift/reducf,	   $
				YSHIFT = y_shift/reducf,	   $
				XOFF=round_off( x_offset/reducf ), $
				YOFF=round_off( y_offset/reducf ), $
				MONITOR=monitor, NUMPIX=numpix     )

	  endif else if keyword_set( Magf ) then begin

		Magf = fix( Magf ) > 1
		if keyword_set( monitor ) then $
				print,"Magnification = ",strtrim( Magf, 2 )
		sA = simA*Magf
		sB = simB*Magf

		return, correl_images( rebin( image_A, sA(1), sA(2) ),$
				       rebin( image_B, sB(1), sB(2) ),$
					XS=2*Magf+1, YS=2*Magf+1,		 $
					XOFF=round_off( x_offset*Magf ), $
					YOFF=round_off( y_offset*Magf ), $
					MONITOR=monitor, NUMPIX=numpix   )
	   endif

	Nx = 2 * x_shift + 1
	Ny = 2 * y_shift + 1
	Nim = 1 + keyword_set( numpix )

	correl_mat = fltarr( Nx, Ny, Nim )

	xs = round_off( x_offset ) - x_shift
	ys = round_off( y_offset ) - y_shift

	sAx = simA(1)-1
	sAy = simA(2)-1
	sBx = simB(1)-1
	sBy = simB(2)-1

	fmean_A = total( image_A )/N_elements( image_A )
	fmean_B = total( image_B )/N_elements( image_B )

	for y = 0, Ny-1 do begin	;compute correlation for each y,x shift.

	    yoff = ys + y
	    yAmin = yoff > 0
	    yAmax = sAy < (sBy + yoff)
	    yBmin = (-yoff) > 0
	    yBmax = sBy < (sAy - yoff)		;Y overlap

	    if (yAmax GT yAmin) then begin

	       for x = 0, Nx-1 do begin

		   xoff = xs + x
		   xAmin = xoff > 0
		   xAmax = sAx < (sBx + xoff)
		   xBmin = (-xoff) > 0
		   xBmax = sBx < (sAx - xoff)		;X overlap

		   if (xAmax GT xAmin) then begin

			im_ov_A = image_A( xAmin:xAmax, yAmin:yAmax )
			im_ov_B = image_B( xBmin:xBmax, yBmin:yBmax )
			Npix = N_elements( im_ov_A )

			if N_elements( im_ov_B ) NE Npix then begin
				message,"overlap error: # pixels NE",/INFO
				print, Npix, N_elements( im_ov_B )
				if !DEBUG then stop
			   endif

			if keyword_set( use_full_mean ) then begin
				mean_A = fmean_A
				mean_B = fmean_B
			 endif else begin
				mean_A = total( im_ov_A )/Npix
				mean_B = total( im_ov_B )/Npix
			  endelse

			im_ov_A = im_ov_A - mean_A
			im_ov_B = im_ov_B - mean_B
			totAA = total( im_ov_A * im_ov_A ) > 1e-19
			totBB = total( im_ov_B * im_ov_B ) > 1e-19

			correl_mat(x,y) = total( im_ov_A * im_ov_B ) / $
							sqrt( totAA * totBB )

			if keyword_set( numpix ) then correl_mat(x,y,1) = Npix
		     endif

	          endfor
		endif

		if keyword_set( monitor ) then print, Ny-y, FORM="($,i3)"
	  endfor

	if keyword_set( monitor ) then print," "

return, correl_mat
end
