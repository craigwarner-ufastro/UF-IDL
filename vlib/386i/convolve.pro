function convolve, image, psf, FT_PSF=psf_FT, FT_IMAGE=imFT, NO_FT=noft, $
				CORRELATE=correlate, AUTO_CORRELATION=auto
;+
;PURPOSE:
;	Convolution of image with Point Spread Function (PSF),
;	default is to compute using product of Fourier transforms.
;CALLING:
;	imconv = convolve( image1, psf, FT_PSF = psf_FT )
;  or:	correl = convolve( image1, image2, /CORREL )
;  or:	correl = convolve( image, /AUTO )
;INPUTS:
;	image = the image to be convolved with psf
;	psf = the Point Spread Function, (size < or = to size of image).
;KEYWORDS:
;	FT_PSF = passes the Fourier transform of PSF so that it can be re-used.
;	FT_IMAGE = passes in/out the Fourier transform of image.
;      /CORRELATE uses the conjugate of the Fourier transform of PSF,
;			to compute the cross-correlation of image and PSF,
;		(equivalent to IDL function convol() with NO rotation of PSF)
;      /AUTO_CORR computes the auto-correlation function of image using FFT.
;      /NO_FT overrides the use of FFT, using the IDL function convol() instead.
;		(then PSF is rotated by 180 degrees to give equivalent result)
;METHOD:  When using FFT, PSF is centered & expanded to size of image.
;HISTORY:	written by Frank Varosi at NASA/GSFC 1992.
;-
	sp = size( psf_FT )  &  sif = size( imFT )
	sim = size( image )  &  sc = sim/2  &  npix = N_elements( image )

	if (sim(0) NE 2) OR keyword_set( noft ) then begin
		if keyword_set( auto ) then begin
			message,"auto-correlation only for images with FFT",/INF
			return, image
		  endif else if keyword_set( correlate ) then $
				return, convol( image, psf ) $
			else	return, convol( image, rotate( psf, 2 ) )
	   endif

	if (sif(0) NE 2) OR (sif(sif(0)+1) NE 6) OR $
	   (sif(1) NE sim(1)) OR (sif(2) NE sim(2)) then imFT = FFT( image,-1 )

	if keyword_set( auto ) then $
	 return, shift( npix*float( FFT( imFT*conj( imFT ),1 ) ), sc(1),sc(2) )

	if (sp(0) NE 2) OR (sp(sp(0)+1) NE 6) OR $
	   (sp(1) NE sim(1)) OR (sp(2) NE sim(2)) then begin
		sp = size( psf )
		if (sp(0) NE 2) then begin
			message,"must supply PSF matrix (2nd arg.)",/INFO
			return, image
		   endif
		Loc = ( sc - sp/2 ) > 0		;center PSF in new array,
		s = (sp/2 - sc) > 0	   ;handle all cases: smaller or bigger
		L = (s + sim-1) < (sp-1)
		psf_FT = fltarr( sim(1), sim(2) )
		psf_FT( Loc(1), Loc(2) ) = psf( s(1):L(1), s(2):L(2) )
		psf_FT = FFT( psf_FT, -1 )
	   endif

	if keyword_set( correlate ) then $
		conv = npix * float( FFT( imFT * conj( psf_FT ), 1 ) ) $
	  else	conv = npix * float( FFT( imFT * psf_FT, 1 ) )

return, shift( conv, sc(1), sc(2) )
end
