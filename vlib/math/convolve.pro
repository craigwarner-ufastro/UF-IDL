;+
; NAME:
;	CONVOLVE
; PURPOSE:
;	Convolution of an image with a Point Spread Function (PSF),
;	or correlation two images, or autocorrelation of an image.
;	Default is to compute using product of Fourier transforms.
;
; CALLING SEQUENCE:
;
;	imconv = convolve( image, psf )
;  or:
;	imconv = convolve( image, psf, FT_PSF=psf_FT )
;  then:
;	imconv = convolve( image, FT_PSF=psf_FT )
;  or:
;	correl = convolve( image1, image2, /CORREL )
;	correl = convolve( image1, image2, /CORREL,/NORM )
;  or:
;	correl = convolve( image, /AUTO )
;
; INPUTS:
;	image = 2-D array (matrix) to be convolved with PSF.
;	psf = the Point Spread Function, with size < or = to size of image.
;
; KEYWORDS:
;
;	FT_PSF = passes out/in the Fourier transform of the PSF,
;		(so that it can be re-used the next time function is called).
;
;	FT_IMAGE = passes out/in the Fourier transform of image.
;
;	/CORRELATE uses the conjugate of the Fourier transform of PSF,
;		to compute the cross-correlation of image and PSF,
;		(equivalent to IDL function convol() with NO rotation of PSF).
;
;	/NORMALIZE for the /CORREL option scales result to range 0 to 1.
;		(both image and psf args must be input for this option)
;
;	/AUTO_CORR computes the auto-correlation function of image using FFT.
;
;	/NO_FT overrides the use of FFT, using IDL function convol() instead.
;		(then PSF is rotated by 180 degrees to give same result)
; METHOD:
;	When using FFT, PSF is centered & expanded to size of image.
; HISTORY:
;	written, Frank Varosi, NASA/GSFC 1992.
;	F.V.1993, added one to shift for case of odd size of image.
;	F.V.1997, change in 93 was not exactly correct, instead:
;		when PSF size is less than image size, fixed placement
;		of PSF relative to image, for case of odd size of image.
;	F.V.2021, UFastro: added keyword option /NORMALIZE for the case /CORREL.
;-

function convolve, image, psf, FT_PSF=psf_FT, FT_IMAGE=imFT, NO_FT=noft, $
                   CORRELATE=correlate, AUTO_CORRELATION=auto, NORMALIZE=normalize

  sim = size( image )
  sp = size( psf )

  if keyword_set( correlate ) and keyword_set( normalize ) then begin
     if (sim[0] eq 2) and (sp[0] eq 2 ) then begin
        normfac = 1 / sqrt( total( image^2 ) * total( psf^2 ))
        return, normfac * convolve( image, psf,/CORR )
     endif
  endif

	spf = size( psf_FT )
	if (spf[0] ne 2) or (spf(spf[0]+1) ne 6) or (spf(spf[0]+1) ne 9) or $
	   (spf[1] ne sim[1]) or (spf[2] ne sim[2]) then no_psf_FT = 1

	if (sim[0] ne 2) or ((sp[0] ne 2) and $
	    keyword_set( no_psf_FT ) and (NOT keyword_set( auto ))) then begin
		print,"syntax:	result = convolve( image, psf )"
		print,"    or:	result = convolve( image, psf, FT_PSF=psf_FT )"
		print,"    or:	correl = convolve( image1, image2, /CORREL )"
		print,"    or:	autocorr = convolve( image, /AUTO )"
		return,0
	   endif

	if keyword_set( noft ) then begin
		if keyword_set( auto ) then begin
			message,"auto-correlation available only with FFT",/INFO
			return, image
		  endif else if keyword_set( correlate ) then $
				return, convol( image, psf ) $
			else	return, convol( image, rotate( psf, 2 ) )
	   endif

	sc = sim/2
	npix = N_elements( image )
	sif = size( imFT )

	if (sif[0] ne 2) or (sif(sif[0]+1) ne 6) or (sif(sif[0]+1) ne 9) or $
	   (sif[1] ne sim[1]) or (sif[2] ne sim[2]) then imFT = FFT( image,-1 )

	if keyword_set( auto ) then $
           return, shift( npix*float( FFT( imFT*conj( imFT ), 1 ) ), sc[1],sc[2] )

	if Keyword_Set( no_psf_FT ) then begin
		s2 = sc + (sim MOD 2)*(sp LT sim)   ;correct for odd size image.
		Loc = (s2 - sp/2) > 0		    ;center PSF in new array,
		s = (sp/2 - s2) > 0	            ;handle all cases: smaller or bigger
		L = (s + sim-1) < (sp-1)
		psf_FT = complexarr( sim[1], sim[2] )
		psf_FT( Loc[1], Loc[2] ) = psf( s[1]:L[1], s[2]:L[2] )
		psf_FT = FFT( psf_FT, -1, /OVERWRITE )
	   endif

	if keyword_set( correlate ) then $
		conv = npix * float( FFT( imFT * conj( psf_FT ), 1 ) ) $
	  else	conv = npix * float( FFT( imFT * psf_FT, 1 ) )

return, shift( conv, sc[1], sc[2] )
end
