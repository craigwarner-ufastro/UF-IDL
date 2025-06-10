;+
; NAME:
;	rfftinv
;
; PURPOSE:
;	Compute real FFT inverse of 2-D spectrum (coefficients of modes),
;	where only half of the frequencies (modes, wavenumbers) are required
;	since the other half is always the rotated and shifted conjugate.
;	Assumes that the original image (from which spectrum was derived) has
;	size in each dimension which is an even number (better for FFT anyway).
; CALLING:
;	image = rfftinv( hmodes, [fmodes] )
;
; INPUT:
;	hmodes = half of the 2-D spectrum:
;		specifically, if nfx, nfy are the Nyquist frequencies
;		then hmodes should be an array of size = ( 2*nfx, nfy+1 ).
;		For example, if size( image ) = (nx,ny) and f = FFT( image, -1 )
;		then hmodes = f(*,0:ny/2) so nfx = nx/2 and nfy = ny/2.
;
; KEYWORDS:
;	/CENTERED : assume that the hmodes matrix is shifted in first
;		dimension (edge to center) so that the coefficient of
;		frequency (0,0) is at array location (nfx,0).
;		Default is that frequency (0,0) is at array location (0,0),
;		the normal result of FFT.
;
;	/FORCE_REAL : apply conjugate rules to the input matrix hmodes
;		in order to insure that the inverse FFT result is real valued.
;		May change some values of the input spectrum, if necessary,
;		along the edges of the matrix.
;
; OUTPUTS:
;	fmodes = optional output of full 2-D spectrum, size = ( 2*nfx, 2*nfy ).
;
;	Function returns the inverse FT of spectrum giving a 2-D image
;	of size = ( 2*nfx, 2*nfy ).
;
; PROCEDURE:
;	Take the conjugate of input spectrum, shift, rotate and concatenate
;	with itself to obey conjugate rules for a real function.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1993.
;-

function rfftinv, hmodes, fmodes, FORCE_REAL=force_real, CENTERED=centered

	sm = size( hmodes )
	nx = sm(1)/2
	ny = sm(2)-1

	if keyword_set( force_real ) then begin
		hmodes(0,0) = float( hmodes(0,0) )
		hmodes(nx,0) = float( hmodes(nx,0) )
		hmodes(0,ny) = float( hmodes(0,ny) )
		hmodes(nx,ny) = float( hmodes(nx,ny) )
	   endif

	if keyword_set( centered ) then begin

		if keyword_set( force_real ) then begin
			hmodes(1,0) = rotate( conj( hmodes(nx+1:*,0) ), 2 )
			hmodes(1,ny) = rotate( conj( hmodes(nx+1:*,ny) ), 2 )
		   endif

		fmodes = [ [shift( hmodes, nx,0 )], $
			[rotate( shift( conj(hmodes(*,1:ny-1)), nx-1,0 ), 2 )] ]

	 endif else begin

		if keyword_set( force_real ) then begin
			hmodes(nx+1,0) = rotate( conj( hmodes(1:nx-1,0) ), 2 )
			hmodes(nx+1,ny) = rotate( conj( hmodes(1:nx-1,ny) ), 2 )
		   endif

		fmodes = [ [ hmodes ], $
			[rotate( shift( conj(hmodes(*,1:ny-1)), -1,0 ), 2 )] ]
	  endelse

return, float( fft( fmodes, 1 ) )
end
