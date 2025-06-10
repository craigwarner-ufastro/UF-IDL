;+
; NAME:
;	RotMag
;
; PURPOSE:
;	Rotate, magnify or demagnify, and/or translate an image.
;	( Combines the effect of ROT and CONGRID ).
;
; CATEGORY:
;	Z3 - Image processing, geometric transforms.
;
; CALLING:
;	Result = RotMag( image, ANGLE=Angle, MAG=Mag, /INTERP, MISSING=missval )
;
; INPUTS:
;	image:	The image array to be rotated.  This array may be of any type,
;		but it must have two dimensions.
;
; KEYWORDS:
;
;	ANGLE = Angle of rotation in degrees Counter-Clockwise.
;
;	MAG = Magnification/demagnification factor.  A value of 1.0 = no
;		change, > 1 is magnification and < 1 is demagnification.
;		Size of returned (output) image is increased/decrease approp.
;
;	XC, YC = X or Y subscripts for the center of rotation.  If omitted,
;		default is the size of the image divided by 2.
;
;	INTERP:	Set this keyword for bilinear interpolation.  If this keyword
;		is set to 0 or omitted, nearest neighbor sampling is used,
;		set to 1 then bilinear interpolation is used,
;		if INTERP = 2 then cubic interpolation (IDL v.3.5) is used.
;
;	MISSING = The data value to substitute for pixels in the output image 
;		that map outside the input image.
;
;	PIVOT: Setting this keyword causes the image to pivot around the point
;		XC, YC, so that this point maps into the same point in the
;		output image.  If this keyword is set to 0 or omitted, then the
;		point XC, YC in the input image is mapped into the center of
;		the output image.
;
; OUTPUTS:
;	RotMag returns a rotated, magnified, and/or translated version of the
;	input image.  The dimensions of the output image equal
;	the magnification factor times dimensions of the input image.
;
; PROCEDURE:
;	The POLY_2D function is used to translate, scale, and rotate the image.
;
; EXAMPLE:
;	Create and display an image.  Then display a rotated and magnified
;	version.  Create and display the image by entering:
;
;		A = BYTSCL(DIST(256))
;		TV, A
;
;	Rotate the image 33 degrees and magnify it 1.5 times.  Use bilinear
;	interpolation to make the image look nice.  Enter:
;
;		B = RotMag( A, 33, 1.5, /INTERP )
;		TV, B
;	
; MODIFICATION HISTORY:
;	June, 1982. 	Written by DMS, RSI.
;
;	Feb, 1986. 	Modified by Mike Snyder, ES&T Labs, 3M Company.
;	 		Adjusted things so that rotation is exactly on the 
;			designated center.
;
;	October, 1986.  Modified by DMS to use the POLY_2D function.
;
;	Aug, 1988.	Added INTERP keyword.
;       Dec, 1992.      Added PIVOT keyword, William Thompson, NASA/GSFC.
;	Apr, 1994, F.Varosi, GSFC, copied ROT and mod to magnify size of image.
;-

function RotMag, image, ANGLE = angle, MAGNIFICATION = mag, XC=x0, YC=y0, $
			INTERP = interp, MISSING = missing, PIVOT = pivot 

	sz = float( size( image ) )	;Get dimensions
	if N_elements( interp ) NE 1 then interp = 0

;Center of rotation in X and Y:

	if N_elements( x0 ) NE 1 then X0 = (sz[1]-1)/2.
	if N_elements( y0 ) NE 1 then Y0 = (sz[2]-1)/2.
	if N_elements( Mag ) NE 1 then Mag = 1
	sz = round( sz * Mag )

	IF KEYWORD_SET( PIVOT ) THEN BEGIN
		XC = X0 * Mag
		YC = Y0 * Mag
	 ENDIF ELSE BEGIN
		xc = (sz[1]-1)/2.        ;center of output image.
		yc = (sz[2]-1)/2.
	  ENDELSE

	if keyword_set( angle ) then begin
		theta = angle/!RADEG		;angle in degrees.
		c = cos( theta ) * Mag
		s = sin( theta ) * Mag
	  endif else begin
		if (Mag EQ 1) then return, image
		c = Mag
		s = 0
	   endelse

	kx = -xc + c*x0 - s*y0		;useful constants.
	ky = -yc + s*x0 + c*y0
	cc = c * c
	kk = 1./(1.+s^2/cc)

	cx = kk * [  ky*s/cc + kx/c, s/cc,    1/c, 0. ]		;x coeff...
	cy = kk * [ -kx*s/cc + ky/c,  1/c,  -s/cc, 0. ]		;y coeff.

	if N_elements( missing ) NE 1 then $
			 return, poly_2d( image, cx,cy, interp, sz[1], sz[2] ) $
	   else $
	   return, poly_2d( image, cx,cy, interp, sz[1], sz[2], MISS=missing )
END
