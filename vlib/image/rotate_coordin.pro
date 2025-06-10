;+
; NAME:
;	rotate_coordin
; PURPOSE:
;	Apply one of the standard 8 image matrix rotations (0-7) of the
;	IDL function rotate( image, rotation ) to (x,y) pixel coordinate pair.
;	This is not the same as planar coordinate rotations,
;	since all rotations of matrix stay in first quadrant.
;
; CALLING EXAMPLES:
;	rotate_coordin, xin,yin, xout,yout, rotation, MAX=[64,48]
;	rotate_coordin, xin,yin, xout,yout, /INVERSE, ROT=rotation, /NORM
;	rotate_coordin, XY_IN=xyin, XY_OUT=xyout, ROT=rotation, /NORM
; INPUTS:
;	xin, yin = pixel coordinates in an image.
;	rotation = standard IDL rotation number, but negative means inverse.
; KEYWORDS:
;	ROTATION = another way to specify the rotation number (overrides arg.).
;	/INVERSE means do inverse of whatever <rotation> specifies.
;	/NORM means coordinates are 0 < (x,y) < 1, otherwise must specify MAX=.
;	MAX_COORDIN = [xmax,ymax], maximums of non-rotatated coordin, def=[1,1]
;
;	ZOOM_FACTOR = factor to apply to (xin,yin) -> zfac * ([xin,yin] - zoff)
;	ZOOM_OFFSET = offset to apply to (xin,yin), default zoff=0.
;
; Alternative Keyword Input/Output:
;
;	XY_IN = [xin,yin]
;	XY_OUT = [xout,yout]
;
; OUTPUTS:
;	xout, yout = pixel coordinates in rotated image.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1990.
;	F.V.1993, added alternative keywords XY_IN, XY_OUT.
;-

pro rotate_coordin, xin,yin, xout,yout, rotation, INVERSE=inv, NORMALIZE=norm, $
					MAX_COORDINATES=maxcoord, $
					ZOOM_FACTOR=zfac, ZOOM_OFFSET=zoff, $
					ROTATION_NUMBER=rotkey, $
					XY_IN=xyin, XY_OUT=xyout

	if N_elements( xyin ) EQ 2 then begin
		xin = xyin(0)
		yin = xyin(1)
	   endif

	if keyword_set( rotkey ) then rotation = rotkey
	if N_elements( rotation ) NE 1 then rotation=0

	if N_elements( zfac ) EQ 2 then begin

		if N_elements( zoff ) NE 2 then zoff=[0,0]

		if keyword_set( inv ) then begin

			rotate_coordin, xin,yin, xout,yout, rotation,/INVERSE,$
							NORM=norm,MAX=maxcoord
			xout = zoff(0) + xout/zfac(0)
			yout = zoff(1) + yout/zfac(1)
			xyout = [xout,yout]
			return

		  endif else begin

			rotate_coordin, zfac(0) * ( xin - zoff(0) ),$
					zfac(1) * ( yin - zoff(1) ),$
				xout, yout, rotation, NORM=norm, MAX=maxcoord
			xyout = [xout,yout]
			return
		   endelse
	   endif

	if keyword_set( inv ) then  RotNum = -rotation  $
				  else  RotNum = rotation

	if keyword_set( norm ) then begin
		xmax = 1.
		ymax = 1.	;all rotations in normalized coordinates
	  endif else begin
		if N_elements( maxcoord ) EQ 2 then begin
			xmax = maxcoord(0)
			ymax = maxcoord(1)
		  endif else begin
			message,"assuming MAX_COORDIN=[1,1] (normalized)",/INFO
			xmax = 1
			ymax = 1	;assuming normalized coordinates
		   endelse
	   endelse

	CASE RotNum OF
		0: BEGIN
			xout = xin
			yout = yin
		     END
		1: BEGIN
			xout = ymax - yin
			yout = xin
		     END
		2: BEGIN
			xout = xmax - xin
			yout = ymax - yin
		     END
		3: BEGIN
			xout = yin
			yout = xmax - xin
		     END
		4: BEGIN
			xout = yin
			yout = xin
		     END
		5: BEGIN
			xout = xmax - xin
			yout = yin
		     END
		6: BEGIN
			xout = ymax - yin
			yout = xmax - xin
		     END
		7: BEGIN
			xout = xin
			yout = ymax - yin
		     END
		(-1): BEGIN
				xout = yin
				yout = ymax - xin
			END
		(-2): BEGIN
				xout = xmax - xin
				yout = ymax - yin
			END
		(-3): BEGIN
				xout = xmax - yin
				yout = xin
			END
		(-4): BEGIN
				xout = yin
				yout = xin
			END
		(-5): BEGIN
				xout = xmax - xin
				yout = yin
			END
		(-6): BEGIN
				xout = xmax - yin
				yout = ymax - xin
			END
		(-7): BEGIN
				xout = xin
				yout = ymax - yin
			END
	     else: BEGIN
			xout = xin
			yout = yin
			message,"rotation = " + strtrim( RotNum, 2 ) + $
						" not implemented",/INFO
		     END
	ENDCASE

	xyout = [xout,yout]
end
