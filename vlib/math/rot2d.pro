;+
; NAME:
;	Rot2D
; PURPOSE:
;	Rotate 2D coordinates, single or array of (x,y) coordinates.
; CALLING:
;	xy_rot = Rot2D( xy, angle )
; INPUTS:
;	xy = [x,y] vector or and array of vectors,
;		in either [[x,y]] form or transposed [[x],[y]] form.
;	angle = the desired rotation angle in radians, unless /DEGREES.
; KEYWORDS:
;	/DEGREES : indicates that angles are in degrees (default is radians).
;	ROTMAT = optional output, the 2D rotation matrix (2 by 2).
; OUTPUTS:
;	Function returns rotated (x,y) array with same structure as input.
; HISTORY:
;	Written: Frank Varosi, U.MD.1988.
;-

function Rot2d, xy, angle, DEGREES=degrees, ROTMAT=rotmat

	if keyword_set( degrees ) then  arad = angle*!DTOR  else  arad=angle
	ca = cos( arad )
	sa = sin( arad ) 

	rotmat = [ [  ca , sa ] , $
		   [ -sa , ca ] ] 

	s = size( xy )

	if (s(0) EQ 2) AND (s(2) EQ 2)  then return, xy # transpose( rotmat ) $
					else return, rotmat # xy
end
