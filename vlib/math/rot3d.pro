;+
; NAME:
;	Rot3D
; PURPOSE:
;	Rotate 3D coordinates, single or array of (x,y,z) coordinates.
;	Order of rotations is first around Y-axis, then Z-axis, then X-axis.
; CALLING:
;	xyz_rot = Rot3D( xyz )
; INPUTS:
;	xyz = [x,y,z] vector or and array of vectors,
;		in either [[x,y,z]] form or transposed [[x],[y],[z]] form. 
; KEYWORDS:
;	Xang =
;	Yang =
;	Zang =
;	/DEGREES : indicates that angles are in degrees (default is radians).
;	ROTMAT = optional output, the 3D rotation matrix (3 by 3).
; OUTPUTS:
;	Function returns rotated (x,y,z) array with same structure as input.
; PROCEDURE:
;	Compute Y-axis, Z-axis, and X-axis rotation matrices and multiply.
; HISTORY:
;	Written: Frank Varosi, U.MD.1988.
;-

function Rot3D ,xyz ,Xang=angX, Yang=angY, Zang=angZ, DEGREES=deg, ROTMAT=rotmat

	if N_elements( angX ) NE 1 then angX=0
	if N_elements( angY ) NE 1 then angY=0
	if N_elements( angZ ) NE 1 then angZ=0

	if keyword_set( deg ) then begin
		rady = angY*!DTOR
		radz = angZ*!DTOR
		radx = angX*!DTOR
	  endif else begin
		rady = angY
		radz = angZ
		radx = angX
	   endelse

	if (rady EQ 0) then begin
		ca = 1.
		sa = 0.
	  endif else begin
		ca = cos( rady )
		sa = sin( rady )
	   endelse

	rotY = [ [ ca, 0, -sa ] , $
		 [  0, 1,   0 ] , $
		 [ sa, 0,  ca ] ] 

	if (radz EQ 0) then begin
		ca = 1.
		sa = 0.
	  endif else begin
		ca = cos( radz )
		sa = sin( radz )
	   endelse

	rotZ = [ [  ca, sa, 0 ] , $
		 [ -sa, ca, 0 ] , $
		 [   0,  0, 1 ] ] 

	if (radx EQ 0) then begin
		ca = 1.
		sa = 0.
	  endif else begin
		ca = cos( radx )
		sa = sin( radx )
	   endelse

	rotX = [ [ 1,   0,  0 ] , $
		 [ 0,  ca, sa ] , $
		 [ 0, -sa, ca ] ] 

	rotmat = rotX # rotZ # rotY
	s = size( xyz )

	if (s(0) EQ 2) AND (s(2) EQ 3)  then return, xyz # transpose( rotmat ) $
					else return, rotmat # xyz
end
