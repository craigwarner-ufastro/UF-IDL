;+
; NAME:
;	Show_Volume
; PURPOSE:
;	Return a shaded image of an iso-surface of 3D volume data (voxels).
;	Assumes that a window of desired image size is already open.
;
; CALLING EXAMPLES:
;
;	image = show_volume( Voxels, THRESH=0.7, XANG=30, ZANG=40 )
;
;	tv, show_volume( Voxels, THRESH=0.5, /LOW )
;
; INPUTS:
;	Voxels = 3D array, data values on equally spaced (x,y,z) grid.
;
; KEYWORD INPUTS:
;
;	THRESHOLD = value of iso-surface (default = average of data).
;	XANGLE = degrees rotation about X-axis.
;	ZANGLE = degrees rotation about Z-axis.
;	/LOW : show and shade the Low side of surface (default is high side).
;
; KEYWORD OUTPUTS/INPUTS:
;
;	VERTICES = vertices returned by Shade_Volume, for reuse.
;	POLYGONS = polygon info returned by Shade_Volume, for reuse.
;		If vertices and polygons are input via the keywords
;		then the call to Shade_Volume is skipped.
;	SIZE = [3,nx,ny,nz] : alternate specification of 3D array dimensions
;		if vertices and polygons are input instead of Voxels.
;
; OUTPUTS:
;	Function returns a byte image to be displayed using TV.
;
; EXTERNAL CALLS:
;	pro scale3
; PROCEDURE:
;	Call IDL intrinsic procedure Shade_Volume to get the iso-surface,
;	call pro scale3 to set the 3D view transformation, then
;	call IDL intrinsic function PolyShade for projected image.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;-

function Show_Volume, Voxels, THRESHOLD=thresh, LOW=Low, SIZE=sizev, $
		XANGLE=angx, ZANGLE=angz, VERTICES=verts, POLYGONS=polys

	sv = size( Voxels ) 

	if (N_elements( verts ) LE 0) OR (N_elements( polys ) LE 0) then begin

		if sv(0) NE 3 then begin
			print,"syntax:	Show_Volume, Voxels, THRESHOLD= , $"
			print,"			LOW= , SIZE= , $"
			print,"			XANGLE= , ZANGLE= , $"
			print,"			VERTICES= , POLYGONS="
			return,sv
		   endif

		; produce vertices and polygons of iso-surface:

		if N_elements( Low ) NE 1 then Low=0
		if N_elements( thresh ) NE 1 then $
			thresh = total(Voxels)/N_elements(Voxels)

		Shade_Volume, Voxels, thresh, verts, polys, LOW=Low
	  endif

	if N_elements( sizev ) GE 4 then sv = sizev

	if N_elements( sv ) LT 4 then begin
		message,"need to specify SIZE=[3,nx,ny,nz]",/INFO
		return,0
	  endif

; create 2D shaded projection of 3D iso-surface:

 	scale3, XR=[0,sv(1)], YR=[0,sv(2)], ZR=[0,sv(3)], AX=angx, AZ=angz

return, PolyShade( verts, polys,/T3D )
END
