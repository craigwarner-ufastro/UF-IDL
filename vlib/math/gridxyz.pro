;+
; NAME:
;	gridxyz
; PURPOSE:
;	Create a 3-D grid of X & Y coordinates, suitable for computations,
;	in the form of 3 data-cubes, stacked as 4-D array,
;	or 3 vectors (collapsed cubes, 2-D array result).
; CALLING:
;	gxyz = gridxyz( Ngx, Ngy, Ngz, XRAN=[-1,1], YRAN=[-1,1] )
; INPUTS:
;	Ngx, Ngy, Ngz = number of X, Y, & Z grid points, resp.
; KEYWORDS:
;	XRANGE, YRANGE = 2 element arrays specifying [min,max] of X & Y range.
;		Note that the ranges are saved in common MapLim, so that if
;		next call does not specify them, the common values are used.
;		Default values are [0,Ngx] and [0,Ngy].
;	DIMENSIONS = 3 elements array, alternate specification of [Ngx,Ngy,Ngz].
;	/INCLUDE_MAXRAN causes the last coordinate in output array to be
;			the maximum of X & Y range (default is max - grid_size).
;	SHIFT = fraction of grid size to shift points so as to stay within
;			specified range, default is start at minimum of range.
;
;	/VECTOR : collapse 3-D to 1-D and return 2-D array : ( Ngx*Ngy*Ngz, 3 ).
;	POWER = scalar, optional exponent used to raise elements to a power
;		before creating matrix, thus faster than afterwards.
;		Power is not applied to zero values, they are kept zero.
; OUTPUTS:
;	Result is an array of X & Y coordinate points:
;	4-D array = fltarr( Ngx, Ngy, Ngz, 3 )
;	or 2-D array = fltarr( Ngx*Ngy*Ngz, 3 )  if /VECTOR set.
; COMMON BLOCKS:
;	common MapLim ,xminc,xmaxc ,yminc,ymaxc		;can specify limits.
;	common MapLim2 ,zminc,zmaxc
; EXTERNAL CALLS:
;	function gridxy
; PROCEDURE:
;	Call func gridxy for 2 coordinates the matrix outer product (#).
; HISTORY:
;	Written: Frank Varosi U.MD. 1988.
;	F.V. 1990, updated for IDL version-2, added keywords.
;-

function gridxyz, Ngx,Ngy,Ngz, DIMENSIONS=dimen, VECTOR=vector, POWER=power, $
			INCLUDE_MAXRAN=include_maxran, SHIFT_FRACTION=shift, $
				XRANGE=xran, YRANGE=yran, ZRANGE=zran

  common MapLim2 ,zminc,zmaxc

	if N_elements( dimen ) EQ 3 then begin
		Ngx = dimen(0)
		Ngy = dimen(1)
		Ngz = dimen(2)
	   endif

	if N_elements( zran ) EQ 2 then begin
		zminc = zran(0)
		zmaxc = zran(1)
	  endif else if (N_elements( zminc ) EQ 1) AND $
			(N_elements( zmaxc ) EQ 1) then zran = [zminc,zmaxc]

	if N_elements( zran ) NE 2 then zran = [0,Ngz]
	zgs = float( zran(1) - zran(0) )/Ngz
	if keyword_set( include_maxran ) then zgs = zgs * float( Ngz )/(Ngz - 1)

	Gz = zgs * findgen( Ngz ) + zran(0)
	if keyword_set( shift ) then Gz = Gz + shift * zgs

	if keyword_set( power ) then begin
		w = where( Gz NE 0, nw )
		if (nw GT 0) then Gz(w) = Gz(w)^power
	   endif

	g2d = gridxy( Ngx, Ngy, XRAN=xran, YRAN=yran, VEC=vector, POW=power, $
				 INCLUDE_MAX=include_maxran, SHIFT_FRAC=shift)

	if keyword_set( vector ) then begin

		Ngxy = Ngx * Long( Ngy )
		grid = fltarr( Ngxy * Ngz , 3 )

		for k=0,Ngz-1 do begin
			Loc= k*Ngxy
			grid(Loc,0) = g2d(*,0)
			grid(Loc,1) = g2d(*,1)
			grid(Loc,2) = replicate( Gz(k), Ngxy )
		  endfor

	  endif else begin

		grid = fltarr( Ngx, Ngy, Ngz, 3 )

		for k=0,Ngz-1 do begin
			grid(*,*,k,0) = g2d(*,*,0)
			grid(*,*,k,1) = g2d(*,*,1)
			grid(*,*,k,2) = replicate( Gz(k), Ngx, Ngy )
		  endfor
	   endelse

return, grid
end
