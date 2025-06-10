;+
; NAME:
;	Moffat
; PURPOSE:
;	Evaluate the Moffat function and partial derivatives, in 1-D or 2-D.
; CALLING EXAMPLES:
;	mf = Moffat( xin, parms )
;	mf = Moffat( xin, parms, pderiv )
; INPUTS:
;	xin = independent variable, 1-D vector or 2-D array [ nx, ny, 2 ]
;
;	parms = array, Moffat function parameters (4):
;		parms[0] = maximum value (factor),
;		parms[1] = center pixel,
;		parms[2] = alpha,
;		parms[3] = beta (power).
; RETURN:
;	Moffat function evaluated at (xin).
; OUTPUT (optional):
;	pderiv = matrix of partial derivatives respect to parameters,
;		always array [ npoints, nparams ]
; PROCEDURE:
;	Handle Cases of 1-D and 2-D seperately.
; HISTORY:
;	Written: Frank Varosi UFastro 2007.
;       F.V. @ UFAstro. 2017: fixed mistake in 1-D case: result is now multiplied by factor.
;-

function Moffat, xin, parameters, pderiv

	szx = size( xin )
	if( szx[0] gt 1 ) then ndim = szx[szx[0]]  else ndim = 1
	szp = size( parameters )

	if (szp[0] EQ 2) AND (szp[2] GE 4) then begin
		parms = double( transpose( parameters ) )
		szp = size( parms )
	  endif else parms = double( parameters )

	Nparm = szp[1]
	pdim = szp[0]

	if (Nparm LT 4) then begin
		message,"need at least 4 parameters",/INFO
		return,(-1)
	   endif

CASE ndim OF

1: BEGIN
	factor = parms[0]
	cntrd = parms[1]
	alpha = parms[2]
	beta = parms[3]

	xm = xin - cntrd
	xda = xm/alpha
	xda2 = xda * xda
	Af = 1 + xda2
	Mf = Af^(-beta)
	Mff = factor * Mf

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( xda ), Nparm )
		pderiv[*,0] = Mf
		pdf = 2 * beta * ( Mff / Af )
		pderiv[*,1] = pdf * ( xda / alpha )
		pderiv[*,2] = pdf * ( xda2 / alpha )
		pderiv[*,3] = -aLog( Af ) * Mff
		if (Nparm GE 5) then  pderiv[*,4] = 1
	   endif

	if (Nparm GE 5) then return, Mff + parms[4] $
			else return, Mff
     END

2: BEGIN
	if( pdim gt 1 ) then begin
		factor = parms[0,0]
		cntx = parms[1,0]
		cnty = parms[1,1]
		alphax = parms[2,0]
		alphay = parms[2,1]
		beta = parms[3,0]
		if (Nparm GE 5) then begin
			offset = parms[4,0]
			Nparm = 7
		 endif else begin
			offset= 0
			Nparm = 6
		  endelse
	 endif else begin
		factor = parms[0]
		cntx = parms[1]
		cnty = parms[2]
		alphax = parms[3]
		alphay = parms[4]
		beta = parms[5]
		if (Nparm GT 6) then offset = parms[6] else offset=0
	  endelse

	xm = xin[*,*,0] - cntx
	ym = xin[*,*,1] - cnty
	xda = xm/alphax
	yda = ym/alphay
	xda2 = xda * xda
	yda2 = yda * yda
	Af = 1 + xda2 + yda2
	Mf = Af^(-beta)
	Mff = factor * Mf

	if N_params() GE 3 then begin
		Npts = N_elements( xm )
		pderiv = dblarr( Npts, Nparm )
		pderiv[*,0] = reform( Mf, Npts )
		Mf = reform( Mff, Npts )
		Af = reform( Af, Npts )
		pdf = 2 * beta * ( Mf / Af )
		pderiv[*,1] = pdf * ( xda / alphax )
		pderiv[*,2] = pdf * ( yda / alphay )
		pderiv[*,3] = pdf * ( xda2 / alphax )
		pderiv[*,4] = pdf * ( yda2 / alphay )
		pderiv[*,5] = -aLog( Af ) * Mf
		if (Nparm GT 6) then  pderiv[*,6] = 1
	   endif

	if( offset ne 0 ) then return, Mff + offset $
			else return, Mff
     END

else: BEGIN

	help, xin
	message,"Unsupported dimensionality of independant variable: xin"
	return,0
     END

ENDCASE
end
