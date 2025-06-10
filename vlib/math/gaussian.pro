;+
; NAME:
;	gaussian
;
; PURPOSE:
;	Compute the Gaussian function, and partial derivatives, in 1-D or 2-D.
;
; CALLING:
;	f = gaussian( xin, parms )
;	f = gaussian( xin, parms, pderiv )
;
; INPUTS:
;	xin = independent variable, 1-D vector or 2-D array [ nx, ny, 2 ]
;
;	parms = parameters of Gaussian, 2 or 3 element array:
;		parms[0] = maximum value (factor) of Gaussian,
;		parms[1] = mean value (center) of Gaussian,
;		parms[2] = standard deviation (sigma) of Gaussian.
;		parms[3] = optional, constant offset added to Gaussian.
;		(if parms has only 2 elements then sigma taken from common).
;
; OUTPUT:
;	pderiv = optional output of partial derivatives,
;		computed only if parameter is present in call,
;		always array [ npoints, nparams ]
;
;		pderiv[*,i] = partial derivative at all xin absisca values
;		with respect to parms[i], i=0,1,2.
;
;	Function returns array of Gaussian evaluated at xin.
;
; COMMON BLOCKS:
;	common gaussian, sigma
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;	F.V. 1994, added optional fourth parameter = constant offset.
;	F.V. 2007 @ UF-astro: added 2-dimensional code option.
;-

function gaussian, xin, parameters, pderiv

  common gaussian, sigma

	szx = size( xin )
	if( szx[0] gt 1 ) then ndim = szx[szx[0]]  else ndim = 1
	szp = size( parameters )

	if (szp[0] EQ 2) AND (szp[2] GE 3) then begin
		parms = double( transpose( parameters ) )
		szp = size( parms )
	  endif else parms = double( parameters )

	Nparm = szp[1]
	pdim = szp[0]

	if (Nparm LT 2) then begin
		message,"need at least 2 parameters",/INFO
		return,(-1)
	   endif

CASE ndim OF

1: BEGIN
	Nparmg = N_elements( parms )
	parms = double( parms )
	if (Nparmg GE 3) then sigma = parms[2]

	z = ( xin - parms[1] )/sigma
	zz = z*z
	gausf = fltarr( N_elements( zz ) )
	w = where( zz LT 172, nw )
	if (nw GT 0) then gausf[w] = exp( -zz[w] / 2 )

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( xin ), Nparmg )
		fsig = parms[0] / sigma

		pderiv[0,0] = gausf
		pderiv[0,1] = gausf * z * fsig

		if (Nparmg GE 3) then  pderiv[0,2] = gausf * zz * fsig
		if (Nparmg GE 4) then  pderiv[0,3] = 1
	   endif

	if (Nparmg GE 4) then return, parms[0] * gausf + parms(3) $
			else return, parms[0] * gausf
     END

2: BEGIN
	if( pdim gt 1 ) then begin
		factor = parms[0,0]
		cntx = parms[1,0]
		cnty = parms[1,1]
		sigx = parms[2,0]
		sigy = parms[2,1]
		if (Nparm GE 4) then begin
			offset = parms[3,0]
			Nparm = 6
		 endif else begin
			offset= 0
			Nparm = 5
		  endelse
	 endif else begin
		factor = parms[0]
		cntx = parms[1]
		cnty = parms[2]
		sigx = parms[3]
		sigy = parms[4]
		if (Nparm GT 5) then offset = parms[5] else offset=0
	  endelse

	xm = ( xin[*,*,0] - cntx )/sigx
	ym = ( xin[*,*,1] - cnty )/sigy
	zz = xm*xm + ym*ym
	szz = size( zz )
	gausf = fltarr( szz[1], szz[2] )
	w = where( zz LT 172, nw )
	if (nw GT 0) then gausf[w] = exp( -zz[w] / 2 )

	if N_params() GE 3 then begin

		np = N_elements( gausf )
		pderiv = fltarr( np, Nparm )
		fsigx = factor / sigx
		fsigy = factor / sigy

		pderiv[*,0] = reform( gausf, np ) 
		pderiv[*,1] = pderiv[*,0] * xm * fsigx 
		pderiv[*,2] = pderiv[*,0] * ym * fsigy 
		pderiv[*,3] = pderiv[*,1] * xm /sigx 
		pderiv[*,4] = pderiv[*,2] * ym /sigy 

		if (Nparm gt 5) then  pderiv[*,5] = 1
	   endif
 
	gausf = factor * gausf
	if (Nparm gt 5) then gausf = gausf + offset
	return, gausf
     END

else: BEGIN

	help, xin
	message,"Unsupported dimensionality of independant variable: xin"
	return,0
     END

ENDCASE
end
