;+
; NAME:
;	Lorentzian
; PURPOSE:
;	Evaluate the generalized (modified) Lorentzian function,
;	and optionally the partial derivatives, in 1-D or 2-D.
; CALLING EXAMPLES:
;	f = Lorentzian( xin, parms )
;	f = Lorentzian( xin, parms, pderiv )
; INPUTS:
;	xin = independent variable, 1-D vector or 2-D array [ nx, ny, 2 ]
;
;	parms = array, generalized Lorentzian (modified Cauchy) function parameters (5):
;		parms[0] = maximum value (factor) of Gaussian,
;		parms[1] = center pixel,
;		parms[2] = radius at half max,
;		parms[3] = power scalling from center,
;		parms[4] = power exponent factor.
; OUTPUT (optional):
;	pderiv = matrix of partial derivatives respect to parameters:
;		always array [ npoints, nparams ]
; PROCEDURE:
;	Case of dimension breakdown.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;	F.V. 1994, added optional sixth parameter = constant offset.
;	F.V. 2007 @ UF-astro: added 2-dimensional code option.
;-

function Lorentzian, xin, parameters, pderiv

	szx = size( xin )
	if( szx[0] gt 1 ) then ndim = szx[szx[0]]  else ndim = 1
	szp = size( parameters )

	if (szp[0] EQ 2) AND (szp(2) GE 5) then begin
		parms = double( transpose( parameters ) )
		szp = size( parms )
	  endif else parms = double( parameters )

	Nparm = szp[1]
	pdim = szp[0]

	if (Nparm LT 5) then begin
		message,"need at least 5 parameters",/INFO
		return,(-1)
	   endif

CASE ndim OF

1: BEGIN
	factor = parms[0]
	cntrd = parms[1]
	radius = parms[2]
	pscale = parms[3]
	power = parms[4]

	xm = xin - cntrd
	xa = abs( xm )
	alpha = xa/radius
	gama = xa * sqrt( abs( pscale ) )
	beta = power * ( 1 + gama )
	apb = ( alpha^beta ) < 1.e30
	Lcf = 1/(1 + apb)

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( alpha ), Nparm )
		pderiv[*,0] = Lcf
		Logalf = aLog( alpha > 1.e-33 )

		w = where( alpha GT 0, np )
		apbm2 = apb
		if (np GT 0) then apbm2[w] = apb[w] / alpha[w]^2

		pderiv[*,1] = xm * apbm2 * beta / radius^2
		w = where( gama GT 0, np )

		if (np GT 0) then pderiv[w,1] = pderiv[w,1] + $
			(power * pscale) * ( xm[w] * Logalf[w] * apb[w] / gama[w] )

		xm2 = xm^2
		pderiv[*,2] = beta * apbm2 * xm2 / radius^3

		if (np GT 0) then pderiv[w,3] = -power * $
					( Logalf[w] * apb[w] * xm2[w] / gama[w] )

		pderiv[*,4] = -apb * Logalf * (1 + gama)

		df = factor * Lcf^2
		for i=1,4 do pderiv[*,i] = df * pderiv[*,i]
		if (Nparm GE 6) then  pderiv[*,5] = 1
	   endif

	if (Nparm GE 6) then return, factor * Lcf + parms(5) $
			else return, factor * Lcf
     END

2: BEGIN
	if( pdim gt 1 ) then begin
		factor = parms[0,0]
		cntx = parms[1,0]
		cnty = parms[1,1]
		radx = parms[2,0]
		rady = parms[2,1]
		pscalx = parms[3,0]
		pscaly = parms[3,1]
		power = parms[4,0]
		if (Nparm GE 6) then begin
			offset = parms[5,0]
			Nparm = 9
		 endif else begin
			offset= 0
			Nparm = 8
		  endelse
	 endif else begin
		factor = parms[0]
		cntx = parms[1]
		cnty = parms[2]
		radx = parms[3]
		rady = parms[4]
		pscalx = parms[5]
		pscaly = parms[6]
		power = parms[7]
		if (Nparm GT 8) then offset = parms[8] else offset=0
	  endelse

	xm = xin[*,*,0] - cntx
	ym = xin[*,*,1] - cnty
	xm2 = xm*xm
	ym2 = ym*ym
	xr = xm/radx
	yr = ym/rady
	xr2 = xr*xr
	yr2 = yr*yr
	alpha = sqrt( xr2 + yr2 )
	gama = sqrt( pscalx*xm2 + pscaly*ym2 )
	beta = power * ( 1 + gama )
	apb = ( alpha^beta ) < 1.e33
	Lcf = 1/(1 + apb)
	Lcff = factor * Lcf

	if N_params() GE 3 then begin

		Npts = N_elements( xm )
		pderiv = dblarr( Npts, Nparm )
		pderiv[*,0] = reform( Lcf, Npts )

		dadrx = -reform( xr2, Npts )/radx
		dadry = -reform( yr2, Npts )/rady
		dadcx = -reform( xr, Npts )/radx
		dadcy = -reform( yr, Npts )/rady
		w = where( alpha GT 0, ngz )
		alpha = alpha[w]
		dadrx[w] = dadrx[w]/alpha
		dadry[w] = dadry[w]/alpha
		dadcx[w] = dadcx[w]/alpha
		dadcy[w] = dadcy[w]/alpha
		Logalf = dblarr( Npts )
		Logalf[w] = aLog( alpha )
		apb = reform( apb, Npts )

		pderiv[*,7] = apb * Logalf * reform( 1 + gama, Npts )

		beta = reform( beta, Npts )
		bda = beta
		bda[w] = bda / alpha
		apbm1 = apb
		apbm1[w] = apbm1[w] / alpha

		dgdcx = -pscalx * reform( xm, Npts )
		dgdcy = -pscaly * reform( ym, Npts )
		dgdpx = reform( xm2, Npts )/2
		dgdpy = reform( ym2, Npts )/2
		w = where( gama GT 0, ngz )
		gama = gama[w]
		dgdpx[w] = dgdpx[w]/gama
		dgdpy[w] = dgdpy[w]/gama
		dgdcx[w] = dgdcx[w]/gama
		dgdcy[w] = dgdcy[w]/gama
		Logalf = power * Logalf

		pderiv[*,1] = apb * ( Logalf*dgdcx + bda*dadcx )
		pderiv[*,2] = apb * ( Logalf*dgdcy + bda*dadcy )

		pderiv[*,3] = beta * apbm1 * dadrx
		pderiv[*,4] = beta * apbm1 * dadry

		pderiv[*,5] = apb * Logalf * dgdpx
		pderiv[*,6] = apb * Logalf * dgdpy

		df = reform( -Lcff * Lcf, Npts )
		for i=1,7 do pderiv[*,i] = df * pderiv[*,i]
		if (Nparm GE 9) then  pderiv[*,8] = 1
	   endif

	if( offset ne 0 ) then return, Lcff + offset  else  return, Lcff

     END

else: BEGIN

	help, xin
	message,"Unsupported dimensionality of independant variable: xin"
     END

ENDCASE
end
