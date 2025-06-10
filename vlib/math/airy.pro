;+
; NAME:
;	airy
;
; PURPOSE:
;	Compute the obscured Airy disc diffraction pattern function,
;	and optionally the partial derivatives, in 1-D or 2-D.
;
; CALLING:
;	f = airy( xin, parms )
;	f = airy( xin, parms, pderiv )
;
; INPUTS:
;	xin = independent variable (pixels), 1-D vector or 3-D array [ nx, ny, 2 ]
;	      if 3-D array then function is 2-D and  
;	      [*,*,0] are the x-coordinates and [*,*,1] are y-coordinates of pixels.
;
;	parms = parameters of Airy, 4 element vector in 1-D:
;
;		parms[0] = maximum value (factor),
;		parms[1] = center pixel,
;		parms[2] = (mirror_diameter/wavelength) * (arcsec/pixel)
;		parms[3] = ratio of secondary mirror diameter to primary.
;		parms[4] = (optional) constant offset added.
;
;	In 2-D a 5 element vector:
;
;		parms[0] = maximum value (factor),
;		parms[1] = center x-pixel,
;		parms[2] = center y-pixel,
;		parms[3] = (mirror_diameter/wavelength) * (arcsec/pixel)
;		parms[4] = ratio of secondary mirror diameter to primary.
;		parms[5] = (optional) constant offset added.
;
;	Diameter and wavelength should be in same units.
;
; OUTPUT:
;	pderiv = optional output of partial derivatives,
;		computed only if parameter is present in call,
;		always array [ npoints, nparams ]
;
;		pderiv[*,i] = partial derivative at all xin absisca values
;		with respect to parms[i], i=0,1,2.
;
;	Function returns array of Airy evaluated at xin.
;
; HISTORY:
;	Written: Frank Varosi, Astron. dept., U. of. Fl., 2008.
;	Mod.: Frank Varosi, UFastro, 2012, fixed mistake with parms[3] (mirror etc.)
;-

function airy, xin, parameters, pderiv

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
	factor = parms[0]
	cntrd = parms[1]
	pka = parms[2]
	eps = parms[3]

	rm = abs( xin - cntrd )
        radarc = 2*!PI/360/60/60
	prm = !PI * pka * rm * radarc
	Besj1 = Beselj( prm, 1 )
	epr = eps * prm
	Besj1e = Beselj( epr, 1 )
	wz = where( prm eq 0, nzpr )

	if( nzpr gt 0 ) then begin
		Besj1[wz] = 0.5
		Besj1e[wz] = eps/2  ;;note this will be multiplied by eps next
		prm[wz] = 1
	  endif

	Besjed = ( Besj1 - eps * Besj1e )/prm
	Airy = 4 * Besjed * Besjed

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( xin ), Nparm )
		pderiv[*,0] = Airy

		dBde = -Besj1e/prm - (eps/2)*( Beselj( epr, 0 ) - Beselj( epr, 2 ) )
		if( nzpr gt 0 ) then prm[wz] = 0
		eps2 = eps*eps
		dBdp = (eps2 * Beselj( epr, 2 ) - Beselj( prm, 2 ) )/pka
		if( nzpr gt 0 ) then rm[wz] = 1e-9
		dBdr = pka * dBdp / rm

		if( nzpr gt 0 ) then begin
			dBdr[wz] = 0
			dBde[wz] = -eps
		   endif

		cf = 2 * factor * 4
		pderiv[*,1] = -cf * Besjed * dBdr
		pderiv[*,2] = cf * Besjed * dBdp
		pderiv[*,3] = cf * Besjed * dBde

		if (Nparm GE 5) then  pderiv[*,4] = 1
	   endif

	if (Nparm GE 5) then return, factor * Airy + parms[4] $
			else return, factor * Airy
     END

2: BEGIN
	if( pdim gt 1 ) then begin
		factor = parms[0,0]
		cntx = parms[1,0]
		cnty = parms[1,1]
		pka = parms[2,0]
		eps = parms[3,0]
		if (Nparm GE 4) then begin
			offset = parms[4,0]
			Nparm = 6
		 endif else begin
			offset= 0
			Nparm = 5
		  endelse
	 endif else begin
		factor = parms[0]
		cntx = parms[1]
		cnty = parms[2]
		pka = parms[3]
		eps = parms[4]
		if (Nparm GT 5) then offset = parms[5] else offset=0
	  endelse

	xm = ( xin[*,*,0] - cntx )
	ym = ( xin[*,*,1] - cnty )
	rm = sqrt( xm*xm + ym*ym )
        radarc = 2*!PI/360/60/60
	prm = !PI * pka * rm *radarc
	Besj1 = Beselj( prm, 1 )
	epr = eps * prm
	Besj1e = Beselj( epr, 1 )
	wz = where( prm eq 0, nzpr )

	if( nzpr gt 0 ) then begin
		Besj1[wz] = 0.5
		Besj1e[wz] = eps/2  ;;note this will be multiplied by eps next
		prm[wz] = 1
	  endif

	Besjed = ( Besj1 - eps * Besj1e )/prm
	Airy = 4 * Besjed * Besjed

	if N_params() GE 3 then begin

		np = N_elements( Airy )
		pderiv = fltarr( np, Nparm )
		pderiv[*,0] = reform( Airy, np ) 

		rm = reform( rm, np )
		prm = reform( prm, np )
		epr = reform( epr, np )
		Besj1 = reform( Besj1, np )
		Besj1e = reform( Besj1e, np )
		Besjed = reform( Besjed, np )
		Besj2e = Beselj( epr, 2 )

		dBde = -Besj1e/prm - (eps/2)*( Beselj( epr, 0 ) - Besj2e )
		if( nzpr gt 0 ) then prm[wz] = 0
		eps2 = eps*eps
		dBdp = (eps2 * Besj2e - Beselj( prm, 2 ) )/pka
		if( nzpr gt 0 ) then rm[wz] = 1e-9
		dBdr = pka * dBdp / rm
		drdcx = -reform( xm )/rm
		drdcy = -reform( ym )/rm

		if( nzpr gt 0 ) then begin
			dBdr[wz] = 0
			dBde[wz] = -eps
			drdcx[wz] = -1
			drdcy[wz] = -1
		   endif

		cf = 2 * factor * 4
		pderiv[*,1] = cf * Besjed * dBdr * drdcx
		pderiv[*,2] = cf * Besjed * dBdr * drdcy
		pderiv[*,3] = cf * Besjed * dBdp
		pderiv[*,4] = cf * Besjed * dBde

		if (Nparm gt 5) then  pderiv[*,5] = 1
	   endif
 
	Airy = factor * Airy
	if (Nparm gt 5) then Airy = Airy + offset
	return, Airy
     END

else: BEGIN

	help, xin
	message,"Unsupported dimensionality of independant variable: xin"
	return,0
     END

ENDCASE
end
