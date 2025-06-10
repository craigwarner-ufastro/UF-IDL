;+
; NAME:
;	chisqdist
;
; PURPOSE:
;	Compute the Chi-square distribution function, and partial derivatives, in 1-D or 2-D.
;
; CALLING:
;	f = chisqdist( xin, parms )
;	f = chisqdist( xin, parms, pderiv )
;
; INPUTS:
;	xin = independent variable, 1-D vector.
;
;	parms = parameters of Chi-square distribution, 2 element array:
;		parms[0] = maximum value (factor) of Chi-square distribution,
;		parms[1] = mean value (center) of Chi-square distribution,
; OUTPUT:
;	pderiv = optional output of partial derivatives,
;		computed only if parameter is present in call,
;		always array [ npoints, nparams ]
;
;		pderiv[*,i] = partial derivative at all xin absisca values
;		with respect to parms[i], i=0,1.
;
;	Function returns array of Chi-square distribution evaluated at xin.
;
; HISTORY:
;	Written: Frank Varosi @ UF-astro: 2012.
;-

function chisqdist, xin, parameters, pderiv

  parms = double( parameters )
  Nparm = N_elements( parms )

  if (Nparm LT 2) then begin
     message,"need at least 2 parameters",/INFO
     return,(-1)
  endif

  parms = double( parms )
  rd2 = parms[1]/2

  if( NParm gt 2 ) then xs = xin * parms[2] else xs = xin
  z = exp( -xs/2 )
  xr = xs^(rd2 - 1)
  chisqf = ( xr * z )/gamma( rd2 )

  if N_params() GE 3 then begin
     pderiv = fltarr( N_elements( xin ), Nparm )
     pderiv[0,0] = chisqf
     pderiv[0,1] = chisqf * alog(xs) * parms[0]/2
     if( NParm gt 2 ) then pderiv[0,2] = chisqf * (parms[1] - 2 - xs) * parms[0]/(2*parms[2])
  endif

return, parms[0] * chisqf
end
