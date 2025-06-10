;+
; NAME:
;	inversefreq
;
; PURPOSE:
;	Compute the Inversefreq function, and partial derivatives
;
; CALLING:
;	f = inversefreq( xin, parms )
;	f = inversefreq( xin, parms, pderiv )
;
; INPUTS:
;	xin = independent variable, vector 
;
;	parms = parameters of Inversefreq, 2 element array:
;		parms[0] = constant offset
;		parms[0] = factor of inverse freq,
; OUTPUT:
;	pderiv = optional output of partial derivatives,
;		computed only if parameter is present in call,
;		always array [ npoints, nparams ]
;
;		pderiv[*,i] = partial derivative at all xin absisca values
;		with respect to parms[i], i=0,1,2.
;
;	Function returns array of Inversefreq evaluated at xin.
;
; HISTORY:
;	Written: Frank Varosi UF-astro 2011.
;-

function inversefreq, xin, params, pderiv

	Nparm = N_elements( params )

	if (Nparm LT 2) then begin
		message,"need at least 2 parameters",/INFO
		return,(-1)
	   endif

	parms = double( params )

        fval = parms[0]/xin + parms[1]

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( xin ), Nparm )

		pderiv[*,0] = 1/xin
		pderiv[*,1] = 1
	   endif

return, fval
end
