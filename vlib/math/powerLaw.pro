;+
; NAME:
;	powerLaw
;
; PURPOSE:
;	Compute the powerLaw function, and partial derivatives
;
; CALLING:
;	f = powerLaw( xin, parms )
;	f = powerLaw( xin, parms, pderiv )
;
; INPUTS:
;	xin = independent variable, vector 
;
;	parms = parameters of powerLaw, 2 element array:
;		parms[0] = factor
;		parms[0] = power Law exponent
; OUTPUT:
;	pderiv = optional output of partial derivatives,
;		computed only if parameter is present in call,
;		always array [ npoints, nparams ]
;
;		pderiv[*,i] = partial derivative at all xin absisca values
;		with respect to parms[i], i=0,1,2.
;
;	Function returns array of PowerLaw evaluated at xin.
;
; HISTORY:
;	Written: Frank Varosi UF-astro 2011.
;-

function powerLaw, xin, params, pderiv

	Nparm = N_elements( params )

	if (Nparm LT 2) then begin
		message,"need at least 2 parameters",/INFO
		return,(-1)
	   endif

	parms = double( params )

        if( Nparm gt 2 ) then begin
           fvalp = parms[0] * xin^parms[1]
           fval = fvalp + parms[2]
        endif else fval = parms[0] * xin^parms[1]

	if N_params() GE 3 then begin

		pderiv = fltarr( N_elements( xin ), Nparm )

                if( Nparm gt 2 ) then begin
                   pderiv[*,0] = fvalp/parms[0]
                   pderiv[*,1] = fvalp * aLog( xin )
                   pderiv[*,2] = 1
                endif else begin
                   pderiv[*,0] = fval/parms[0]
                   pderiv[*,1] = fval * aLog( xin )
                endelse
	   endif

return, fval
end
