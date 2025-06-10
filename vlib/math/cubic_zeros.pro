;+
; NAME:
;	cubic_zeros
; PURPOSE:
;	Attempt to implement Cardano's formula for the zero solutions
;	of a cubic equation. Presently written to solve reduced form,
;	with parameters input by keywords:  x^3 + px + q
; CALLING:
;
; INPUTS:
;
; KEYWORDS:
;
; OUTPUTS:
;
; PROCEDURE:
;	Not yet finished. Must handle more special cases.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1998.
;-

function cubic_zeros, coeffs, Preduc=pr, Qreduc=qr

	dr = (pr/3.)^3 + (qr/2.)^2

	if( dr LT 0 ) then begin

		dr = sqrt( complex( dr ) )
		u = ( -qr/2. + dr )^(1./3)
		v = ( -qr/2. - dr )^(1./3)

	 endif else begin

		dr = sqrt( dr )
		u = -qr/2. + dr
		v = -qr/2. - dr
		if( u LT 0 ) then u = -( abs(u)^(1./3) )
		if( v LT 0 ) then v = -( abs(v)^(1./3) )
	  endelse

	upv = u+v
	umv = 3^(1./3) * (u-v)/2

	if !DEBUG then begin
		help, dr, u, v
		if !DEBUG gt 2 then stop
	   endif

return, [ upv, complex( -upv/2, umv ), complex( -upv/2, -umv ) ]
end
