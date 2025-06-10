;+
; NAME:
;	modpos
; PURPOSE:
;	Apply the MOD operator to array, but keep result positive, or
;	optionally, centered around zero in a periodic interval fashion.
;
; CALLING EXAMPLES:
;	To MOD into interval [0,1] :
;
;		xmod = modpos( xin, WRAP=1 )
;
;	To MOD into interval [-1,1] :
;
;		xmod = modpos( xin, WRAP=1, /ZERO_CENT )
;
;	so that in all cases interval has periodic boundary conditions.
;
; INPUTS:
;	xin = array to MOD.
; KEYWORDS:
;	WRAP_VALUE = scalar, determines the interval from [0,wrap]
;			or [-wrap,wrap] if /ZERO set (default: wrap = 2 * !pi ).
;
;	/ZERO_CENTERED : keep result in periodic interval centered on zero.
; RESULT:
;	Function returns the array shifted to be in specified interval.
; PROCEDURE:
;	Just check for negative values and shift by the wrap.
;	The zero-centered case is done by recursive call to modpos and shifting.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;-

function modpos, xin, WRAP_VALUE=wrap, ZERO_CENTERED=zero_centered

	if N_elements( wrap ) NE 1 then begin
		s = size( xin )
		if ( s(s(0)+1) GT 4 ) then wrap = 2*!dpi else wrap = 2*!pi
	   endif

	if keyword_set( zero_centered ) then $
		return, modpos( xin + wrap, WRAP=2*wrap ) - wrap

	x = xin MOD wrap
	Lz = where ( x LT 0, nn )
	if (nn GT 0) then x(Lz) = x(Lz) + wrap

return, x
END
