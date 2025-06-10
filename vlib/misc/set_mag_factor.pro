;+
; NAME:
;
; PURPOSE:
;
; CALLING:
;	magf = set_mag_factor( magf_min, magf_max )
; INPUTS:
;	magf_min = minimum magnification factor, can be < 1, default=1.
;	magf_max = max mag. factor, def=4.
; KEYWORDS:
;	OLD_MAGF =
; OUTPUTS:
;	Function returns selected mag. factor.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function set_mag_factor, magf_min, magf_max, OLD_MAGF=old_magf

	if N_elements( old_magf ) NE 1 then old_magf = 1
	if N_elements( magf_min ) NE 1 then magf_min = 1
	if N_elements( magf_max ) NE 1 then magf_max = 4

	magf_max = ceil( magf_max > (2*magf_min) ) > 1
	magfac = indgen( magf_max ) +1

	if (magf_min LT 1) then begin
		redf = rotate( findgen( ceil( 1/magf_min ) )+1, 2 )
		magfac = [ 1/redf, magfac(1:*) ]
	 endif else magfac = magfac( where( magfac GE fix( magf_min ) ) )

	menu = [ "Magnification ?", string( magfac, FORMAT="(F5.2)" ) ]
	init = where( abs( old_magf - magfac ) LT 0.001, nw )
	if (nw GT 0) then init = init[0]+1 else init=N_elements( magfac )/2

	sel = wmenux( menu, INIT=init, TIT=0 ) -1
	if (sel LT 0) then sel = init

return, magfac(sel)
end
