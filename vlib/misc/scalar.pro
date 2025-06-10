;+
; NAME:
;	scalar
; PURPOSE:
;	Convert input to a scalar by returning just first element.
; CALLING:
;	s = scalar( x )
; INPUTS:
;	x = any variable.
; OUTPUT:
;	Function returns x(0).
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function scalar, x

return, x(0)
end
