function strnumber,st,val
;+
; NAME:
;   STRNUMBER
; PURPOSE:
;   Function to determine if a string is a valid numeric value.
; CALLING SEQUENCE:
;	result = strnumber(st,val)
; INPUTS:
;	st - string
; OUTPUTS:
;	1 is returned as the function value if the string st has a
;	valid numeric value, otherwise, 0 is returned.
; OPTIONAL OUTPUT:
;	val - (optional) value of the string.  real*8
; WARNING:
;       Note that (as of Version 2.0.10) a blank string (e.g. " ") is not
;       a valid numeric value, although an empty string ("") is.
; HISTORY:
;	version 1  By D. Lindler Aug. 1987
;-
on_ioerror,L1			;Go to L1 if conversion error occurs
val=double(st)
return,1			;No conversion error
L1: return,0			;Conversion error occured
end
