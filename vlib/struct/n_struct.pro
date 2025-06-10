;+
; NAME:
;	N_struct
;
; PURPOSE:
;	To determine if variable is a structure and return number of elements.
;
; CALLING:
;	n = N_struct( var, ntags )
;
; INPUT:
;	var = any variable.
;
; OUTPUT:
;	ntags = number of structure tags.
;
; RESULT:
;	Returns zero if variable is not a structure, otherwise returns # elems.
;
; PROCEDURE:
;	Determine if argument is a structure by checking for # of tags.
;	If structure, use size function to get # of elements
;	(instead of N_elements) so that it works on I/O associated structures.
;
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;-

function N_struct, var, ntags

	ntags = N_tags( var )

	if (ntags LE 0) then return,(0L)  else begin
		s = size( var )
		return, s[s[0]+2]
	 endelse
end
