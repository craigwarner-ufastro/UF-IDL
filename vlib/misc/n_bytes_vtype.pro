;+
; NAME:
;	N_bytes_vtype
; PURPOSE:
;	Return the number of bytes in a scalar (or vector)
;	of specified variable type code(s), except for strings or structures
;	since they do not have predetermined size.
;	For usage example see map_var_type.pro.
; CALLING:
;	nbytes = N_bytes_vtype( type_code )
; INPUTS:
;	type_code = scalar or vector of IDL variable type codes (0 to 9).
; KEYWORDS:
;	VARIABLE = optionaly give the variable and get the type_code.
; OUTPUTS:
;	Returns the # of bytes in a single element of each given variable type,
;	unless the code is for strings or structures, then 99 is returned.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1994.
;	F.V.1995, added case for double complex variable type.
;-

function N_bytes_vtype, type_code, VARIABLE=variable

  common N_bytes_vtype, nbytes

	if N_elements( nbytes ) LE 1 then nbytes = [0,1,2,4,4,8,8,99,99,16,4,4,2,4,8,8]

	if N_elements( type_code ) LE 0 then begin
		if N_elements( variable ) LE 0 then return,0	
		s = size( variable )
		type_code = s[ s[0]+1 ]
	   endif

return, nbytes[ type_code ] 
end
