;+
; NAME:
;	map_var_type
; PURPOSE:
;	Map a variable to memory location of different type, but no conversion.
;	Not for use with strings or structures since
;	they do not have predetermined size.
;
; CALLING:
;	vmem = map_var_type( variable, nout, TYPE_CODE_OUT=type_out )
;
; INPUTS:
;	variable = array, the variable to be memory mapped.
;
; KEYWORDS:
;	TYPE_CODE_OUT = the desired type code of returned array.
;
; OUTPUTS:
;	nout = # elements actually returned and valid.
;
;	Returns an array of requested type containing the input variable
;	with no conversion.
;
; EXTERNAL CALLS:
;	function N_bytes_vtype
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1994.
;	F.V.1995, added case for double complex variable type.
;-

function map_var_type, variable, nout, TYPE_CODE_OUT=type_out

	if N_elements( variable ) LE 0 then begin
		nout = 0
		return,(-1)
	   endif

	s = size( variable )
	type_in = s( s(0)+1 )
	nin = s( s(0)+2 )
	nout = nin

	if N_elements( type_out ) NE 1 then return, variable
	if (type_out EQ type_in ) then return, reform( variable, nin )

	if (type_out GT 0) then nout = ceil( float( nin ) * $
			N_bytes_vtype( type_in ) / N_bytes_vtype( type_out ) )

	CASE type_out OF
		1:	return, byte( variable, 0, nout )
		2:	return, fix( variable, 0, nout )
		3:	return, Long( variable, 0, nout )
		4:	return, float( variable, 0, nout )
		5:	return, double( variable, 0, nout )
		6:	return, complex( variable, 0, nout )
		9:	return, dcomplex( variable, 0, nout )
		else:	return, reform( variable, nin )
	ENDCASE
end
