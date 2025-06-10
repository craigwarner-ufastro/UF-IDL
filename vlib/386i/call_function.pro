function call_function, func_name, p1, p2, p3

; For compatibility of Sun386i version of IDL with newer versions.
; Frank Varosi NASA/GSFC 1992.

	exec_string = "fval = " + func_name

	CASE N_params()-1 OF

	   0:	exec_string = exec_string + "( )"

	   1:	exec_string = exec_string + "( p1 )"

	   2:	exec_string = exec_string + "( p1, p2 )"

	   3:	exec_string = exec_string + "( p1, p2, p3 )"

	else:	return,(0)

	ENDCASE

	if (NOT execute( exec_string ) ) then begin
		message," error executing: " + exec_string
		if N_elements( fval ) LE 0 then fval = 0
	   endif

return, fval
end
