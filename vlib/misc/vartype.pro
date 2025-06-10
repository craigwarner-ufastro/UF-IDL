;+
; NAME:
;	vartype
;
; PURPOSE:
;	Check the type of IDL variables.
;
; CALLING:
;	type_string = vartype( variable )
; or:
;	type_code = vartype( variable, /CODE )
;
; INPUTS:
;	variable = anything.
;
; KEYWORDS:
;	/CODE : causes the integer IDL type code to be returned,
;		instead of a string describing variable type.
;
;	/NO_NULL_STRING: consider variable a string only if nonzero length.
;
; OUTPUTS:
;	sz = (optional) the result of the function size( variable ).
;
;	Function returns string describing variable type (see code below),
;	or if /CODE then the integer IDL type code (0 to 9).
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V.1994, added keyword /CODE.
;	F.V.1997, added keyword /NO_NULL_STRING.
;-

function vartype, variable, sz, CODE=code, NO_NULL_STRING=no_null

	sz = size( variable )
	type = sz[ sz[0]+1 ]

	if keyword_set( code ) then begin
           if( type eq 7 ) and keyword_set( no_null ) then begin
              if N_elements( variable ) eq 1 then begin
                 if strlen( variable ) LE 0 then return,0
              endif
           endif
           return,type
        endif

	CASE type OF
	1:	typename = "BYTE"
	2:	typename = "INTEGER SHORT"
	3:	typename = "INTEGER LONG"
	4:	typename = "FLOATING"
	5:	typename = "DOUBLE FLOATING"
	6:	typename = "COMPLEX FLOATING"
	7: BEGIN
		typename = "STRING"
		if keyword_set( no_null ) then begin
		   if N_elements( variable ) eq 1 then begin
			if strlen( variable ) LE 0 then typename = "UNDEFINED"
		     endif
		  endif
		END
	8:	typename = "STRUCTURE"
	9:	typename = "COMPLEX DOUBLE FLOATING"
	10:	typename = "POINTER"
	11:	typename = "OBJECT"
	12:	typename = "UNSIGNED SHORT"
	13:	typename = "UNSIGNED LONG"
	14:	typename = "LONG LONG"
	15:	typename = "UNSIGNED LONG LONG"
	else:	typename = "UNDEFINED"
	ENDCASE

return, typename
end
