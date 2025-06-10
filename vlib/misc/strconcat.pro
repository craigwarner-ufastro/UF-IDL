;+
; NAME:
;	strconcat
; PURPOSE:
;	Concatenate a string array into single string,
;	or a matrix of strings into a string array.
; CALLING:
;	string = strconcat( string_array )
; INPUTS:
;	string_array = array of strings or numbers, if numbers they
;			will be converted to strings before concatenating.
; OUTPUT:
;	Function returns a single string.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function strconcat, string_array

	sz = size( string_array )

	if sz(sz(0)+1) NE 7 then begin
		if sz(sz(0)+2) LE 0 then return,""
		return, strconcat( string( string_array ) )
	   endif

	if (sz(0) LE 0) then return,string_array else if (sz(0) EQ 1) then begin

		strsum = string_array(0)
		for i=1,sz(1)-1 do strsum = strsum + string_array(i)

	 endif else if (sz(0) EQ 2) then begin

		strsum = string_array(*,0)
		for i=1,sz(2)-1 do strsum = strsum + string_array(*,i)

	  endif else strsum = ""

return, strsum
end
