;+
; NAME:
;	sep_alpha_num
; PURPOSE:
;	Separate the characters in an array of strings into alpha-strings and
;	numeric-strings, numeric being the digits 0-9 and alpha is non-numeric.
; CALLING:
;	sep_alpha_num, strings, alpha, numeric
; INPUTS:
;	strings = string or string array.
; OUTPUTS:
;	alpha = array of strings, same number of elements as input,
;		containing only the non-numeric characters.
;	numeric = array of strings containing only the numeric characters.
;		Leading and trailing blanks are trimmed for all output.
; PROCEDURE:
;	Convert string to byte array and find where the digits are & are not.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1995.
;-

pro sep_alpha_num, strings, alpha, numeric

	bs = byte( strings )
	w = where( (bs LE 57b) AND (bs GE 48b), nw )
	if (nw GT 0) then bs(w) = 32b
	alpha = strtrim( bs, 2 )

	bs = byte( strings )
	w = where( (bs GT 57b) OR (bs LT 48b), nw )
	if (nw GT 0) then bs(w) = 32b
	numeric = strtrim( bs, 2 )
end
