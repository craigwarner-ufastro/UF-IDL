;+
; NAME:
;	normalize
; PURPOSE:
;	Scale the input array so that Mean_Value=1 (default action),
;	or optionally, so Total=1, or either Max=1.
; CALLING:
;	am1 = normalize( array )
; INPUTS:
;	array = data to be normalized
; KEYWORDS:
;	/TOTAL : just divide by total( array ) so that total of result =1.
;	/MAX : just divide by max( array ) so that maximum of result =1.
;		Default is to also divide by # elements so mean value =1.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-

function normalize, array, TOTAL_UNITY=total_1, MAX_UNITY=max_1

	sa = size( array )
	na = sa(sa(0)+2)

	if (na LE 1) then begin
		message,"expecting an array",/INFO
		return,array
	   endif

	if keyword_set( total_1 ) then return, array/total( array ) $
	 else if keyword_set( max_1 ) then return, array/max( array ) $
				  else return, array/( total( array )/na )
end
