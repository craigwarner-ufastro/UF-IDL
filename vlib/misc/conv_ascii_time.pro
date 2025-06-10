;+
; NAME:
;	conv_ascii_time
; PURPOSE:
;	Convert time strings of form 'hh:mm:ss.s' into seconds
;	relative to start of day or start of month / year if dates are given.
;	(integer or floating point depending if decimal point is found).
; CALLING:
;	seconds = conv_ascii_time( times, dates )
; INPUTS:
;	time_strings = string array in form  "hh:mm:ss".
;	date_strings = string array in form  "mm/dd/yy"  or  "dd-mmm-yy"
; LIMITATIONS:
;	If dates have different month or year the relative times will be wrong.
; OUTPUTS:
;	Function results is seconds relative to begining of month.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1991.
;-

function conv_ascii_time, time_strings, date_strings

	h = strmid( time_strings, 0, 2 )
	m = strmid( time_strings, 3, 2 )
        wp = where( strpos( time_strings,".") gt 0, np )

        if( np gt 0 ) then begin
           seconds = h*3600L + m*60 + double( strmid( time_strings, 6, 9 ) )
        endif else begin
           seconds = h*3600L + m*60 + Long( strmid( time_strings, 6, 2 ) )
        endelse

	Nday = N_elements( date_strings )
	if Nday NE N_elements( time_strings ) then return, seconds

	ws = where( strpos( date_strings, "/" ) GT 0, ns )
	wd = where( strpos( date_strings, "-" ) GT 0, nd )
	wc = where( strpos( date_strings, ":" ) GT 0, nc )
	wb = where( strpos( date_strings, " " ) GT 0, nb )

	if (ns + nd + nb + nc) NE Nday then begin
           message,"found some unknown type of date strings",/INFO
        endif

	day = intarr( Nday )

	if (ns GT 0) then  day[ws] = fix( strmid( date_strings[ws], 3, 2 ) )
	if (nd GT 0) then  day[wd] = fix( strmid( date_strings[wd], 8, 2 ) )
	if (nc GT 0) then  day[wc] = fix( strmid( date_strings[wc], 4, 3 ) )
	if (nb GT 0) then  day[wb] = fix( strmid( date_strings[wb], 4, 2 ) )

        seconds = seconds + day * (24 * 3600L)

        if N_elements( seconds ) eq 1 then return, seconds[0]  else  return, seconds
end
