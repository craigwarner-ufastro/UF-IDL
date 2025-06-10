;+
; NAME:
;	conv_time_ascii
; PURPOSE:
;	Convert seconds of day into time strings of form 'hh:mm:ss.sss'.
; CALLING:
;	time_strings = conv_time_ascii( seconds )
; INPUTS:
;	seconds = array of seconds in a day.
; LIMITATIONS:
;	If seconds are greater than 24 hours there is no adjustment.
; OUTPUTS:
;	Function results is time strings of form 'hh:mm:ss.sss'.
; HISTORY:
;	Written, Frank Varosi, UF, 2011.
;	Frank Varosi, UF, 2015, make result accurate to milli-secs.
;-

function conv_time_ascii, seconds

	ht = fix( seconds/3600L )
        sh = seconds - ht*3600L
	mt = fix( sh/60L )
	st = sh - mt*60L

        hs = strmid( string( ht+100, FORM="(I3)"), 1, 2 )
        ms = strmid( string( mt+100, FORM="(I3)"), 1, 2 )
        ss = strmid( string( st+100, FORM="(F7.3)"), 1, 6 )

return, hs + ":" + ms + ":" + ss
end
