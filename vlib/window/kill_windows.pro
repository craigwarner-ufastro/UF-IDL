;+
; NAME:
;	kill_windows
;
; PURPOSE:
;	Delete window(s), first checking if window(s) actually exists,
;	thus avoiding accidental error trap.
;
; CALLING:
;	win_stat = kill_windows( winum )
;
; INPUTS:
;	winum = integer scalar or array, window number(s) to delete
;		(if undefined, then nothing happens and (-1) is returned).
;
; OUTPUTS:
;	Function always returns a scalar or array of (-1), same size as input.
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function kill_windows, winum

	if N_elements( winum ) GT 0 then begin

		device, WIN=wflags
		w = where( (winum GE 0) AND (winum LT N_elements(wflags)), nw )

		for i=0,nw-1 do begin
			if wflags(winum(w(i))) then wdelete, winum(w(i))
		  endfor

		winum(*) = -1
		return, winum

	   endif else return,(-1)
end
