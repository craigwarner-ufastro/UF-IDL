;+
; NAME:
;	iconify_windows
;
; PURPOSE:
;	Iconify window(s), first checking if window(s) actually exists,
;	thus avoiding accidental error trap.
;
; CALLING:
;	iconify_windows, winum
;
; INPUTS:
;	winum = integer scalar or array, window number(s) to iconify.
;
; KEYWORDS:
;	/SHOW : show (de-iconify) the windows instead.
;
; OUTPUTS:	none.
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

pro iconify_windows, winum, SHOW=show

	if N_elements( winum ) GT 0 then begin

		device, WIN=wflags
		w = where( (winum GE 0) AND (winum LT N_elements(wflags)), nw )
		if keyword_set( show ) then icf=0 else icf=1

		for i=0,nw-1 do begin
			if wflags(winum(w(i))) then wshow, winum(w(i)),ICON=icf
		  endfor
	   endif
end
