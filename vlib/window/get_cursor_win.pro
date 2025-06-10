;+
; NAME:
;	get_cursor_win
;
; PURPOSE:
;	Find out which window the cursor is in and return the window number.
;	The cursor Location is also returned.
;
; CALLING:
;	window_number = get_cursor_win( x, y )
;
; INPUTS:	none
; KEYWORDS:	none
;
; OUTPUTS:
;	x, y = Location of cursor if found in some window.
;
;	Function returns the window number if cursor is found in it,
;	otherwise returns -1.
;
; PROCEDURE:
;	Get the numbers of currently open windows
;	and check cursor position in each window (see if positive).
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1990.
;-

function get_cursor_win, x, y

	device, WINDO=winflags
	windows = where( winflags, Nwin )

	x = -1
	y = -1

	for iw = 0, Nwin-1 do begin

		win = windows(iw)
		device, WINDO=winflags		;double check

		if winflags(win) then begin
			wset, win
			cursor,x,y,/DEV,/NOWAIT
			if (x GE 0) then return, win
		   endif
	  endfor

return,(-1)
end
