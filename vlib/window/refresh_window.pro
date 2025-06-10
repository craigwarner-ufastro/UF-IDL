;+
; NAME:
;	refresh_window
;
; PURPOSE:
;	Refresh pixel values in a graphics window.
;
; CALLING:
;	refresh_window, windex
;
; INPUTS:
;	windex = index of window to refresh, default is window in which cursor is.
;
; HISTORY:
;	Written: Frank Varosi UF 2011.
;-

pro refresh_window, windex

	if N_elements( windex ) NE 1 then windex = get_cursor_win()
	if (windex LT 0) then return

        prevwind = !D.window
	wset, windex
	wshow, windex
        tv, tvrd( 0,0, !D.x_size, !D.y_size )
        wset, prevwind
return
end
