;+
; NAME:
;	read_window
;
; PURPOSE:
;	Read pixel values from a graphics window and return as array (image).
;
; CALLING:
;	image = read_window( window )
;
; INPUTS:
;	window = # of window to read, default is window in which cursor is.
;
; EXTERNAL CALLS:
;	function get_cursor_win
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1991.
;-

function read_window, window

	if N_elements( window ) NE 1 then window = get_cursor_win()

	if (window LT 0) then begin
		message,"bad window number: " + strtrim( window, 2 ),/INFO
		return, window
	   endif

	wset, window
	message,"performing TVRD of window #" + strtrim( window, 2 ) + $
		" size: " + strtrim( !D.x_vsize, 2 ) + " X " + $
		strtrim( !D.y_vsize, 2 ),/INFO

return, tvrd( 0,0, !D.x_vsize, !D.y_vsize )
end
