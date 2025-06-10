;+
; NAME:
;	valid_window
;
; PURPOSE:
;	Check if a window number is valid, in existence.
;
; CALLING:
;	if( valid_window( winum ) ) then .....
;
; INPUTS:
;	winum = integer window # to check.
;
; OUTPUTS:
;	Function returns 1 if window # actually exists, otherwise 0.
;
; PROCEDURE:
;	Get the numbers of currently open windows and check if in table.
; HISTORY:
;	Written: Frank Varosi UF 2011.
;-

function valid_window, winum

  if N_elements( winum ) NE 1 then return,0
  if (winum LT 0) then return,0

  CASE !D.name OF
     "X":	device,WINDOW_STATE=win_flags
     "SUN":	device,WINDOW_STATE=win_flags
     "WIN":	device,WINDOW_STATE=win_flags
     "MAC":	win_flags = intarr(32)
     else:	return,-1
  ENDCASE

  if winum ge N_elements( win_flags ) then return,0

  return, win_flags[winum]
end
