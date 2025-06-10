;+
; NAME:
;	box_erase2
; PURPOSE:
;	Erase the double box (restore what was there)
;	drawn by previous call to box_draw2,
;	only if current window is the one in which box was drawn.
; CALLING:
;	box_erase2
; KEYWORDS:
; OUTPUTS:
;	none
; COMMON BLOCKS:
;	common box_draw
; EXTERNAL CALLS
;	pro box_erase
;	pro box_save
; PROCEDURE:
; HISTORY:
;	Frank Varosi NASA/GSFC 1997.
;-

pro box_erase2

	box_erase
	box_save,/RESTORE
	box_erase
end
