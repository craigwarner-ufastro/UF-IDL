;+
; NAME:
;	box_erase
; PURPOSE:
;	Erase the box (restore what was there)
;	drawn by previous call to box_draw,
;	only if current window is the one in which box was drawn.
; CALLING:
;	box_erase
; KEYWORDS:
;	/RESET : just clear common block, no changes to window.
; OUTPUTS:
;	none
; COMMON BLOCKS:
;	common box_draw
; PROCEDURE:
; HISTORY:
;	Frank Varosi NASA/GSFC 1989
;	F.V.1992 added /RESET option to just forget box (clear out common).
;-

pro box_erase, RESET=reset

  common box_draw, Lox,Loy, Hix,Hiy, $
		   Horiz_B, Horiz_T, $
		   Vert_L, Vert_R, Rectangle, box_window

	if keyword_set( reset ) then begin
		Horiz_B = 0
		Rectangle = 0
		box_window = -1
		return
	   endif

	if N_elements( box_window ) NE 1 then return
	if (box_window NE !D.window) then return

	if N_elements( Horiz_B ) GT 1 then begin	;box perimeter only.

		tv, Horiz_B, Lox,Loy
		tv, Vert_L, Lox,Loy
		tv, Horiz_T, Lox,Hiy
		tv, vert_R, Hix,Loy
		Horiz_B = 0

	  endif else if N_elements( Rectangle ) GT 1 then begin   ;whole thing.

		tv, Rectangle, Lox,Loy
		Rectangle = 0
	   endif
end
