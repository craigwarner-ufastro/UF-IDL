;+
; NAME:
;	box_draw2
; PURPOSE:
;	Draw two boxes (rectangles) in the current window,
;	the first with specified color at specified location,
;	the second just around the first with minimum color index (black)
;	so that the double box can be seen against any background image.
;	The window image data is saved in common
;	so that the double box can be erased with box_erase2.
; CALLING:
;	box_draw2, POS_XY=, RADIUS_XY=, SIZE_XY=, COLOR=
; KEYWORD INPUTS:
;	RADIUS_XY = radius of box, 1 or 2 integers.
;	SIZE_XY = diameter of box, 1 or 2 integers (overrides radius).
;	POS_XY = 2 integers, specifying position of box center if RADIUS given,
;		or specifying position of box lower-left corner if SIZE given.
;	COLOR = index to color table, default = !D.table_size-1
;	/NOSAVE : do not bother to save window data in common block.
; OUTPUTS:
;	none
; EXTERNAL CALLS
;	pro box_draw
;	pro box_save
; PROCEDURE:
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1997.
;-

pro box_draw2, POS_XY=posxy, RADIUS_XY=radius, SIZE_XY=sizxy, COLOR=bcol, NOSAVE=nosave

	box_draw, POS_XY=posxy, RADIUS=radius, SIZE=sizxy, COLOR=bcol, NOSAV=nosave
	if NOT keyword_set( nosave ) then box_save

	if N_elements( sizxy ) ge 1 then begin

		box_draw, POS_XY=posxy-1, SIZE_XY=sizxy+2, COLOR=0, NOSAVE=nosave

	 endif else if N_elements( radius ) ge 1 then begin

		box_draw, POS_XY=posxy, RADIUS_XY=radius+1, COLOR=0, NOSAVE=nosave

	  endif else begin

		print,"syntax:	box_draw2, POS_XY=[x,y], RADIUS_XY=, COLOR="
		print,"or:	box_draw2, POS_XY=[x,y], SIZE_XY=, COLOR="
	   endelse
end
