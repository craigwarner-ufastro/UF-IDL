;+
; NAME:
;	box_create
; PURPOSE:
;	Interactively create a box using the cursor/mouse
;	in a rubber-band fashion. First click LEFT button for corner,
;	then drag and click MIDDLE or RIGHT for final corner.
;	Device coordinates of lower left & upper right corners, are output.
;	Function returns negative status (-4 or -2) if first click is Right/Middle button (abort),
;	else if first click is LEFT button (to start rubber-box) then
;	return status = 1 or 2 or 4 for LEFT, MIDDLE or RIGHT buttons, respectively.
; CALLING:
;	status = box_create( xLow, yLow, xUp, yUp )
; INPUTS:
;	none
; OUTPUTS:
;	xLow, yLow = device coordinates of lower left corner.
;	xUp,  yUp  = device coordinates of upper right corner.
; RETURN:
;	status = negative indicating abort, if first click is Right/Middle button,
;	         otherwise returns the value of !mouse.button for second click.
; KEYWORDS:
;	/CENTERED causes box to be centered on point selected
;			by first mouse click
;	/DATA_COORDIN causes coordinates of box to be converted to data space,
;			(default is device coordinates).
;	/DOUBLE_BOX : box is drawn as 2 rectangles,
;			outer is dark, (min color index),
;			inner is bright (max color index),
;			to help assure visibility against any background.
; EXTERNAL CALLS:
;	pro box_draw
;	pro box_erase
;	pro box_draw2
;	pro box_erase2
; COMMON BLOCKS:
;	common box_draw
; PROCEDURE:
;	Loop on box_draw and box_erase while reading cursor coordinates.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1991 added option to center rubber box around first point selected.
;	F.V. 1993 added option to convert to data coordinates.
;	F.V. 1997 added /DOUBLE_BOX option.
;	F.V. 1998, wait until mouse button is RELEASED (/UP) before returning.
;	F.V. 2011, box completion can also be indicated with Left button click.
;-

function box_create, xLow, yLow, xUp, yUp, CENTERED=centered, DATA_COORDIN=datacoor, DOUBLE_BOX=dbox

  common box_draw, Lox,Loy, Hix,Hiy, Horiz_B, Horiz_T, Vert_L, Vert_R, Rectangle, box_window

	cursor,/DEV, x1,y1		;get first corner of box.

	if (!mouse.button GT 1) then return, -!mouse.button	;signal abort.

	if keyword_set( centered ) then begin
		xc = x1
		yc = y1
	   endif

        minbut = !mouse.button + 1

	while (!mouse.button LT minbut) do begin	;get second corner of box.

                minbut = !mouse.button + 1
		cursor,/DEV, x2,y2, /CHANGE
		x2 = x2+1
		y2 = y2+1	; so that box is not obscured by cursor.

		if (x2 LE 0) OR (y2 LE 0) OR	$
		   (x2 GE !D.x_vsize) OR 	$
		   (y2 GE !D.y_vsize) then  wait,0.1  else begin

			if keyword_set( dbox ) then begin   ;double-rubber-box:

                           box_erase2

                           if keyword_set( centered ) then begin
                              box_draw2, POS=[xc,yc], RADIUS=abs( [x2-xc,y2-yc] )
                           endif else box_draw2, POS=[x1<x2,y1<y2], SIZE=abs( [x2-x1,y2-y1] )

                        endif else begin ;single rubber-box effect:

                           if keyword_set( centered ) then begin
                              x1 = 2*xc - x2
                              y1 = 2*yc - y2
                           endif

                           box_erase
                           box_draw, x1,y1,x2,y2
                        endelse
		   endelse
	  endwhile

	if keyword_set( datacoor ) then begin
		d = convert_coord( [Lox,Hix], [Loy,Hiy], /DEVICE, /TO_DATA )
		xLow = d[0,0]
		xUp = d[0,1]
		yLow = d[1,0]
		yUp = d[1,1]
	  endif else begin
		xLow = Lox	;return coordinates of box from common block
		yLow = Loy
		xUp = Hix
		yUp = Hiy
	   endelse

	if (!mouse.button GE 4) then begin
		if keyword_set( dbox ) then box_erase2 else box_erase
	   endif

	mbutton = !mouse.button	;because of bug in v4 IDL must save value
				; of button cause next cursor,/UP destroys it.
	cursor,/DEV,x,y,/UP	;wait until mouse button is released.
	!mouse.button = mbutton

return, mbutton
end
