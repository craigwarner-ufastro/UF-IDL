;+
; NAME:
;	box_draw
; PURPOSE:
;	Draw a Box (rectangle) in the current window, with specified color,
;	first saving the data in common block,
;	so it can be later restored with pro box_erase.
; CALLING:
;	box_draw, x1,y1, x2,y2, color
; INPUTS:
;	x1, y1 = device coordinates of lower left corner.
;	x2, y2 = device coordinates of upper right corner.
;	color = index to color table, default = !D.table_size-1
; KEYWORDS:
;   Alternate (overrides args.) method of specifying box:
;	RADIUS_XY = radius of box, 1 or 2 integers, default=1.
;	SIZE_XY = diameter of box, 1 or 2 integers (overrides radius).
;	POS_XY = 2 integers, specifying position of box center if RADIUS given,
;		or specifying position of box lower-left corner if SIZE given.
;	COLOR = index to color table, default = !D.table_size-1
;	/NOSAVE : do not bother to save window data in common block.
; OUTPUTS:
;	none
; COMMON BLOCKS:
;	common box_draw
; PROCEDURE:
;	If size of box > 16 :
;	read pixels at sides of desired box using TVRD, store into common,
;	display the four sides of box using TV.
;	If size of box < 16 :
;	read all pixels in desired box using TVRD, store into common,
;	set the border to desired color, redisplay using TV.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1990, if Box < 16x16 then save and redisplay with interior also.
;	F.V. 1992, added keywords POS=[x,y] and RADIUS=(radius or [radx,rady]).
;	F.V. 1993, option to pass all 4 coordinates in first argument.
;	F.V. 1995, option to pass [x,y] coordinates in first 2 arguments.
;	F.V. 1997, added full argument presence checking.
;	F.V. 2024, added Limits to size or radius to avoid wierdness.
;-

pro box_draw, x1,y1, x2,y2, color, POS_XY=posxy, RADIUS_XY=radius, $
				SIZE_XY=sizxy, COLOR=bcol, NOSAVE=nosave

  common box_draw, Lox,Loy, Hix,Hiy, $
		   Horiz_B, Horiz_T, $
		   Vert_L, Vert_R, Rectangle, box_window

  if N_elements( x1 ) EQ 4 then begin
     box_draw, x1[0], x1[1], x1[2], x1[3], COLOR=bcol, NOSAV=nosave
     return
  endif else if (N_elements( x1 ) EQ 2) AND $
     (N_elements( y1 ) EQ 2) then begin
     box_draw, x1[0], x1[1], y1[0], y1[1], COLOR=bcol, NOSAV=nosave
     return
  endif

	box_window = !D.window
	x_vLim = !D.x_vsize-1
	y_vLim = !D.y_vsize-1

	if N_elements( posxy ) EQ 2 then begin

		x = posxy[0]
		y = posxy[1]

		if N_elements( sizxy ) GE 1 then begin

                   if N_elements( sizxy ) EQ 2 then begin
                      sizx = ceil( sizxy[0] )
                      sizy = ceil( sizxy[1] )
                   endif else begin
                      sizx = ceil( sizxy[0] )
                      sizy = sizx
                   endelse

                   sizx >= 3
                   sizy >= 3
                   Lox = ( x > 0 ) < x_vLim
                   Loy = ( y > 0 ) < y_vLim
                   Hix = ( (x + sizx) < x_vLim ) > Lox
                   Hiy = ( (y + sizy) < y_vLim ) > Loy

                endif else begin

                   if N_elements( radius ) EQ 2 then begin
                      radx = ceil( radius[0] )
                      rady = ceil( radius[1] )
                   endif else if N_elements( radius ) EQ 1 then begin
                      radx = ceil( radius[0] )
                      rady = radx
                   endif else begin
                      radx = 1
                      rady = 1
                   endelse

                   radx >= 1
                   radY >= 1
                   Lox = ( (x - radx) > 0 ) < x_vLim
                   Loy = ( (y - rady) > 0 ) < y_vLim
                   Hix = ( (x + radx) < x_vLim ) > Lox
                   Hiy = ( (y + rady) < y_vLim ) > Loy
                endelse

	  endif else if N_params() ge 4 then begin

		Lox = ( (x1 < x2) > 0 ) < x_vLim
		Loy = ( (y1 < y2) > 0 ) < y_vLim

		Hix = ( (x1 > x2) < x_vLim ) > 0
		Hiy = ( (y1 > y2) < y_vLim ) > 0

	   endif else begin

		print,"syntax:	box_draw, POS_XY=[x,y], RADIUS_XY=, COLOR="
		print,"or:	box_draw, POS_XY=[x,y], SIZE_XY=, COLOR="
		print,"or:	box_draw, x1,y1, x2,y2, color"
		return
	    endelse

	xsiz = ceil( Hix - Lox +1 )>3
	ysiz = ceil( Hiy - Loy +1 )>3
	if N_elements( bcol ) EQ 1 then  color = bcol
	if N_elements( color ) NE 1 then  color = !D.table_size-1
	color = byte( color )

	if (xsiz GT 16) AND (ysiz GT 16) then begin

		if NOT keyword_set( nosave ) then begin
			Horiz_B = tvrd( Lox,Loy, xsiz,1 )  ;;save data first.
			Horiz_T = tvrd( Lox,Hiy, xsiz,1 )
			Vert_L = tvrd( Lox,Loy, 1,ysiz )
			Vert_R = tvrd( Hix,Loy, 1,ysiz )
		   endif

		horiz = replicate( color, xsiz, 1 )
		vert = replicate( color, 1, ysiz )

		tv, horiz, Lox,Loy			;draw perimeter only.
		tv, vert, Lox,Loy
		tv, horiz, Lox,Hiy
		tv, vert, Hix,Loy
		Rectangle = 0

	  endif else if (xsiz GT 2) AND (ysiz GT 2) then begin

             Rectangle = tvrd( Lox,Loy, xsiz, ysiz ) ;;save data.
             box = replicate( color, xsiz, ysiz )
             box[1,1] = Rectangle(1:xsiz-2,1:ysiz-2)
             tv, box, Lox,Loy   ;redisplay rectangle with box.
             Horiz_B = 0
          endif
end
