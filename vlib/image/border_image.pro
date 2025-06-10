pro border_image, xmin_Loc, ymin, xmax, ymax, color, number, COLOR=color2

; Draw a one pixel width border around an image on screen at
;  window device coordinates (xmin,ymin,xmax,ymax) (can be negative).
; If image is only partially in window, the partial border is drawn.
; Default color is !D.table_size-1,
;  and if arg. number is specified it is printed at left bottom corner of image.
;Frank Varosi NASA/GSFC 1989
;F.V. 1991, option to pass (xmin,ymin,xmax,ymax) vector in 1st. arg. <xmin_Loc>
;					and keyword COLOR overrides arg. color.

	if N_elements( xmin_Loc ) EQ 4 then begin
		xmin = xmin_Loc(0)
		xmax = xmin_Loc(1)
		ymin = xmin_Loc(2)
		ymax = xmin_Loc(3)
		if N_elements( color2 ) EQ 1 then  color = color2
	  endif else   xmin = xmin_Loc

	if (xmin GE !D.x_vsize) OR (ymin GE !D.y_vsize) then return

	if N_elements( color ) NE 1 then  color = !D.table_size-1
	color = byte( color )
	horiz = replicate( color, xmax-xmin+1, 1)
	vert = replicate( color, 1, ymax-ymin+1 )

	if (xmin GE 0) AND (ymin GE 0) then begin

		tv, horiz, xmin, ymin
		tv, vert, xmin, ymin
		if (ymax LT !D.y_vsize) then  tv, horiz, xmin, ymax
		if (xmax LT !D.x_vsize) then  tv, vert, xmax, ymin

		if N_elements( number ) EQ 1 then $
		xyouts, xmin, ymin+2, string( number, FORM="(I3)" ),FONT=0,/DEV

	  endif	else if (xmax GT 0) AND (ymax GT 0) then begin

		if (ymin GT 0) then  tv, horiz( (-xmin>0):* ), xmin>0, ymin
		if (xmin GT 0) then  tv, vert( *, (-ymin>0):* ), xmin, ymin>0

		if (ymax LT !D.y_vsize) then  $
					tv, horiz( (-xmin>0):* ), xmin>0, ymax

		if (xmax LT !D.x_vsize) then  $
					tv, vert(*, (-ymin>0):* ), xmax, ymin>0
	   endif
return
end
