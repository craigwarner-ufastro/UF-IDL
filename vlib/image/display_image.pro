;+
; NAME:
;	display_image
; PURPOSE:
;	Display the visible portion of an image in window.
; CALLING:
;	display_image, image, x,y
; INPUTS:
;	image =
;	x,y =
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1989.
;	F.V. 1991, added option (xmax,ymax) to display just central subset
;		of image, (so that edge regions of image are not displayed).
;-

pro display_image, image, x,y, xmax, ymax

	if N_elements( ymax ) EQ 1 then begin

		s = size( image )-1		;check to see if
		xsiz = xmax - x			; (x:xmax,y:ymax) selects
		ysiz = ymax - y			; centered subset of image.
		xbw = ((s(1)-xsiz)/2) > 0
		ybw = ((s(2)-ysiz)/2) > 0

		if (xbw GT 0) OR (ybw GT 0) then begin
			Lx = s(1)-xbw
			Ly = s(2)-ybw
			image = image( xbw:Lx, ybw:Ly )
		   endif
	   endif

	if (x LT !D.x_size) AND (y LT !D.y_size) then begin

	   if (x LT 0) OR (y LT 0) then begin

		sim = size( image )		;check to see if
		ic = [x,y] + sim(1:2)		; any part of image is visible.

		if ( min( ic ) GT 1 ) then $
				      tv, image( (-x>0):*, (-y>0):* ), x>0,y>0

	      endif  else  tv,image,x,y
	  endif
end
