function blinkdrag_image, xioff,yioff, image, xmov,ymov

; While blinking an image, drag it across window using mouse.
;	xioff & yioff are the offsets of cursor inside the image
;					(relative to lower-left corner).
;	xmov & ymov are the starting and ending Location of corner of image.

; Note: this code for IDL on Sun386i only.
; 	Sun386i is the slowest CPU and oldest version of IDL (2.0.4) so
;		do not check for out of window on TVRD and no blink timing.

;Frank Varosi NASA/GSFC 1989

	sim = size( image )
	xsiz = sim(1)
	ysiz = sim(2)

	tvcrs, xioff + xmov, yioff + ymov
	!err = 0

	while (!err LE 0) do begin
		cursor,xc,yc,/DEVICE,/NOWAIT		;get new Location
		if (xc GE 0) then xmov = xc - xioff	;Location of corner
		if (yc GE 0) then ymov = yc - yioff
		x = xmov>0
		y = ymov>0
		imsav = tvrd( x,y, xsiz,ysiz )		;get background
		if (x EQ xmov) AND $			;blink image at new Loc
		   (y EQ ymov) then tv, image, x,y $
			       else tv, image(x-xmov:*,y-ymov:*), x,y
		empty
		tv, imsav, x,y				;blink background
	endwhile

return, !mouse.button
end
