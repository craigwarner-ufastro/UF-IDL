;+
; NAME:
;	blinkdrag_image
; PURPOSE:
;	Blink an image (alternately displaying image and background), 
;	drag it across window using mouse-cursor until a button is pressed.
;	New location of image and mouse button value are returned.
;	In X-windows, the window backing store should be pixmap,
;	that is, use device,RETAIN=2  or window,RETAIN=2,...
;	for fastest blinking of image.
; CALLING:
;	button = blinkdrag_image( ioffx, ioffy, image, Locx, Locy )
; INPUTS:
;	ioffx & ioffy = Location of cursor inside the image (in device pixels)
;			offset relative to lower-left corner.
;	image = byte image to display, blink and drag.
;	Locx & Locy = starting Location (pixels) of lower-left corner of image.
; KEYWORDS:
;	WAIT_DISPLAY = seconds to wait after displaying image to equalize blink,
;			default = 0.003 seconds, seems to work.
; OUTPUTS:
;	Locx & Locy = ending Location of lower-left corner of image
;				(in device pixels) after a button is pressed.
;	Result of function is value of !mouse.button.
; EXAMPLE:
;	Locx=100  &  Locy=100
;	button = blinkdrag_image( 0, 0, image, Locx, Locy )
; COMMON BLOCKS:
;	common blinkdrag1, wait_image, wait_back	;wait times in millisec.
;	common blinkdrag2, aex, aey, cex, cey		;for overlay of ellipse.
; PROCEDURE:
;	Sparcstation running SunView is faster than X-windows,
;	so use waits to stall the blink times, otherwise too much flicker.
;	For X-windows use PIXMAP and device,COPY=[...] instead of tv and tvrd.
; HISTORY:
;	written Frank Varosi NASA/GSFC 1989
;	F.V. 91, mod to keep tvrd in window, since idl-v.2.1.2 demands it.
;	F.V. 92, split into two loop codes for different display types.
;	F.V. 96, added option to overlay ellipse (coordinates in common).
;	F.V. 97, can now set blinking image/background display times in common.
;	F.V. 2011, set default blinking display times to 20 & 10 millisec.
;	F.V. 2011, use !moust.button instead of old !err sysvar.
;-

function blinkdrag_image, ioffx, ioffy, image, Locx, Locy, WAIT_DISPLAY=wdisp

   common blinkdrag1, wait_image, wait_back	;wait times in milliseconds.
   common blinkdrag2, aex, aey, cex, cey	;optional overlay of ellipse.

	sim = size( image )
	xsiz = sim[1]
	ysiz = sim[2]
	if N_elements( wait_image ) ne 1 then wait_image = 33
	if N_elements( wait_back ) ne 1 then wait_back = 33
	if N_elements( wdisp ) ne 1 then wdisp = wait_image/1000.
	wback = wait_back/1000.
	if (N_elements( aex ) eq 1) and $
	   (N_elements( aey ) eq 1) and $
	   (N_elements( cex ) eq 1) and $
	   (N_elements( cey ) eq 1) then ellipse=1 else ellipse=0
	tvcrs, ioffx + Locx, ioffy + Locy
	!mouse.button = 0

        if (!D.name EQ "SUN") or (!D.name EQ "MAC") then begin

           while (!mouse.button LE 0) do begin
		cursor,xc,yc,/DEVICE,/NOWAIT		;get new Location
		if (xc GE 0) then Locx = xc - ioffx	;Location of corner
		if (yc GE 0) then Locy = yc - ioffy
		x = Locx>0
		y = Locy>0
		xsizr = xsiz < (!D.x_size - x)		;keep read in window
		ysizr = ysiz < (!D.y_size - y)
		imsav = tvrd( x,y, xsizr,ysizr )	;get background
		if (x EQ Locx) AND $			;blink image at new Loc
		   (y EQ Locy) then tv, image, x,y $
			       else tv, image(x-Locx:*,y-Locy:*), x,y
		if (ellipse) then tvellipse, aex, aey, cex, cey
		wait, wdisp
		tv, imsav, x,y				;blink background
		wait, wback
             endwhile

        endif else if (!D.name EQ "X") then begin

           wim = !D.window
           window, /FREE, /PIXMAP, XSIZ=2*xsiz, YSIZ=ysiz
           wpixmap = !D.window
           tv,image
           wset,wim

           while (!mouse.button LE 0) do begin
		cursor,xc,yc,/DEVICE,/NOWAIT		;get new Location
		if (xc GE 0) then Locx = xc - ioffx	;Location of corner
		if (yc GE 0) then Locy = yc - ioffy
		x = Locx>0
		y = Locy>0
		xd = x-Locx
		yd = y-Locy
		xsizr = xsiz < (!D.x_size - x)		;keep read in window
		ysizr = ysiz < (!D.y_size - y)
		wset,wpixmap
		device,COPY=[x,y,xsizr,ysizr,xsiz,0,wim]	;get background
		wset,wim
		device,COPY=[xd,yd,xsizr-xd,ysizr-yd,x,y,wpixmap]  ;blink image
		if (ellipse) then tvellipse, aex, aey, cex, cey
		wait, wdisp
		device,COPY=[xsiz,0,xsizr,ysizr,x,y,wpixmap]    ;blink background
		wait, wback
             endwhile

           wdelete,wpixmap
           wset,wim

        endif

return, !mouse.button
end
