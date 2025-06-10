;+
; NAME:
;	Profile - Extract a profile from an image.
; PURPOSE:
;	Allow the operator to mark two points on the image display
;					with the cursor/mouse/joystick.
;	The profile Line is defined by
;			using the marked points as endpoints of a Line, or,
;	if /CENTER is specified,
;		the First point defines the center of a Line,
;		second point defines one endpoint of the Line,
;	other endpoint automatically taken in equal and opposite direction.
;	Image values at points along the Line are extracted and returned .
;	Optionally return the X and Y values of each extracted point.
; CATEGORY:
;	??
; CALLING SEQUENCE:
;	Result = Profile( Image, XX, YY )
; INPUTS:
;	Image = data array.  May be any type except complex.
;		Operator then enters locations of end points of profile.
; Optional Input:
;	xx, yy = each with exactly 2 elements giving
;		Location of profile end points, then skip interactive choice.
; KEYWORD PARAMETERS:
;	XSTART, YSTART = starting (x,y) location of lower left corner of image.
;	/NOMARK to inhibit marking the image with the line.
;	MAGF = magnification of displayed image (default=1).
;	WINDOW = window number in which image is displayed (default is current)
;	/CENTER to specify center of Line and one endpoint.
;	INFO = optional window number to display instructions.
; OUTPUTS:
;	Function result = floating vector containing data points.
;	xx, yy = Locations of profile cut thru image.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	Cursor on image display is enabled.
; PROCEDURE:
;	Straightforward.  Calls function DRAW_MARK.
; MODIFICATION HISTORY:
;	Written, DMS, November, 1982.
;	Modified for Sun, march, 1988.
;	Mod, FV, 4/1990: options to specify Magnification in display Window,
;			and to specify profile cut by center and one endpoint.
;	Mod, FV, 1/1991: call function draw_mark.
;	Mod, FV, 7/1991: if XX & YY are input with 2 elements, each, then
;				skip interactive and just extract the profile.
;	Mod, FV, 1998: added keyword NPIX which return # of pixels extracted,
;		also can now handle arbitrary (out of image) xx & yy inputs,
;		in which case only the valid pixels are returned.
;-

Function profile, image, xx, yy, XSTART=x0,YSTART=y0, NOMARK=nomark, NPIX=npix,$
		CENTER=center, MAGFAC=Magf, WINDOW=image_window, INFO=info_win

s = fix( size(image) )
sx=s(1) & sy=s(2)
if n_elements(x0) le 0 then x0 = 0
if n_elements(y0) le 0 then y0 = 0

if n_elements( Magf ) eq 0 then Magf = 1  else Magf = float( Magf ) > 1
if n_elements( image_window ) ne 1 then image_window = !d.window

;on_error, 2

if (N_elements( xx ) EQ 2) AND (N_elements( yy ) EQ 2) then begin
	x1 = xx(0)
	x2 = xx(1)
	y1 = yy(0)
	y2 = yy(1)
	goto,EXTRACT
   endif

wset, image_window
wshow, image_window
tvcrs, .5, .5, /NORM

Pnt1:
	if keyword_set( center ) then $
		info = " Mark the CENTER point of the profile:" $
	else	info = ' Mark the two end points of the profile.'

	if N_elements( info_win ) EQ 1 then begin
		printw, info, /ERASE, LINE=0, WINDOW=info_win
		wset, image_window
		wshow, image_window
	  endif else print,info

	tvrdc,xx1,yy1,1,/dev	;Get first point

	y1 = fix( (yy1 - y0)/Magf )
	x1 = fix( (xx1 - x0)/Magf )

	if !order ne 0 then y1 = sy - 1 - y1    ;Invert?

	if (x1 lt 0) or (x1 ge sx) or (y1 lt 0) or (y1 ge sy) then begin
		print,'PROFILE: point outside image' + string( 7B )
		wait,.5
		goto, pnt1
	   endif

	imsav = draw_mark( xx1,yy1, 19, xsav,ysav )

	if keyword_set( center ) then print," Center: (",x1,",",y1," )" $
				 else print,' From: (',x1,',',y1,')'
pnt2:
	if keyword_set( center ) then begin
		info = " Mark END-point of the profile."
		if N_elements( info_win ) EQ 1 then begin
			printw, info, /ERASE, LINE=0, WINDOW=info_win
			wset, image_window
			wshow, image_window
		  endif else print,info
	   endif

	wait,.5			;for fast displays
	tvrdc,xx2,yy2,/dev	;2nd point.

	y2 = fix( (yy2 - y0)/Magf )
	x2 = fix( (xx2 - x0)/Magf )

	if !order ne 0 then y2 = sy - 1 - y2   ;Invert?

	if (x2 lt 0) or (x2 ge sx) or (y2 lt 0) or (y2 ge sy) then begin
		print,'PROFILE: point outside image' + string( 7B )
		wait,.5
		goto, pnt2
	   endif

	if keyword_set( center ) then begin

		xc = x1
		x2x = x2-xc
		yc = y1
		y2y = y2-yc
		slope = float( y2y ) / float( x2x )

		x1 = xc - x2x
		y1 = yc - y2y

		if (x1 LT 0) OR (x1 GT sx) then begin

			x1 = ((x1 > 0) < sx)
			y1 = fix( .5 + yc + (x1-xc)*slope )

		  endif else if (y1 LT 0) OR (y1 GT sy) then begin

			y1 = ((y1 > 0) < sy)
			x1 = fix( .5 + xc + (y1-yc)/slope )
		   endif
	   endif

	print," Endpoints: (",x1,",",y1," )   (",x2,",",y2," )"
	tv,imsav,xsav,ysav

	if not keyword_set( nomark ) then begin

		if keyword_set( center ) then begin
			xx1 = Magf*x1 + x0
			yy1 = Magf*y1 + y0
		  endif

		plots, [xx1,xx2], [yy1,yy2], /dev,/noclip      ;Draw the line
	  endif

	if N_elements( info_win ) EQ 1 then $
		printw, " ", /ERASE, LINE=0, WINDOW=info_win
EXTRACT:
	dx = abs( float( x2-x1 ) )		;delta x
	dy = abs( float( y2-y1 ) )
	n = dx > dy

	if n eq 0 then begin
		message,'Zero Length Line',/INFO
		return,0
	   endif

	if dx gt dy then begin
		if x2 ge x1 then fx=1 else fx=-1
		fy = (y2-y1)/dx
	   endif else begin
		if y2 ge y1 then fy=1 else fy=-1
		fx = (x2-x1)/dy
	   endelse

	npix = fix( n+1 )
	xx = round( indgen( npix ) * fx + x1 )	;X values, make into longwords.
	yy = round( indgen( npix ) * fy + y1 )	;Y values

	w = where( (xx GE 0) and (xx LT sx) and (yy GE 0) and (yy LT sy), npix )
	if( npix LE 0 ) then return,0

	xx = xx(w)
	yy = yy(w)

return, image( yy*sx + xx )
end
