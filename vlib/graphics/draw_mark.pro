;+
; NAME:
;	draw_mark
; PURPOSE:
;	Draw a mark of requested symbol and diameter on graphics device.
;	Default symbol is cross, which for X or SUN device
;       is composed of foreground cross over background (darker) cross.
;	By default the image data from marked area is first read
;	(unless /NOSAVE or area is not in a window)
;	and this saved image data is returned, and then symbol is drawn.
;	Diameter is in DEVICE coordinates (max diam of mark is 256 pixels).
; CALLING:
;	imsav = draw_mark( xcur, ycur, diameter, xsave, ysave )
; INPUTS:
;	xcur, ycur = desired location of mark, in device coordinates.
;	diameter = diameter of mark in DEVICE units (max diam is 256 pixels).
; KEYWORDS:
;	SYMBOL = "circle", "box", "diamond", "cross", or "asterisk".
;		Default is "cross" with black around white center lines.
;	FORE_COLOR = center color of cross, default is white.
;	BACK_COLOR = border color of cross, default is black.
;	THICKNESS = thickness of lines drawing the mark,
;		this is always 1 (one) for window displays,
;		and default is 4 when drawing on PostScript (non windows).
;	/NOSAVE : default is save pixels in display that the mark overwrites.
;		Setting /NOSAVE skips this part, thus faster.
; OUTPUTS:
;	xsave, ysave = the location of saved window region (if NOSAVE=0).
;
;	Funtion returns window pixel values from marked region, as read by TVRD,
;	or returns integer 1 if /NOSAVE.
; EXTERNAL CALLS:
;	function rot2d
; COMMON BLOCKS:
;	common draw_mark, Hmark, Vmark, Fcol, Bcol
;	common draw_marks, circle, box, diamond, cross, asterisk
; PROCEDURE:
;	Call TV if windows and symbol = cross, else call PLOT.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1991.
;	F.V. 1991, added keyword /NOSAVE to skip the tvrd.
;	F.V. 1991, adapted for PostScript, added keywords SYMBOL & THICKNESS.
;	F.V. 1999, added print of help with syntax and MAGF keyword.
;	F.V. 2022, do not Limit thickness of marks in Xwin.
;	F.V. 2022, increased max allowed diameter to 1230 pixels.
;-

function draw_mark, xcur, ycur, diameter, xsave, ysave, SYMBOL=symbol, $
				FORE_COLOR=colf, BACK_COLOR=colb, $
				THICKNESS=thick, NOSAVE=nosave, MAGF=magf

  common draw_mark, Hmark, Vmark, Fcol, Bcol
  common draw_marks, circle, box, diamond, cross, asterisk

  if (N_elements( xcur ) gt 1) and (N_elements( xcur ) eq N_elements( ycur )) then begin
     nmark = N_elements( xcur )
     for i = 0,nmark-1 do null = draw_mark( xcur[i], ycur[i], diameter, /NOSAVE, $
                                            FORE_COLOR=colf, BACK_COLOR=colb, $
                                            THICKNESS=thick, SYMBOL=symbol, MAGF=magf )
     return, null
  endif

	if (N_elements( xcur ) NE 1) or (N_elements( ycur ) NE 1) then begin
	    print,"syntax:"
	    print,"	imsav = draw_mark( xcur, ycur, diameter, xsave, ysave )"
	    print,"or:	x = draw_mark( xcur, ycur, diameter, /NOSAVE )"
	    print,"keywords:"
	    print,'  SYMBOL="circle", "box", "diamond", "cross", or "asterisk"'
	    print,'		(default SYMBOL="cross")'
	    print,"  FORE_COLOR=, BACK_COLOR=, THICKNESS="
            return,0
         endif

	if N_elements( magf ) eq 1 then begin
		return, draw_mark( Magf * xcur, Magf * ycur, diameter, $
                                   xsave, ysave, SYMBOL=symbol, $
                                   FORE_COLO=colf, BACK_COL=colb, $
                                   THICKNESS=thick, NOSAVE=nosave )
	   endif

        if N_elements( symbol ) NE 1 then symbol="cross"
        maxdiam = 1230
	symbol = strlowcase( symbol )
	if N_elements( colf ) NE 1 then colf = !D.table_size-1
	if N_elements( diameter ) NE 1 then Msize=33 else Msize=diameter

	xwindows = (!D.name EQ "SUN") OR (!D.name EQ "X")

	if (xwindows) then begin
		if (xcur GE !D.x_vsize) OR (ycur GE !D.y_vsize) then return,(1)
		Msize = Msize < maxdiam
		ms2 = Msize/2
		xsave = (xcur - ms2) > 0
		ysave = (ycur - ms2) > 0
		if keyword_set( nosave ) then  imsave=1  else begin
			Msizx = (Msize+2) < (!D.x_vsize - xsave)
			Msizy = (Msize+2) < (!D.y_vsize - ysave)
			imsave = tvrd( xsave, ysave, Msizx, Msizy )
	  	   endelse
	  endif else begin
		imsave=1
		if N_elements( thick ) NE 1 then thick=4
	   endelse

	if (symbol EQ "cross") AND (xwindows) then begin

		if N_elements( Hmark ) LE 1 then begin
			Fcol = byte( !D.table_size-1 )
			Bcol = 0B
			Hmark = [ [ replicate( Bcol, maxdiam ) ]	,$
				  [ replicate( Fcol, maxdiam ) ]	,$
				  [ replicate( Bcol, maxdiam ) ]	]
			Vmark = transpose( Hmark )
		   endif

		if (N_elements( colf ) EQ 1) AND $
		   (N_elements( colb ) EQ 1) then begin

			if (Fcol NE colf) OR (Bcol NE colb) then begin
				Fcol = byte( colf )
				Bcol = byte( colb )
				Hmark[*,0] = Bcol
				Hmark[*,1] = Fcol
				Hmark[*,2] = Bcol
				Vmark = transpose( Hmark )
			   endif
		   endif

		Lm = Msize-1
		tv, Hmark[0:Lm,*], xsave, (ycur-1)>0
		tv, Vmark[*,0:Lm], (xcur-1)>0, ysave
		return, imsave
	   endif

	if N_elements( circle ) LE 1 then begin
		box = [ [-1, 1, 1,-1,-1],$
			[-1,-1, 1, 1,-1] ]
		diamond = [ [-1, 0, 1, 0,-1],$
			    [ 0,-1, 0, 1, 0] ]
		cross = [ [-1, 1, 0, 0, 0],$
			  [ 0, 0, 0,-1, 1] ]
		asterisk = float( cross )
		asterisk = [ [[asterisk]], [[rot2d( asterisk, 45,/DEG )]] ]
		np = 256
		np1 = np-1
		xc = 1 - (1 - 1/sqrt(2)) * findgen( np+1 )/np
		yc = sqrt( 1 - xc*xc )
		k = np-1
		xq = [ xc, rotate( yc[0:k], 2 ) ]
		yq = [ yc, rotate( xc[0:k], 2 ) ]
		k = 2*np-1
		xh = [ xq, rotate( -xq[0:k], 2 ) ]
		yh = [ yq, rotate(  yq[0:k], 2 ) ]
		k = 4*np-1
		circle = [ [ xh, rotate( xh[0:k],2 ) ], [ yh, -yh[1:*] ] ]
	   endif

	Msiz2 = Msize/2

	CASE symbol OF

	"circle": BEGIN
			Msiz2 = (2*Msiz2+1)/2.
			s = size( circle )
			nc = s(1)-1
			np = fix( 2^fix( aLog( 2*!PI*Msiz2 )/aLog(2) ) )
			np = (np > 32) < nc
			w = indgen( np+1 ) * (nc/np)
			x = circle[w,0]*Msiz2 + xcur
			y = circle[w,1]*Msiz2 + ycur
		    END

	"diamond": BEGIN
			x = diamond[*,0]*Msiz2 + xcur
			y = diamond[*,1]*Msiz2 + ycur
		     END

	"asterisk": BEGIN
			x = asterisk[*,0,0]*Msiz2 + xcur
			y = asterisk[*,1,0]*Msiz2 + ycur
			plots, x, y, /DEVICE, THICK=thick, COLOR=colf
			x = asterisk[*,0,1]*Msiz2 + xcur
			y = asterisk[*,1,1]*Msiz2 + ycur
		     END

	  "box": BEGIN
			x = box[*,0]*Msiz2 + xcur
			y = box[*,1]*Msiz2 + ycur
		   END

	   else: BEGIN					;default is cross
			x = cross[*,0]*Msiz2 + xcur
			y = cross[*,1]*Msiz2 + ycur
		   END
	  ENDCASE

	plots, x, y, /DEVICE, THICK=thick, COLOR=colf

return, imsave
end
