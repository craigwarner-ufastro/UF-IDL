;+
; NAME:	
;	ZOOMW
; PURPOSE:
;	Display part of an image (or graphics) from the current window
;		expanded in another window.
;	The cursor is used to mark the center of the zoom.
; CALLING:
;	Zoom, cur_w
; INPUTS:
;	cur_w = window to zoom (optional), < 0 indicates use common ZOOMW3,
;		or if common is empty, then choose from currently open windows.
; KEYWORDS:
;	FACT = zoom expansion factor, default = 4.
;	XSIZE = X size of new window, if omitted, 500.
;	YSIZE = Y size of new window, default = 500.
;	/CONTINUOUS : keyword param which obviates the need to press the
;		left mouse button.  The zoom window tracks the mouse.
;		Only works well on fast computers.
; OUTPUTS:
;	No explicit outputs. A new window is created in which zoom is displayed.
; COMMON BLOCKS:
;	The window used for zoom display and zoom parameters are in ZOOMW1,
;		possible window sizes are in ZOOMW2,
;		other windows and names are in ZOOMW3 (for case cur_w < 0).
; EXTERNAL CALLS:
;	pros:	get_window, box_erase, box_draw, color_tables, printw
; SIDE EFFECTS:
;	A window is created and then kept for next time zoomw is used.
; PROCEDURE:
;	Straightforward.
; HISTORY:
;	This version evolved from zoom.pro in IDL library with modifications to
;	keep window, added menu items, show zoom box, etc. by
;	Frank Varosi NASA/GSFC 1989.
;	F.V. 1991, added select window to zoom, using common zoomw3.
;	F.V. 2011, use function wmenux, and improved efficiency.
;-

pro zoomw, cur_w, XSIZE=xs, YSIZE=ys, FACT=fact, CONTINUOUS=cont, INFO=info_w

   common zoomw1, zoom_w, zfact, zwxs, zwys, old_w, zx,zy
   common zoomw2, xwsizes, ywsizes, zoomwin
   common zoomw3, windows, winames

if n_elements(xs) EQ 1 then zwxs = xs
if n_elements(ys) EQ 1 then zwys = ys
if n_elements(fact) EQ 1 then zfact=fact
if keyword_set(cont) then waitflg = 2 else waitflg = 1

if n_elements( zfact ) LE 0 then zfact=4
if N_elements( xwsizes ) LE 0 then xwsizes = [300,500,300,500,800,500,800,1024,1280]
if N_elements( ywsizes ) LE 0 then ywsizes = [300,300,500,500,500,800,700, 870,1024]
if n_elements( zwxs ) NE 1 then zwxs = xwsizes[3]
if n_elements( zwys ) NE 1 then zwys = ywsizes[3]

menu = ["Zoom Options:"		,$
	"Set Zoom Factor"	,$
	"Resize Zoom Window"	,$
	"Color Tables"		,$
	"return to main menu"	]
task = ""
menu_size = ["Select Size:",strtrim( xwsizes,2) + " x " + strtrim( ywsizes,2)]

if N_elements( cur_w ) EQ 1 then begin

	if (cur_w LT 0) then begin

		if N_elements( windows ) LE 0 then begin
			device,WIN=winflags
			windos = where( winflags EQ 1, Nwin )
			if (Nwin GT 0) then begin
				winams = "# " + strtrim( windos, 2 )
			 endif else begin
				message,"no windows open",/INFO
				return
		  	  endelse
		  endif else begin
			windows = [windows]
			w = where( windows GE 0, Nwin )
			if (Nwin GT 0) then begin
				windos = windows[w]
				winams = strmid( winames[w], 0, 55 )
			   endif
		   endelse

		if (Nwin GT 0) then begin
			menuw = ["Select window to zoom:", winams]
			sel = wmenux( menuw, INIT=1, TIT=0,/NO_SEL ) -1
			if (sel GE 0) then  cur_w = windos[sel] else return
			func = [ "function = ZOOM of " + winams[sel], " " ]
		  endif else begin
			message,"no windows in table",/INFO
			return
		   endelse

	  endif else begin
		device,WIN=winflags
		if (cur_w GE N_elements( winflags )) then begin
			message,"request window is out of range",/INFO
			return
		   endif
		if (winflags(cur_w) NE 1) then begin
			message,"request window is not open",/INFO
			return
		   endif
	   endelse

  endif else begin

	cur_w = !D.window
	if (cur_w LT 0) then begin
		message,"current window is not open",/INFO
		return
	   endif
   endelse

if N_elements( info_w ) EQ 1 then begin

	printw, func, LINE=-3, /ERASE, WIN=info_w
	instructions = ["LEFT button for zoom CENTER, "	,$
			"MIDDLE button for zoom MENU, "	,$
			"RIGHT button to quit zoom"," "	]
	printw, instructions, LINE=-5, /ERASE

  endif else print," LEFT for zoom CENTER, MIDDLE for zoom MENU, RIGHT to quit"

wset, cur_w
wshow, cur_w

if N_elements( old_w ) NE 1 then begin
	zx = !D.x_vsize/2
	zy = !D.y_vsize/2
 endif else if (cur_w NE old_w) then begin
	zx = !D.x_vsize/2
	zy = !D.y_vsize/2
  endif

tvcrs, zx, zy, /DEV
old_w = cur_w
zerase=1
zshow=1

AGAIN:  cursor,/DEV, x,y, waitflg	;Wait for change (main Loop)

CASE !mouse.button OF

4:   goto,EXIT

2:   BEGIN

	sel = wmenux( menu, INIT=1, TITLE=0 )
	task = next_word( menu(sel>0) )

	CASE task OF

	"Set": BEGIN
           zfact = set_zoom( 14, INIT=zfact, /ZOOM_ONLY )
           zerase = 1
        END

	"Resize": BEGIN

		sel = wmenux( menu_size, INIT=2, TITLE=0 ) -1

		if (sel GE 0) then begin

			zwxs = xwsizes(sel)
			zwys = ywsizes(sel)

			get_window, zoom_w, TIT="Zoom Window",/ERASE,/SHOW, $
					    XSIZ=zwxs, YSIZ=zwys, XPOS=50,YPOS=100
			zoomwin = zoom_w
		   endif
		END

	"Color":    color_tables, "zoom", IMAGE=zoom_w, MENU=zoom_w, INFO=info_w

	"return":	goto,EXIT

	else:
	ENDCASE

	wset,cur_w
	wshow,cur_w
	tvcrs,x,y,/DEV	;Restore cursor
     END

else:	BEGIN

	box_erase
	zx = x					;save zoom center.
	zy = y
	x0 = (x-zwxs/(zfact*2)) > 0 		;left edge from center
	y0 = (y-zwys/(zfact*2)) > 0 		;bottom
	nx = (zwxs/zfact) < (!D.x_vsize - x0)	;Size of new image
	ny = (zwys/zfact) < (!D.y_vsize - y0)

	area = tvrd( x0,y0, nx,ny )		;Read image
	box_draw, x0-1,y0-1, x0+nx, y0+ny

	get_window, zoom_w, TIT="Zoom Window", $
                    XSIZ=zwxs,YSIZ=zwys,XPOS=50,YPOS=100,ERASE=zerase,SHOW=zshow
        zerase=0
        zshow=0
	zoomwin = zoom_w
	xss = nx * zfact	;Make integer rebin factors
	yss = ny * zfact

	tv, rebin( area, xss, yss, /SAMPLE )
        empty
	area = 0
	wset, cur_w
     END

ENDCASE

goto,AGAIN

EXIT:
	if N_elements( x0 ) EQ 1 then  box_erase

	if N_elements( info_w ) EQ 1 then begin
		printw, [" "," "," "," "], LINE=-5, /ERASE, WIN=info_w
		wset, cur_w
	   endif
end
