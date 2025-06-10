pro profiles, image, SX=sx, SY=sy, WSIZE_PLOT=wsize, LOG10=Log10, $
				MAGFAC=Magf, WINDOW=orig_w, INFO=info_win
;+
; NAME:		PROFILES
; PURPOSE:
;	Interactively draw row (horizontal) or column (vertical) profiles
;	of an image into a separate window.
; CATEGORY:
;	Image analysis
; CALLING SEQUENCE:
;	PROFILES, Image, sx = sx, sy = sy, WSIZE=wsize, MAGF=magf, WINDOW=windo
; INPUTS:
;	Image = image data of image displayed in current window.
;		Data need not be scaled into bytes.  I.e. it may be
;		floating.  The profile graphs are made from this array.
; KEYWORD PARAMETERS:
;	sx = Starting X of image as displayed in window,  default = (0,0).
;	sy = starting Y of displayed image.
;	Magf = magnification (integer factor) of displayed image (default=1),
;		can be 2 element vector for separate x and y magnifications.
;	Window = window number in which image is displayed (default is current)
;	Wsize = size of new window as a fraction or multiple of (512, 512).
;      /BOTH plots both Horizontal and Vertical cuts (when in Horizontal mode).
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	A new window is created and used for the profiles.  When done,
;	the new window is deleted.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	A new window is created and the mouse location in the original window 
;	is used to plot profiles in the new window.
;	Pressing the left mouse button toggles between
;	row (horizontal) or column (vertical) profiles.
;	The right mouse button exits.
; MODIFICATION HISTORY:
;	DMS, Nov, 1988.
;	Modified, FV, 1990: to specify image Magnification in display Window.
;	Modified, FV, 1991: to plot BOTH horiz. & vert. profiles.
;-
if n_elements(sx) eq 0 then sx = 0	;Default start of image
if n_elements(sy) eq 0 then sy = 0
if n_elements(wsize) eq 0 then wsize = 1

info = [' Left mouse button toggles Horizontal / Vertical profile,', $
	' Middle button to see BOTH horiz. & vert. profiles', $
	' Right mouse button to Exit.']

if N_elements( info_win ) EQ 1 then $
				printw, info, /ERASE, LINE=0, WINDOW=info_win $
		 	   else print,info

if n_elements( Magf ) eq 0 then begin
	Magfx = 1
	Magfy = 1
 endif else if n_elements( Magf ) GT 1 then begin
	Magfx = fix( Magf(0) ) > 1
	Magfy = fix( Magf(1) ) > 1
  endif else begin
	Magfx = fix( Magf ) > 1
	Magfy = Magfx
   endelse

if n_elements( orig_w ) ne 1 then orig_w = !d.window

window,/free ,xs=wsize*512, ys=wsize*512,title='Profiles' ;Make new window
new_w = !d.window

wset,orig_w
wshow,orig_w

s = size( image )
nx = s(1)				;Cols in image
ny = s(2)				;Rows in image
tvcrs, sx + Magfx*nx/2, sy + Magfy*ny/2, /DEVICE
maxv = max( image, MIN=minv ) 			;Get extrema

if keyword_set( Log10 ) then begin
	minv = 10.^( Log10 > ( fix( alog10 ( maxv ) ) -9 ) )
	Logflag=1
  endif else Logflag=0

tickl = 0.1				;Cross length
old_mode = -1				;Mode = 0 for rows, 1 for cols
mode = 0
both = 0

while 1 do begin

	wset,orig_w		;Image window
	tvrdc,x,y,2,/dev	;Read position

	if !err eq 1 then begin
		mode = 1-mode	;Toggle mode
		repeat tvrdc,x,y,0,/dev until !err eq 0
	   endif

	if !err eq 2 then begin
		both = 1-both	;Toggle option to Plot Both H & V
		repeat tvrdc,x,y,0,/dev until !err eq 0
	   endif

	x = (x - sx)/Magfx		;Remove bias
	y = (y - sy)/Magfy
	wset,new_w		;Graph window

	if !err eq 4 then begin		;Quit
		wset,orig_w
		tvcrs,nx/2,ny/2,/dev	;curs to old window
		wdelete, new_w
		if N_elements( info_win ) EQ 1 then $
			printw, [" "," "," "], /ERASE, LINE=0, WINDOW=info_win
		return
	   endif

	if mode ne old_mode then begin
		old_mode = mode
		first = 1
		if mode then begin	;Columns?
		     plot,[minv,maxv],[0,ny-1],/NODAT,TIT="Vertical Profile",$
				XTIT="pixel values", YTIT="y - pixel numbers",$
				YSTYLE=1, XTYPE=Logflag
			vecy = findgen(ny)
			crossx = [-tickl, tickl]*(maxv-minv)
			crossy = [-tickl, tickl]*ny
		end else begin
		    plot,[0,nx-1],[minv,maxv],/NODAT,TIT="Horizontal Profile",$
				XTIT="x - pixel numbers", YTIT="pixel values",$
				XSTYLE=1, YTYPE=Logflag
			vecx = findgen(nx)
			crossx = [-tickl, tickl]*nx
			crossy = [-tickl, tickl]*(maxv-minv)
		endelse
		xyouts,2,4,/DEV,"Cursor (x,y) = "
	   endif

	if (x lt nx) and (y lt ny) and $
		(x ge 0) and (y ge 0) then begin	;Draw it
		
		if first eq 0 then begin	;Erase?
			plots, vecx, vecy, col=0	;Erase graph
			if N_elements( vx ) GT 1 then $
				plots, vx,vy, LINESTYLE=1, COL=0
			plots, old_x, old_y, col=0	;Erase cross
			plots, old_x1, old_y1, col=0
			xyouts,120,4,/DEV,value,col=0	;Erase text
			empty
		  endif else first = 0

		value = strmid(x,8,4)+strmid(y,8,4)
		ixy = image(x,y)		;Data value

		if mode then begin		;Columns?
			vecx = image(x,*)	;get column
			old_x = crossx + ixy
			old_y = [y,y]
			old_x1 = [ixy, ixy]
			old_y1 = crossy + y
			vx=0
		  endif else begin
			vecy = image(*,y)	;get row
			old_x = [x,x]
			old_y = crossy + ixy
			old_x1 = crossx + x
			old_y1 = [ixy,ixy]
			if keyword_set( both ) then begin
				vy = image(x,*)
				vx = indgen(ny)-y+x
				w = where( (vx GE 0) AND (vx LT nx), nw )
				if (nw GT 0) then begin
					vx = vx(w)
					vy = vy(w)
				  endif else vx=0
			  endif else vx=0
		  endelse

		xyouts,120,4,/DEV,value	;Text of locn
		plots,vecx,vecy		;Graph
		if keyword_set( both ) AND $
		   N_elements( vx ) GT 1 then plots, vx,vy, LINESTYLE=1
		plots,old_x, old_y	;Cross
		plots,old_x1, old_y1
	   endif
endwhile
end
