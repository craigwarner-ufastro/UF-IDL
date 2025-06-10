;+
; NAME:
;	patch_surf
; PURPOSE:
;	Interactively select a circular or polygonal region in image and
;	then patch over the region by minimum curvature surface interpolation,
;	using the pixel values at edge of region to govern the interpolation.
;
; CALLING:
;	im_patched = patch_surf( image )
;
; INPUTS:
;	image = 2-D array of data.
;
; KEYWORDS:
;	/DISPLAY causes image to be scaled and displayed in a window.
;	ZOOM = zoom (magnification) factor to display image.
;	WINDOW = window number in which image is displayed (default=0).
;
;	Default is to select arbitrary polygonal region, other options:
;
;	BOX_WIDTH = width of fixed size box to patch.
;	CIRCLE_RADIUS = radius of circular region to patch.
;	/VARY_BOX_REGION allows choosing corners of an arbitrary rectangle.
;
;	EXTRA = minimum number pixels exterior to patch region to use
;		in computing interpolating surface, default =1, always >1.
;	RXY = output 2-D array: [[xmin,ymin],[xmax,ymax]] of region used
;		to compute interpolating surface, containing region patched.
; OUTPUTS:
;	Function returns a new image with region patched by interpolation,
;		other pixels are same as original image.
; EXTERNAL CALLS:
;	pro tvs
;	pro box_draw
;	function box_create
;	function defroi
;	function gridxy
;	function get_window
;	function min_curve_surf
; PROCEDURE:
;	Call function defroi (or other type) to get region to patch,
;	then get indices of exterior of patch region, and then by
;	supplying only the data in a rectangle containing the patch region, use
;	function min_curve_surf to interpolate throughout interior of region.
; HISTORY:
;	Written: Frank Varosi, NASA/GSFC 1993.
;-

function patch_surf, image, ZOOM=magf, DISPLAY_IMAGE=dispim, WINDOW=wind, $
				CIRCLE_RADIUS=circrad, BOX_WIDTH=boxwid, $
				VARY_BOX_REGION=varybox, EXTRA=extra, RXY=rxy

	sim = size( image )
	if (sim(0) NE 2) then begin
		message,"missing the image (2-D array)",/INFO
		return,0
	   endif
	nx = sim(1)
	ny = sim(2)
	one = replicate( 1b, nx, ny )

	if N_elements( xpos ) NE 1 then xpos=0
	if N_elements( ypos ) NE 1 then ypos=0
	if N_elements( xoff ) NE 1 then xoff=0
	if N_elements( yoff ) NE 1 then yoff=0
	if N_elements( Magf ) NE 1 then Magf=1

	if keyword_set( dispim ) then begin
		if N_elements( wind ) NE 1 then wind=0
		get_window, wind, XS=Magf*nx, YS=Magf*ny
		wshow, wind, ICON=0
		tvs, image, MAG=magf
	   endif

	if keyword_set( boxwid ) then begin

		cursor,x,y,/DEV,/WAIT
		if (!mouse.button EQ 4) then return,0
		boxwid = boxwid > 3
		boxh = boxwid/2
		if N_elements( boxh ) LT 2 then boxh = [boxh,boxh]
		bord = 2 + boxh
		Limit = sim(1:2) -1 -bord
		xi = ( round( (x-xpos)/Magf ) > bord(0) ) < Limit(0)
		yi = ( round( (y-ypos)/Magf ) > bord(1) ) < Limit(1)
		box_draw, RADIUS = (boxwid*Magf)/2, $
				POS = ([xi,yi]+0.5)*Magf + [xpos,ypos]
		xmin = fix( xi - boxh(0) )
		ymin = fix( yi - boxh(1) )
		xmax = fix( xi + boxh(0) )
		ymax = fix( yi + boxh(1) )
		one(xmin:xmax,ymin:ymax) = 0

	 endif else if keyword_set( varybox ) then begin

	  BOXC:	wait,0.3

		CASE box_create( xl,yl, xh,yh ) OF
		  (-2): BEGIN
				print," define box: LEFT -> MIDDLE," + $
					" or RIGHT button to quit"
				goto,BOXC
			END
		  (-4):	return,0
		  else:
		 ENDCASE

		pos0 = [xpos,ypos]
		posxy = round( ( [ xl,yl, xh,yh ] - [pos0,pos0] )/Magf )
		xmin = posxy(0) > 0
		ymin = posxy(1) > 0
		Limit = sim(1:2)-1
		xmax = posxy(2) < Limit(0)
		ymax = posxy(3) < Limit(1)
		if (xmax LE 0) then xmax = xmin+1
		if (ymax LE 0) then ymax = ymin+1
		if (xmin GE Limit(0)) then xmin = xmax-1
		if (ymin GE Limit(1)) then ymin = ymax-1
		one(xmin:xmax,ymin:ymax) = 0

	  endif else if keyword_set( circrad ) then begin

		cursor,x,y,/DEV,/WAIT
		if (!mouse.button EQ 4) then return,0
		circrad = circrad > 2
		device, GET_graphics=grold, SET_graphics=6
		arc = 2*!PI*findgen(16)/15
		xc = Magf * circrad * cos( arc ) + x
		yc = Magf * circrad * sin( arc ) + y
		plots, xc,yc, /DEV, COLOR=!D.n_colors-1, THICK=2
		device, SET_graphics=grold
		bord = circrad + [2,2]
		Limit = sim(1:2) -1 -bord
		xi = ( round( (x-xpos)/Magf ) > bord(0) ) < Limit(0)
		yi = ( round( (y-ypos)/Magf ) > bord(1) ) < Limit(1)
		xmin = fix( xi - circrad )
		ymin = fix( yi - circrad )
		xmax = fix( xi + circrad )
		ymax = fix( yi + circrad )
		onesub = one(xmin:xmax,ymin:ymax)
		diam = 2*circrad + 1
		xim = [findgen( diam ) - circrad] # replicate( 1, diam )
		yim = rotate( xim, 1 )
		wc = where( (xim*xim + yim*yim) LT circrad^2, npix )	
		onesub(wc) = 0
		one(xmin,ymin) = onesub

	   endif else begin

		print," Select a Polygonal Region:" + string(7b)
		wp = defroi( nx,ny, ZOOM=magf )
		xmin = min( wp MOD nx, MAX=xmax )
		ymin = min( wp/nx, MAX=ymax )
		one(wp) = 0

	    endelse

	if N_elements( extra ) NE 1 then extra=1
	extra = extra>1
	xmin = (xmin-extra)>0
	xmax = (xmax+extra)<(nx-1)
	ymin = (ymin-extra)>0
	ymax = (ymax+extra)<(ny-1)
	rxy = [ [xmin,ymin], [xmax,ymax] ]
	one = one(xmin:xmax,ymin:ymax)
	wnp = where( one, np )

	message,"interpolation requires inverting " + $
		strtrim( np,2 ) + " squared matrix !",/INFO

	impat = image
	imsub = image(xmin:xmax,ymin:ymax)
	sz = size( imsub )
	nxs = sz(1)
	nys = sz(2)
	gxy = gridxy( nxs, nys, xr=[0,nxs],yr=[0,nys], /VEC )

	impat(xmin,ymin) = min_curve_surf( imsub(wnp), gxy(wnp,0), $
							gxy(wnp,1), GS=[1,1] )
	if !DEBUG then stop

return, impat
end
