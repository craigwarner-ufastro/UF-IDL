;+
; NAME:	
;	ZOOMWP
; PURPOSE:
;	if ZOOMW has been called , then ZOOMWP will Re-Zoom a window 
;				   with previous zoom factor and location.
; CALLING SEQUENCE:
;	Zoomwp, windo
; INPUTS: 
;	windo = any window to zoom (default is current win, over-riden by /OLD)
; KEYWORDS:
;	/OLD_ZOOM will Re-Zoom the window zoomed in previous ZOOMW call.
;	/BOX will cause box to be drawn indicating zoomed region.
;	/ICON  will just iconify the zoom window and return.
; OUTPUTS:
;	No explicit outputs.   Uses old zoom window if it exists.
; COMMON BLOCKS:
;	The window used for zoom display and zoom parameters are in ZOOMW1.
; EXTERNAL CALLS:
;	pros:	box_erase, box_draw
; PROCEDURE:
;	Straightforward.
; HISTORY:
;	written: Frank Varosi NASA/GSFC 1989.
;-

pro zoomwp, windo, OLD_ZOOM=old_zoom, ICONIFY=iconify, BOX_SHOW=box_show, $
			SHOW_ZOOM_WIN=show_win

   common zoomw1, zoom_w, zfact, zwxs, zwys, old_w, zx,zy

	if N_elements( zoom_w ) NE 1 then return
	if (zoom_w LT 0) then return

	if keyword_set( iconify ) then begin
		wshow, zoom_w, /ICON
		return
	   endif

	if keyword_set( old_zoom ) then  windo = old_w

	if N_elements( windo ) NE 1 then windo = !D.window   $
	      else  if (windo LT 0) then return

	if keyword_set( show_win ) then wshow, windo, ICON=0
	wset,windo
	x0 = (zx-zwxs/(zfact*2)) > 0 		;left edge from center
	y0 = (zy-zwys/(zfact*2)) > 0 		;bottom
	nx = (zwxs/zfact) < (!D.x_vsize - x0)	;Size of new image
	ny = (zwys/zfact) < (!D.y_vsize - y0)
	if (nx LE 1) OR (ny LE 1) then return

	box_erase
	area = tvrd( x0,y0, nx,ny )		;Read image

	if keyword_set( box_show ) then box_draw, x0-1,y0-1, x0+nx, y0+ny
	if keyword_set( show_win ) then wshow, zoom_w, ICON=0
	wset,zoom_w
	xss = nx * zfact	;Make integer rebin factors
	yss = ny * zfact
	if (xss LT zwxs) OR (yss LT zwys) then erase

	tv, rebin( area, xss,yss, /SAMPLE )
	area = 0
	wset,windo
end
