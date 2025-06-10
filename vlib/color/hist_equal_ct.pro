;+
; NAME:
;	Hist_Equal_CT
;
; PURPOSE:
;	Histogram equalize the color tables of an image or a region of display.
;
; CALLING:
;	Hist_Equal_CT, Image	... to histogram equalize from an image.
;	Hist_Equal_CT      	... to histogram equalize from a region.
;
; INPUTS:
;	Image = image whose histogram is to be used in determining
;		the new color tables.  If omitted, the user is prompted
;		to mark the diagonal corners of a region of the display.
;		The Image MUST be a byte image, scaled the same way as
;		the image loaded to the display.
; KEYWORDS:
;	WINDOW = the window number containing image (default = current window),
;		for interactively selecting region to equalize.
;
;	INFO_WINDOW = optional window number in which to
;		display information & instructions (default is print it).
;
; EFFECTS:
;	Color tables are updated.
;
; EXTERNAL CALLS:
;	pro color_map_load
;	function color_struct
;	pro color_st_Load
;
; COMMON BLOCKS:
;	colors -- the color tables are modified by pro color_map_load
;	adjctmap -- saves the color table mapping for further adjustments.
;
; PROCEDURE:
;	Either the image parameter or the region of the display marked by
;	the user is used to obtain a pixel distribution histogram.  The
;	cumulative integral is taken and scaled.  This function is applied
;	to the current color tables.
;
; HISTORY:
;	DMS, March, 1988, written.
;	modified by F. Varosi NASA/GSFC 1989  to Loop and use rubber-box
;		(box_create, box_draw, and box_erase routines required).
;	F.V.1990 use color_map_load to tvlct and save histogram eq.
;		for use in adjct, allowing further adjustments.
;	F.V.1991 use color_struct and color_st_Load to save and reset tables.
;-

pro hist_equal_ct, image, WINDOW=image_window, INFO_WINDOW=info_window

nc = !D.table_size	;# of colors in device
nc1 = nc-1

if n_elements(image) gt 0 then begin

	h = histogram(image)
	for i=1,n_elements(h)-1 do h(i) = h(i)+h(i-1)
	h = bytscl(h, top = nc1)

	color_map_load, h, nc1 

 endif else begin

	if N_elements( image_window ) NE 1 then  image_window = !D.window

	if N_elements( info_window ) EQ 1 then begin

		wset, info_window
		save_window = tvrd( 0,0, !D.x_vsize, !D.y_vsize )
		erase
		func = [ "function = Hist_Equal_CT", " " ]
		printw, func, LINE=-3
		instructions = ["LEFT & MIDDLE button:"			$
				+ " mark FIRST & SECOND corner of AREA,"   ,$
			"MIDDLE button: exit and keep new color table, "   ,$
			"RIGHT button: exit and restore orignal colors"," " ]
		printw, instructions, LINE=-5
		wshow, info_window

	  endif else begin

		print," LEFT button: mark FIRST corner of AREA," $
			+ " then SECOND corner with MIDDLE button"
		print," MIDDLE button: exit and keep new color table"
		print," RIGHT button: exit and restore orignal colors"
	    endelse

	wset, image_window
	wshow, image_window, ICON=0
	tvcrs, 0.3, 0.3, /NORM
	box_save	;in case there is an existing box on display, save it.
	colorsave = color_struct( 'saved' )
DEFBOX:
	button = box_create( x0,y0, x1,y1 )

	CASE button OF

	-2: BEGIN
		box_erase
		box_save, /RESTORE
		goto,EXIT
	     END

	-4: BEGIN
		color_st_Load, colorsave
		box_erase
		box_save, /RESTORE
		goto,EXIT
	     END

	else: BEGIN
		h = histogram( tvrd( x0+1, y0+1, x1-x0-1, y1-y0-1 ) )
		h = h(0:nc1)
		for i=1,nc1 do h(i) = h(i)+h(i-1)

		color_map_load, bytscl( h, TOP=nc-1 ), nc1 
		wait,0.5
		goto,DEFBOX
		END
	 ENDCASE
EXIT:
	if N_elements( info_window ) EQ 1 then begin
		wset, info_window
		tv, save_window
	   endif
  endelse

end
