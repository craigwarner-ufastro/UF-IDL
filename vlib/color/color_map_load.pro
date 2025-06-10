;+
; NAME:
;	color_map_Load
;
; PURPOSE:
;	Apply pixel value to color index mapping, and load color tables.
;	Pixel value zero is always mapped to color # zero,
;	and max value mapped to color # Lastcolor,
;	Mapping is saved in common adjct_map, cmap.
;
; CALLING:
;	color_map_Load, color_map, Lastcolor, /RELOAD
;
; INPUTS:
;	color_map = array of indices, mapping pixel values to color numbers.
;
;	Lastcolor = optional, specifies last color index to use.
;			(default Lastcolor = !D.table_size-1)
;
; KEYWORDS:
;	/RELOAD just uses previous mapping from common adjct_map, cmap
;		so that input arg. color_map is ignored.
;
; EFFECTS:
;	R_orig, G_orig, B_orig is mapped to R_curr, G_curr, B_curr,
;	the common adjct_map, cmap, is updated,
;	the new color table and mapping is applied to window system (tvLCT).
;
; COMMON BLOCKS:
;	common colors, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
;	common adjct_map, cmap
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;-

pro color_map_load, color_map, Lastcolor, RELOAD=ReLoad

   common colors, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
   common adjct_map, cmap

	if keyword_set( ReLoad ) then goto,RELOAD

	if N_elements( Lastcolor ) NE 1 then begin
		nc = !D.table_size < N_elements( color_map )
		Lastcolor = nc-1
	   endif else Lastcolor = Lastcolor < (N_elements( color_map )-1)

	cmap = Long( color_map )
	cmap[0] = 0			;keep background black...
	cmap[Lastcolor] = Lastcolor	;Prevent invisible graphs...

	if N_elements( R_orig ) LE 0 then begin
		R_orig = bytscl( indgen( Lastcolor+1 ) )
		G_orig = R_orig
		B_orig = R_orig
	   endif
RELOAD:
	if N_elements( cmap ) LE 0 then cmap = indgen( !D.table_size )

	R_curr = R_orig[cmap]
	G_curr = G_orig[cmap]
	B_curr = B_orig[cmap]

	tvLCT, R_curr, G_curr, B_curr
end
