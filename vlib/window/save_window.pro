;+
; NAME:
;	save_window
;
; PURPOSE:
;	Read pixel values from a graphics window and store with colors
;	to a file, in either IDL/XDR format or in GIF format.
;	Inquires user to choose format and enter file name.
;
; CALLING:
;	save_window, window
;
; INPUTS:
;	window = # of window to read, default is window in which cursor is.
;
; KEYWORDS:
;	FILE = string, file name to which image is written,
;		default is to inquire user.
;	/GIF : write in GIF format.
;	/IDL_XDR : write in IDL/XDR format, default is to inquire user.
;
; EXTERNAL CALLS:
;	function read_window
;	function get_text_input
;	function color_struct
;	pro write_gif
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;-

pro save_window, window, FILE=filsav, GIF=gif, TIFF=tiff, IDL_XDR=idl_xdr

	image = read_window( window )

	s = size( image )
	if (s(0) LT 2) then return
	print," window pixel values stored in array of size:",fix( s(1:2) )

	if keyword_set( tiff ) then filtyp = "TIFF" else $
	if keyword_set( gif ) then filtyp = "GIF" else $
	if keyword_set( idl_xdr ) then filtyp = "IDL/XDR" else begin
		menu = ["Format of output file ?","IDL/XDR","GIF","TIFF"]
		filtyp = menu[ wmenux( menu,INIT=1,TIT=0 ) > 1 ]
	   endelse

	if N_elements( filsav ) NE 1 then begin
		filsav = get_text_input( "enter file name for window image:" )
		if strlen( filsav ) LE 0 then return
	   endif

	tvlct, rc,gc,bc,/GET

	if (filtyp EQ "TIFF") then begin
		filsav = filsav + ".tiff"
		write_tiff, filsav, image, 0, RED=rc,GREEN=gc,BLUE=bc
	 endif else if (filtyp EQ "GIF") then begin
		filsav = filsav + ".gif"
		write_gif, filsav, image, rc,gc,bc
	  endif else begin
		colors = color_struct( filsav )
		filsav = filsav + ".window"
		save, image, colors, rc,gc,bc, FILE=filsav, /XDR, /VERBOSE
	   endelse

	print," window image and colors saved to: ",filsav
end
