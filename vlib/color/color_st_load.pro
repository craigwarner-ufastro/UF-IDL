;+
; NAME:
;	color_St_Load
;
; PURPOSE:
;	Use info in structure {RGB_MAP} to Load color Lookup tables.
;	Intended to work with result of function color_struct.
;
; CALLING:
;	color_St_Load, color_st
;
; INPUTS:
;	color_st = scalar structure variable having structure {RGB_MAP},
;		containing all color table and mapping information.
;		(as returned by function color_struct).
;
; KEYWORDS:
;	/RESET : the ADJCT mapping is reset to be one-to-one.
;
; EFFECTS:
;	The common colors, Rorig,Gorig,Borig, loaded with table from color_st,
;	the common adjct_map, color_map, is loaded with mapping from color_st,
;	the common adjct_ramp, rampx, rampy, is set with saved ramp-map,
;		the new color table and mapping is applied to window system.
;
; EXTERNAL CALLS:
;	pro color_map_Load
;
; COMMON BLOCKS:
;	common colors, Rorig,Gorig,Borig, Rcur,Gcur,Bcur
;	common adjct_map, color_map
;	common adjct_ramp, rampx, rampy
;
; PROCEDURE:
;	Interpolate if color_st.Ncolor is different from !D.table_size.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1990.
;-

pro color_St_Load, color_st, RESET_MAPPING=reset

   common colors, Rorig,Gorig,Borig, Rcur,Gcur,Bcur
   common adjct_map, color_map
   common adjct_ramp, rampx, rampy

	if N_struct( color_st ) NE 1 then begin
		print,"syntax:	color_St_Load, color_struct"
		return
	   endif

	Last = color_st.Ncolor-1
	if (Last LE 1) then return
	message,"Loading color table: " + color_st.name,/INFO

	if (color_st.Ncolor NE !D.table_size) then begin
		message,"interpolating " + strtrim( color_st.Ncolor,2 ) + $
		  " colors to "+ strtrim( !D.table_size,2 ) +" table size",/INF
		xi = Last * findgen( !D.table_size )/( !D.table_size-1 )
		Rorig = interpolate( color_st.Red(0:Last), xi )
		Gorig = interpolate( color_st.Green(0:Last), xi )
		Borig = interpolate( color_st.Blue(0:Last), xi )
		fac = float( !D.table_size )/color_st.Ncolor
		color_map= round( interpolate( color_st.Maprgb(0:Last),xi)*fac )
		rampx = round( color_st.rampx * fac )
		rampy = round( color_st.rampy * fac )
	  endif else begin
		Rorig = color_st.Red(0:Last) 
		Gorig = color_st.Green(0:Last) 
		Borig = color_st.Blue(0:Last)
		color_map = color_st.Maprgb(0:Last)
		rampx = color_st.rampx 
		rampy = color_st.rampy
	   endelse

	if keyword_set( reset ) then begin
		color_map = bindgen( !D.table_size )
		rampx = [ 0, !D.table_size-1 ]
		rampy = rampx
	  endif else begin
	   endelse

	color_map_Load, /RELOAD
end
