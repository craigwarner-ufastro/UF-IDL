function color_struct, name
;+
; NAME:
;	color_struct
;
; PURPOSE:
;	Save current info about color Lookup tables and ADJCT mapping
;	into structure {RGB_MAP} and return it.
;	Info in structure {RGB_MAP} can then be saved with pro color_save,
;	or used to Load color Lookup tables by calling pro color_St_Load.
;
; CALLING:
;	colors = color_struct( name )
;
; INPUTS:
;	name = optional string to name the color table (default is null).
;
; RESULT:
;	Returns structure {RGB_MAP}, containing all color table and
;	mapping information. Saves a max of 256 colors.
;
; COMMON BLOCKS:
;	common colors, Rorig,Gorig,Borig, Rcur,Gcur,Bcur
;	common adjct_map, color_map
;	common adjct_ramp, rampx, rampy
;	common color_struct, defined
;
; PROCEDURE:
;	Define the structure and insert current color table mapping.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1990.
;-
   common colors, Rorig,Gorig,Borig, Rcur,Gcur,Bcur
   common adjct_map, color_map
   common adjct_ramp, rampx, rampy
   common color_struct, defined

	if N_elements( defined ) NE 1 then begin

		colors = { RGB_MAP,	Name:"",		$
					Ncolor:0,		$
					Red:    bytarr(256),	$
					Green:  bytarr(256),	$
					Blue:   bytarr(256),	$
					Maprgb: bytarr(256),	$
					rampx:  intarr(2),	$
					rampy:  intarr(2)	}

		defined = N_tags( colors )
		message,"defined structure {RGB_MAP}",/INFO

	  endif else  colors = replicate( {RGB_MAP}, 1 )

	if N_elements( name ) EQ 1 then colors.name = name

	Ncolor = ( !D.table_size < N_elements( Rorig ) ) < 256
	colors.Ncolor = Ncolor

	if (Ncolor LE 0) then begin
		colors.name = "undefined"
		return,colors
	   endif
		
	L = Ncolor-1
	colors.Red(0:L)   = Rorig(0:L)  
	colors.Green(0:L) = Gorig(0:L)
	colors.Blue(0:L)  = Borig(0:L)

	if N_elements( color_map ) GE Ncolor then begin

		colors.Maprgb(0:L)  = color_map(0:L)

	  endif else begin

		colors.Maprgb(0:L) = indgen( Ncolor )
	   endelse

	if N_elements( rampx ) EQ 2 then begin
		colors.rampx =  rampx
		colors.rampy =  rampy
	  endif else begin
		colors.rampx =  [0,L]
		colors.rampy =  [0,L]
	   endelse

return, colors
end
