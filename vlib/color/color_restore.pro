;+
; NAME:
;	color_restore
;
; PURPOSE:
;	Restore RGB_MAP structure variable <colors> from an IDL save file
;	and then load the color table and mapping.
;	If variable <colors> not in file then assume old type with <rc,gc,bc>.
;
; CALLING:
;	color_restore, file_name
;
; INPUTS:
;	file_name = string, name of IDL save file containing variable: colors.
;
; EFFECTS:
;	Color table of frame buffer is modified (using TVLCT).
;
; EXTERNAL CALLS:
;	pro color_st_Load
;
; COMMON BLOCKS:
;	common colors, ro,go,bo, rc,gc,bc
;	common adjct_map, color_map
;	common adjct_ramp, rampx, rampy
;	common color_options, ct_map_reset, ct_feedback
;	common color_table1, table_name
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1991
;		(code from pro color_tables, first written 1989).
;	F.V.1997, interpolate the old rc,gc,bc color arrays if needed.
;-

pro color_restore, file_name

   common colors, ro,go,bo, rc,gc,bc
   common adjct_map, color_map
   common adjct_ramp, rampx, rampy
   common color_options, ct_map_reset, ct_feedback
   common color_table1, table_name

	if strpos( strlowcase( file_name ), ".rgb" ) LT 0 then $
			file_name = file_name + ".rgb"
	on_error,2
	on_ioerror,ERROR
	restore,file_name,/VERBOSE

	if N_struct( colors ) GT 0 then begin

		color_st_Load, colors, RESET=ct_map_reset
		table_name = colors.name

		if N_elements( ct_feedback ) EQ 1 then begin
			if (ct_feedback) then begin
				ro = rc
				go = gc
				bo = bc
				print,"original = current table"
			   endif
		   endif

	  endif else begin

		if N_elements( rc ) NE !D.table_size then begin
			Ncolor = N_elements( rc )
			Last = Ncolor-1
			message,"interpolating " + strtrim( Ncolor,2 ) + $
		 		" colors to "+ strtrim( !D.table_size,2 ) + $
				" table size",/INFO
			xi = Last * findgen( !D.table_size )/( !D.table_size-1 )
			ro = interpolate( rc(0:Last), xi )
			go = interpolate( gc(0:Last), xi )
			bo = interpolate( bc(0:Last), xi )
		 endif else begin
			ro = rc
			go = gc
			bo = bc
		  endelse

		color_map = bindgen( !D.table_size )
		rampx = [ 0, !D.table_size-1 ]
		rampy = rampx
		table_name = file_name
		color_map_Load, /RELOAD
		print," Loaded colors from: ",file_name
	   endelse
return

ERROR:	message,"I/O error:",/INFO
	print,!err_string
end
