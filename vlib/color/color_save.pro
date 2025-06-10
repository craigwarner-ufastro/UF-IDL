pro color_save, filsav
;+
; NAME:
;	color_save
;
; PURPOSE:
;	Save RGB table and mapping as an IDL save file,
;	into subdirectory called "colors", or current directory.
;
; CALLING:
;	color_save, filsav
;
; INPUTS:
;	filsav = string, the name of file in which to save RGB table-mapping,
;		(default = "current"), and ".rgb" is added at end of name.
;
; EFFECTS:
;	File containing color-table-structure is created as "filename.rgb",
;	in subdirectory "colors" if it exists, else in current directory.
;
; EXTERNAL CALLS:
;	pro find_dir
;	pro color_struct
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1990, save as structure given by function color_struct.
;-
	if N_elements( filsav ) NE 1 then filsav = "current"

	if (filsav EQ "") then begin
		print," nothing saved"
		return
	   endif

	filnam = find_dir( "colors" ) + filsav + ".rgb"

	colors = color_struct( filsav )
	save, colors, FILE = filnam

	print," saved color table RGB_Map structure to: ",filnam
return
end
