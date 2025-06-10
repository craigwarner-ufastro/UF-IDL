;+
; NAME:
;	probe_imag_menu
; PURPOSE:
;	Change the options/state of profile plots for pro probe_image.
; CALLING:
;	return_task = probe_imag_menu( xcrs, ycrs )
; INPUTS:
;	xcrs, ycrs = optional, Location for cursor set at return (device coord.)
;
;	All other info taken from common blocks.
;
; OUTPUTS:
;	Function returns a string telling what to do next in probe_image.
; EXTERNAL CALLS:
;	pro X_Var_Edit
;	pro probe_imag_curs
;	pro psport
;	function next_word
;	function get_words
;	function get_text_input
; PROCEDURE:
;	Loop over cursor query code.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function probe_imag_menu, xcrs, ycrs

  common probe_image0, wxsiz, wysiz
  common probe_image2, Log_prof, aver_prof
  common probe_image4, Nhc, hcinfo, fps
  common probe_image5, xy_region
  common probe_imag_menu, menu_prof, mpsiz

	if N_elements( menu_prof ) LE 1 then begin

		menu_prof = ["Profile Plot Options:"		,$
				" ","HARDCOPY"			,$
				" ","Linear/Logarithmic axis"	,$
				" ","Read profiles with cursor"	,$
				" "				,$
				"Fix probe xy-region"		,$
				"Edit xy-region"		,$
				" "				,$
				"Un-fix: keep xy-region"	,$
				"Un-fix: variable xy-region"	,$
				" ","Averaged/Summed profiles"	,$
				" ","Change plot window size"	]

		mpsiz =['500 x 500'	,$
			'500 x 700'	,$
			'700 x 500'	,$
			'700 x 700'	]
	   endif

	task = menu_prof[ wmenux( menu_prof, INIT=2, TIT=0 ) > 0 ]
	retask = ""

	CASE next_word( task ) OF

		"HARDCOPY": Begin
				hcinfo = get_text_input( "comments ?" )
				fps = "prof_" + strtrim(Nhc,2) + ".ps"
				if (N_elements( xcrs ) EQ 1) AND $
				   (N_elements( ycrs ) EQ 1) then $
					tvcrs, xcrs, ycrs, /DEV
				psport,/SQUARE, FILE = fps
				return,"PROF"
			    End

		"Change": Begin
				wsiz = fix( get_words( DELIMITER="x", $
						mpsiz[ wmenux( mpsiz,INIT=1 ) ] ))
				wxsiz = wsiz[0]
				wysiz = wsiz[1]
				retask = "PROF"
			    End

		"Read":		probe_imag_curs

		"Linear/Logarithmic": BEGIN
				Log_prof = 1 - keyword_set( Log_prof )
				retask = "PROF"
				End

		"Averaged/Summed": BEGIN
				aver_prof = 1 - keyword_set( aver_prof )
				retask = "PROF"
				End

		"Fix": BEGIN
				xy_region.mode = 1
				retask = "SKIP"
			End

		"Edit": BEGIN
				X_Var_Edit, xy_region
				xy_region.mode = 1
				retask = "SKIP"
			End

		"Un-fix:": BEGIN
				CASE next_word( task ) OF
					"variable":	xy_region.mode = 0
					"keep":		xy_region.mode = -1
				 ENDCASE
				retask = "SKIP"
			    END
		else:
	 ENDCASE

	if (N_elements( xcrs ) EQ 1) AND $
	   (N_elements( ycrs ) EQ 1) then tvcrs, xcrs, ycrs, /DEV

return, retask
end
