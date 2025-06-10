function select_min_Log, current_value, ZERO_SHIFT=zero_shift, MENU=use_menu

;Frank Varosi NASA/GSFC 1991.

	if keyword_set( zero_shift ) then begin

		if N_elements( current_value ) NE 1 then current_value = 0
		value = strtrim( current_value, 2 )

		if keyword_set( use_menu ) then begin

			menu = ["Select ZERO SHIFT for Log10 scale:"	,$
				"     0"				,$
				"     1"				,$
				"    10"				,$
				"   100"				,$
				"  1000"				,$
				" 10000"				,$
				"take default = " + value		,$
				"enter new value"			]

			sel = wmenux( menu, INIT=7, TIT=0 )
			value = next_word( menu[ sel>0 ] )

		  endif else sel=-1

		if (value EQ "enter") then begin

			input = ""
			read,"enter new MIN >0 for Log:",input
			if (input NE "") then return, float( input )

		  endif else if (value NE "take") AND $
				(sel GT 0) then return, float( value ) $
					   else return, current_value

	  endif else begin

		if N_elements( current_value ) NE 1 then current_value = 1
		value = strtrim( current_value, 2 )

		if keyword_set( use_menu ) then begin

			menu = ["Select MIN for Log10 scale:"	,$
				"      .001"			,$
				"      .01"			,$
				"      .1"			,$
				"     1"			,$
				"    10"			,$
				"   100"			,$
				"  1000"			,$
				" 10000"			,$
				"take default = " + value	,$
				"enter new value"		]

			sel = wmenux( menu, INIT=9, TIT=0 )
			value = next_word( menu[ sel>0 ] )

		  endif else sel=-1

		if (value EQ "enter") then begin

			input = ""
			read,"enter new MIN >0 for Log:",input
			if (input NE "") then return, float( input ) > 1.e-33

		  endif else if (value NE "take") AND $
				(sel GT 0) then return,float( value ) > 1.e-33 $
					   else return, current_value > 1.e-33
	   endelse
end
