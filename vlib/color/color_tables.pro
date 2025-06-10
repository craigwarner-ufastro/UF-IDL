;+
; NAME:
;	color_tables
;
; PURPOSE:
;	Provide interactive menu choice access to all IDL color table options.
;
; CALLING:
;	color_tables, caller
;
; INPUTS:
;	caller = string (optional), to indicate in menu what to return to.
;
; KEYWORDS (all optional):
;
;	IMAGE_WINDOW = window number in which data or image is displayed.
;	INFO_WINDOW = window number in which to write information & instructs.
;	INIT_SEL = initial menu selection desired.
;	MENU_WINDOW = for SunView only, window number associated with menu.
;
; EFFECTS:
;	New color tables and/or mappings are applied to window system.
;
; EXTERNAL CALLS:
;	pros:	adjCTmap	color_edit	palette
;		color_map_Load	color_save	color_restore	Hist_Equal_CT
;	function VarType
;	function wmenux
;
; COMMON BLOCKS:
;	common adjCT_map, color_map
;	common adjCT_ramp, rampx, rampy
;	common color_options, CT_map_reset, CT_feedback
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V.1990, work with adjCTmap and Hist_Equal_CT using common blocks.
;	F.V.1991, use RGB_Map structure given by function color_struct.
;	F.V.1995, mod for IDLv4 change of colors1.tbl location.
;	F.V.1996, use Varosi's wmenux instead of old IDL instrinsic wmenu.
;	F.V.2011, use new options of adjctmap and color_scale,/REFRESH.
;-

pro color_tables, caller, IMAGE_WINDOW=image_window, MENU_WINDOW=menu_window, $
			  INFO_WINDOW=info_window, INIT_SEL=sel

   common color_table0, CT_num
   common color_table1, table_name
   common color_table2, menu_idlct
   common adjCT_map, color_map
   common adjCT_ramp, rampx, rampy
   common color_options, CT_map_reset, CT_feedback

	if N_elements( CT_map_reset ) NE 1 then CT_map_reset = 0
	if N_elements( CT_feedback ) NE 1 then CT_feedback = 0
	if N_elements( CT_num ) NE 1 then CT_num = 3
	if N_elements( caller ) NE 1 then caller = "Calling Routine"
	if N_elements( menu_window ) NE 1 then  menu_window = !D.window
	if N_elements( image_window ) LE 0 then  image_window = !D.window
        imwin = image_window[0]

	if (menu_window LT 0) then begin
		if (!D.window GE 0) then menu_window = !D.window $
				    else menu_window = 0
	   endif

	menu_coltab = [ "Color Table Options:"		,$
			" "				,$
			"return to " + caller		,$
			" "				,$
			"Load IDL colors"		,$
			" "				,$
			"Load RGB colors (personal)"	,$
			"save RGB colors (personal)"	,$
			" "				,$
			"adjust color table"		,$
			"histogram equalize color table",$
			" "				,$
			"edit RGB palette"		,$
			"create RGB colors"		]

	if (!D.name EQ "SUN") then menu_coltab = [ menu_coltab, " "	,$
					"pause  (click mouse to resume)"]

	if N_elements( sel ) NE 1 then sel = 4
	BELL = string( 7b )
	LF = string( 10b )

	if N_elements( menu_idlct ) LE 1 then begin	;Get color table names

		if (!version.arch EQ '386i') then begin

			openr,Lun, !DIR + "/" + "colors.tbl", /get_Lun
			ctrec = assoc( Lun, bytarr(32) )
			menu_idlct = ["Select color table:", $
					strtrim( indgen(16), 2 ) + " : "]
			for i=1,16 do menu_idlct[i] = menu_idlct[i] + $
							string( ctrec[i-1] )
		  endif else begin

			if since_version( '4' ) then begin
				idl_col_tbl = filepath( 'colors1.tbl', $
						SUBDIR=['resource', 'colors'] )
			 endif else idl_col_tbl = filepath( 'colors1.tbl' )

			openr, Lun, idl_col_tbl, /BLOCK,/GET_LUN
			ntables = 0b
			readu, Lun, ntables
			names = bytarr( 32, ntables )
			point_Lun, Lun, ntables * 768L + 1    ;Read table names
			readu, Lun, names
			names = strtrim( names, 2 )
			menu_idlct = ["Select color table:", $
					strtrim( indgen(ntables), 2 ) + " : "]
			for i=1,ntables do menu_idlct[i] = menu_idlct[i] + names[i-1]
		   endelse

		free_Lun,Lun
		menu_idlct = [menu_idlct, "adjust color table"	  ,$
					  "return to options menu",$
					  "return to " + caller	  ]
	   endif

MENU:	if (!D.name EQ "SUN") then window_set_show, menu_window, DELAY=.1

	sel = wmenux( menu_coltab, INIT=sel, TITLE=0, /NO_SEL )
	if (sel LE 0) then return

	request = menu_coltab(sel)
	task = next_word( request )

	CASE task OF

	"adjust":	adjCTmap, XPOS=200, YPOS=200,/REDRAW,WIND=image_window

	"create":	color_edit

	"edit":		palette

	"Load": BEGIN

	    what = next_word( request )

	    CASE what OF

	    "IDL": BEGIN
			ctm = wmenux( menu_idlct, INIT=CT_num+1, TIT=0, $
							/NO_SEL, ML=44 )
			while (ctm GT 0) do begin

				request = menu_idlct[ctm]
				Load = next_word( request )

				CASE Load OF

				"adjust": adjCTmap, XPOS=200, YPOS=200,/REDRAW,WIND=image_window

				"return": BEGIN
						ret = get_words( request )
						CASE ret[1] OF
						"options":	goto,MENU
						     else:	return
						ENDCASE
					    END
				else: BEGIN
					table_name = request
					CT_num = fix( Load )
					LoadCT, CT_num
					if (CT_map_reset) then begin
					    color_map = indgen( !D.table_size )
						rampx = [ 0, !D.table_size-1 ]
						rampy = rampx
					  endif else color_map_Load,/RELOAD
                                        refresh_window, imwin
                                        color_scale,/REFRESH
					END
				ENDCASE

				wait,0.3
				ctm = wmenux( menu_idlct, INIT=CT_num+1, $
							TIT=0, /NO_SEL, ML=44 )
			  endwhile
		       END

	    "RGB": BEGIN
			if VarType( filrgb ) NE "STRING" then begin

				find_files, filrgb, rgbnams, FILEXT=".rgb",  $
							     DIR="colors"

				if VarType( filrgb ) NE "STRING" then $
				     find_files, filrgb, rgbnams, FILEXT=".rgb"

				if VarType( filrgb ) NE "STRING" then begin
					message,"No RGB files found"+BELL,/INFO
					wait,1
					goto,MENU
				   endif

				menu_RGB = ["Select RGB file:", rgbnams	,$
					    " "				,$
					    "adjust color table"	,$
					    "return to options menu"	,$
					    "return to " + caller	]
				sfm = 1
			   endif

			nf = N_elements( filrgb )
			sfm = wmenux( menu_RGB, INIT=sfm, TIT=0, /NO_SEL,ML=33 )

			while (sfm GT 0) do begin

				request = menu_RGB(sfm)
				Load = next_word( request )

				CASE Load OF

				    "adjust": adjCTmap, XPOS=200, YPOS=200,/REDRAW,WIND=image_window

				    "return": BEGIN
						ret = get_words( request )
						CASE ret[1] OF
						   "options":	goto,MENU
						        else:	return
						  ENDCASE
					      END

				    else: BEGIN
					if (sfm LE nf) then begin
						color_restore, filrgb[sfm-1]
                                                refresh_window, imwin
                                                color_scale,/REFRESH
						wait,0.3
					   endif
					END
				  ENDCASE

				sfm = wmenux(menu_RGB,INIT=sfm,TIT=0,/NO_S,ML=33)
			  endwhile
		       END
		ENDCASE
	     END

	"save": BEGIN
			filnam = ""
			read," enter file name for saving RGB colors: ",filnam
			color_save, filnam
			filrgb = 0
		  END

	"histogram": BEGIN

		if N_elements( image_window ) GT 1 then begin

		    w = where( image_window GE 0, nw ) > 0

		    if (nw LE 1) then  imwin = image_window[w[0]]  else begin
			menu = ["Select a window:", $
				"# " + strtrim( image_window[w], 2 ) ]
			imwin = image_window( w[[wmenux( menu,INIT=1,TIT=0 )-1] >0 ] )
		      endelse

		  endif else imwin = image_window

		device, WIN=winflags

		if (imwin GE 0) AND $
		   (imwin LT N_elements( winflags )) then begin

			if winflags[imwin] then begin

				Hist_Equal_CT, WINDOW=imwin, INFO=info_window
                                refresh_window, imwin
                                color_scale,/REFRESH

			  endif else begin

				message,"image window is closed"+BELL,/INFO
				wait,1
			   endelse

		  endif else begin

			message,"image window NOT available"+BELL,/INFO
			wait,1
		   endelse
		END

	"pause": BEGIN
			window_set_show, menu_window
			cursor, x,y, /DEV
		   END

	"return":	return

	else:

	ENDCASE

   goto,MENU
end
