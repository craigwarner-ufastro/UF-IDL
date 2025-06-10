;+
; NAME:
;	cursor_scale
; PURPOSE:
;	Interactively select min-max on a color bar using mouse-cursor,
;	or input range/levels by typing.
; CALLING:
;	range = cursor_scale( minval, maxval, scaling )
; INPUTS:
;	minval, maxval, scaling
; KEYWORDS:
;	MAXLIM=
;	MINLIM=
;	INITIAL_MARKS=
;	LOGMIN=
;	/INCREMENT
; OUTPUTS:
;	mosaic_spec, NLev, Levels
; EXTERNAL CALLS:
;	pro color_scale
;	pro window_set_show
; PROCEDURE:
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1990.
;	F.V. 1991, added keyword /INCREM to enter Min & Increment.
;	F.V. 1993, modified /INCREM option to get & return sequece of Levels.
;	F.V. 1997, added 0.3 sec wait before cursor query.
;	F.V. 1997, added option to read contours from a file (*.levels).
;	F.V. 2011, use function get_text_input for new Threshold entry.
;-

function cursor_scale, minval, maxval, scaling, MAXLIM=maxlim, MINLIM=minlim, $
						INITIAL_MARKS=init_mark, $
						LOGMIN=minLog, INCREMENT=increm

   common color_scale, xpos, ypos, scale_window, colorbar, region
   common cursor_scale, cmin_y, cmax_y

	if N_elements( scaling ) NE 1 then scaling = "Linear"
	topval = !D.table_size-2

	if N_elements( scale_window ) NE 1 then begin
           color_scale, minval,maxval,topval,scaling,"",POS=[470,100],/LARGE
	endif else if (scale_window LT 0) then begin
           color_scale, minval,maxval,topval,scaling,"",POS=[470,100],/LARGE
        endif else begin
           device,WIN=win_flags
           if (win_flags(scale_window) NE 1) then $
              color_scale, minval,maxval,topval,scaling,"",POS=[470,100],/LARG
        endelse

	CASE strupcase( scaling ) OF

	 "LOG10": BEGIN
			if N_elements( minLog ) NE 1 then minLog=1
			minv = aLog10( minval > (minLog > 1.e-37) )
			maxv = aLog10( maxval > (minLog > 1.e-37) )
		    END

	    else: BEGIN
			minv = minval
			maxv = maxval
		    END
	 ENDCASE

	window_set_show, scale_window,/CURSOR_IN_CENTER
	LF = string( byte( 10 ) )
	BELL = string( byte( 7 ) )
	print, LF + " LEFT button to set MIN,  MIDDLE button to set MAX,  " $
							+ "RIGHT to finish"
	xmin = region[0]
	xmax = region[1]
	xran = xmax-xmin
	ymin = region[2]
	ybot = ymin-!D.y_ch_size
	ymax = region[3]
	ytop = ymax+!D.y_ch_size
	yran = ymax-ymin
	topval = region[4]
	range = maxv - minv

	yenter= fix( !D.y_ch_size*2.5 )
	tv, replicate( !D.table_size-1, !D.x_vsize, yenter )
	printw,["CLICK HERE to","ENTER on KEYB"],LINE=0,COL=0
	printw,["LEFT = MIN","MIDDLE = MAX","RIGHT = finish"],LINE=3
	if N_elements( minlim ) EQ 1 then printw,"original MIN",LINE=7
	if N_elements( maxlim ) EQ 1 then printw,"original MAX",LINE=-3

	if N_elements( init_mark ) EQ 2 then begin
		cmax_y = yran * (init_mark[1] - minv)/range
		cmin_y = yran * (init_mark[0] - minv)/range
	   endif

	if N_elements( cmin_y ) NE 1 then cmin_y = 0
	if N_elements( cmax_y ) NE 1 then cmax_y = yran
	cmin_y = (cmin_y > 0) < yran
	cmax_y = (cmax_y > 0) < yran

	cmin_v = range * cmin_y/yran + minv
	cmax_v = range * cmax_y/yran + minv
	markmin = replicate( byte( cmin_y ), xran,3 )
	markmax = replicate( byte( cmax_y ), xran,3 )
	tv, (markmin + topval/2) MOD topval, xmin, cmin_y + ymin-1
	tv, (markmax + topval/2) MOD topval, xmin, cmax_y + ymin-1

	print,"     Thresholds: MIN=" + string( cmin_v, FORM="(G10.3)" )+ $
              "   MAX=" + string( cmax_v, FORM="(G10.3)" )
	!mouse.button = 0

	while (!mouse.button LE 2) do begin

           wait,0.3
           cursor,/DEV,x,y

           if (y LE yenter ) AND (!mouse.button LE 2) then begin

              tv,colorbar,xmin,ymin
              print," "
              text = ""

              if keyword_set( increm ) then begin
                INPUT:
                 print," enter Minimum, Increment, and # intervals,"
                 print," or enter 4 or more contour Levels,"
                 print," or enter filename (*.levels assumed)" + BELL
                 read,text
                 words = get_words( text )

                 if N_elements( words ) eq 1 then begin
                    if strlen( words[0] ) LE 0 then goto,INPUT
                    print," reading Contour Levels from file:  "+ words[0] +".levels"
                    on_ioerror,ERROR
                    openr,Lun,/GET_LUN, words[0] + ".levels"
                    Line = ""
                    readf,Lun,Line
                    Levels = float( get_words( Line ) )
                    if !DEBUG then begin
                       help,Line,Levels
                       print,Levels
                    endif
                    while NOT eof( Lun ) do begin
                       readf,Lun,Line
                       if strlen( Line ) gt 0 then $
                          Levels = [ Levels, float( get_words( Line ) ) ]
                       if !DEBUG then begin
                          help,Line,Levels
                          print,Levels
                       endif
                    endwhile
                    free_Lun,Lun
                    help,Levels
                    print,Levels
                    return, Levels

                   ERROR:
                    if N_elements( Lun ) eq 1 then free_Lun,Lun
                    print," "
                    print, !ERR_STRING + string( 7b )
                    print,"...try again..."
                    wait,1
                    goto,INPUT
                 endif

                 if N_elements( words ) LT 3 then goto,INPUT

;; this is case of user entering exact desired contour Levels:
                 if N_elements( words ) GT 3 then return, float( words )

                 com = "vec = [" + strconcat( words + "," )  + " 0 ]"
                 if NOT execute( com ) then goto,INPUT
                 cmin_v = vec[0]
                 inc = vec[1]
                 Ninc = round_off( vec[2] ) > 1
                 print,[cmin_v,inc,Ninc]
                 if (inc LE 0) then begin
                    print," must enter positive increment!"
                    goto,INPUT
                 endif
                 cmax_v = cmin_v + Ninc * inc
                 cmax_y = yran*(cmax_v - minv)/range
                 cmin_y = yran*(cmin_v - minv)/range
                 return,[ cmin_v, inc, Ninc ]

              endif else begin

                 !mouse.button = 3
                 defrange = strconcat( [cmin_v,cmax_v] )
           RETRY:
                 cvran = float( get_words( get_text_input("enter MIN,MAX for Thresholds",$
                                                          DEF=defrange,WIN=scale_window)))
                 if N_elements( cvran ) ge 2 then begin
                    if( cvran[1] LE cvran[0] ) then begin
                       ans = yes_no_menu(" must enter MIN < MAX", WIN=scale_window )
                       goto,RETRY
                    endif else begin
                       cmax_v = cvran[1]
                       cmin_v = cvran[0]
                       cmax_y = yran*(cmax_v - minv)/range
                       cmin_y = yran*(cmin_v - minv)/range
                    endelse
                 endif
              endelse
           endif

           CASE !mouse.button OF

		1: BEGIN
			tv, markmin, xmin, cmin_y + ymin-1

			if (y LT ybot) AND (N_elements( minlim )) then begin
				y = ymin
				cmin_v = minlim
				if strupcase( scaling ) eq "LOG10" then $
					cmin_v = aLog10( cmin_v > 1e-37 )
				print," MIN reset to:",cmin_v,FORM="(/,A,G10.3)"
				cmin_y = y-ymin
			  endif else begin
				y = ( (y > ymin) < ymax ) < (cmax_y + ymin-2)
				cmin_y = y-ymin
				cmin_v = range * cmin_y/yran + minv
			   endelse

			markmin = replicate( byte( cmin_y ), xran,3 )
			tv, (markmin + topval/2) MOD topval, xmin, y-1
		     END
		2: BEGIN
			tv, markmax, xmin, cmax_y + ymin-1

			if (y GT ytop) AND (N_elements( maxlim )) then begin
				y = ymax
				cmax_v = maxlim
				if strupcase( scaling ) eq "LOG10" then $
					cmax_v = aLog10( cmax_v > 1e-37 )
				print," MAX reset to:",cmax_v,FORM="(/,A,G10.3)"
				cmax_y = y-ymin
			  endif else begin
				y = ( (y > ymin) < ymax ) > (cmin_y + ymin+2)
				cmax_y = y-ymin
				cmax_v = range * cmax_y/yran + minv
			   endelse

			markmax = replicate( byte( cmax_y ), xran,3 )
			tv, (markmax + topval/2) MOD topval, xmin, y-1
		     END
		else:
	     ENDCASE

	     print, " new Thresholds: MIN=" + $
			string( cmin_v, FORM="(G10.3)" ) + "   MAX=" + $
			string( cmax_v, FORM="(G10.3)" )
	  endwhile

	printw,replicate(" ",6),LINE=0,XOFF=0,/ERASE

return, [ cmin_v, cmax_v ]
end
