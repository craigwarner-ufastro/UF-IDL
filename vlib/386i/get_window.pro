;+
; NAME:
;	get_window
;
; PURPOSE:
;	Get a new window or set focus to existing window (non /FREE windows).
;	This makes repeated use of windows easier:
;	first call creates window, subsequent calls just set focus to it.
;	If window is deleted, then it is just recreated with no errors.
;	If existing window size is different then specified in keywords
;	it is recreated with requested size. If new window is requested
;	and more than 32 windows exist (max # of non /FREE windows)
;	this routine stops with request that user delete a window,
;	then continue (IDL> .c), and then new window will be created.
;
; CALLING:
;	get_window, winum
;
; INPUTS:
;	winum = 0 <= integer <= 31, window # to get, default is new window.
;		If window exists the focus is set to it.
;		If # is negative, creates new window.
;
; KEYWORDS:
;	TITLE = string, used as window title (optional),
;		default = "IDL-" + string( winum ).
;
;	XSIZE, YSIZE = size of window (optional),
;		default for new window: 640 by 512.
;
;	XPOS, YPOS = position of window on screen (optional),
;		default for new window: (400,300).
;
;	/SHOW : de-iconify and/or pop window into view.
;
; OUTPUTS:
;	winum = window # actually created or focus set to.
;
; RESTRICTIONS:
;	Works only with window numbers between 0 and 31 (inclusive),
;	not the /FREE created windows which have numbers 32 and higher.
; PROCEDURE:
;	Get the numbers of currently open windows
;	and check if in table or create a new window.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1990.
;	F.V.1992, mod to handle new IDL-X feature that #>31 are for /FREE only.
;	F.V.1995, mod to return if not windowing system,
;		and on Macintosh cannot check window states so skip it.
;-

pro get_window, winum, TITLE=title, XSIZE=xsiz, YSIZE=ysiz, $
			XPOS=xpos, YPOS=ypos, SHOW=show

	CASE !D.name OF
		"X":	device,WINDOW_STATE=win_flags
		"SUN":	device,WINDOW_STATE=win_flags
		"WIN":	device,WINDOW_STATE=win_flags
		"MAC":	win_flags = intarr(32)
		else:	return
	 ENDCASE

	if N_elements( win_flags ) GT 32 then win_flags = win_flags(0:31)
	if N_elements( xpos ) NE 1 then xpos=400
	if N_elements( ypos ) NE 1 then ypos=300
	if N_elements( xsiz ) EQ 1 then xsize=xsiz else xsize=640
	if N_elements( ysiz ) EQ 1 then ysize=ysiz else ysize=512

	if N_elements( winum ) NE 1 then begin

		w = where( win_flags EQ 0, nw )
		if (nw GT 0) then winum = w(nw-1) else goto,FULL
		if N_elements( title ) NE 1 then title="IDL-"+strtrim(winum,2)

		window, winum, TITLE=title, XSIZ=xsize, YSIZ=ysize, $
						XPOS=xpos, YPOS=ypos
	 endif else if (winum LT 0) then begin

		w = where( win_flags EQ 0, nw )
		if (nw GT 0) then winum = w(nw-1) else goto,FULL
		if N_elements( title ) NE 1 then title="IDL-"+strtrim(winum,2)

		window, winum, TITLE=title, XSIZ=xsize, YSIZ=ysize, $
						XPOS=xpos, YPOS=ypos
	  endif else begin

		if winum GE N_elements( win_flags ) then begin
			winum = -1
			get_window, winum, TIT=title, XS=xsize,YS=ysize,$
						      XP=xpos,YP=ypos,SHOW=show
			return
		   endif

		if N_elements( title ) NE 1 then title="IDL-"+strtrim(winum,2)

		if win_flags(winum) EQ 0 then begin

			window, winum, TITLE=title, XSIZ=xsize, YSIZ=ysize, $
							XPOS=xpos, YPOS=ypos

		  endif else begin

			wset,winum
			if keyword_set( show ) then wshow,winum,ICON=0

			if (N_elements( xsiz ) EQ 1) AND $
			   (N_elements( ysiz ) EQ 1) then begin
				if (xsiz NE !D.x_vsize) OR $
				   (ysiz NE !D.y_vsize) then begin
			 		window, winum, TITLE=title, $
						XP=xpos,YP=ypos,XS=xsiz,YS=ysiz
				   endif
			   endif
		   endelse
	   endelse
return

FULL:	message,"all 32 of non-FREE windows are being used!",/INFO
	stop," please delete unused windows and then: IDL> .con" + string(7b)
	get_window, winum, TITLE=title, XSIZ=xsize, YSIZ=ysize,$
				XPOS=xpos, YPOS=ypos, SHOW=show
end
