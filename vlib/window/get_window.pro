;+
; NAME:
;	get_window
;
; PURPOSE:
;	Get a new window or set focus to existing window (non /FREE windows).
;	This makes repeated use of windows easier:
;	first call creates window, subsequent calls just set focus to it.
;	If window was deleted, then it is just recreated with no errors.
;	Window size and position values are the defaults of IDL,
;	unless specified with the usual window routine keywords.
;	If existing window size is different then specified in keywords
;	it is recreated with requested size.
;       If keywords XISIZE or YISIZE are used then window size is
;       specified only at creation.
;       If new window is requested
;	and more than 32 windows exist (max # of non /FREE windows)
;	this routine stops with request that user delete a window,
;	then continue (IDL> .c), and then new window will be created.
;
; CALLING:
;	get_window, winum
;
; INPUTS:
;	winum = 0 <= integer <= 31, the window # to get, default is new window.
;		If window exists the focus is set to it.
;		If # is negative, creates new window.
;
; KEYWORDS:
;
;	XSIZE, YSIZE = size of window (optional), default for new window: 640 by 512.
;
;	XISIZE, YISIZE = initial size of window, not used for an existing window.
;
;	XMSIZE, YMSIZE = minimum size of new window or an existing window.
;
;	/SHOW : de-iconify and/or pop window into view.
;	/ERASE : erase contents of an existing window.
;
;	Standard window keywords such as TITLE, XPOS, and YPOS
;	are passed to window call via _EXTRA keyword mechanism.
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
;	F.V.1998, mod to use _EXTRA keyword instead of XPOS and YPOS explicitly.
;	F.V.2009 @ UFastro: added keywords for various sizing options.
;-

pro get_window, winum, _EXTRA=extra, ERASE=erase, SHOW=show, RECREATE=recreate, $
                XSIZE=xksiz, YSIZE=yksiz, $
                XISIZE=xisiz, YISIZE=yisiz, $
                XMSIZE=xmsiz, YMSIZE=ymsiz

  CASE !D.name OF
     "X":	device,WINDOW_STATE=win_flags
     "SUN":	device,WINDOW_STATE=win_flags
     "WIN":	device,WINDOW_STATE=win_flags
     "MAC":	win_flags = intarr(32)
     else:	return
  ENDCASE

  if N_elements( win_flags ) GT 32 then win_flags = win_flags[0:31]
  if N_elements( winum ) NE 1 then winum = -1

  if N_elements( xisiz ) EQ 1 then xsize = xisiz else begin
     if N_elements( xmsiz ) EQ 1 then xsize = xmsiz else begin
        if N_elements( xksiz ) EQ 1 then xsize = xksiz else xsize = 640
     endelse
  endelse

  if N_elements( yisiz ) EQ 1 then ysize = yisiz else begin
     if N_elements( ymsiz ) EQ 1 then ysize = ymsiz else begin
        if N_elements( yksiz ) EQ 1 then ysize = yksiz else ysize = 512
     endelse
  endelse

  if (winum LT 0) then begin

     w = where( win_flags EQ 0, nw )
     if (nw GT 0) then winum = w[nw-1] else goto,FULL

     window, winum, XSIZ=xsize, YSIZ=ysize, _EXTRA=extra

  endif else begin

     if winum GE N_elements( win_flags ) then begin
        winum = -1
        get_window, winum, XSIZ=xksiz, YSIZ=yksiz, XIS=xisiz, YIS=yisiz, $
                    SHOW=show, _EXTRA=extra, ERASE=erase, XMS=xmsiz, YMS=ymsiz
        return
     endif

     if win_flags[winum] EQ 0 then begin

        window, winum, XSIZ=xsize, YSIZ=ysize, _EXTRA=extra

     endif else begin   ;; an existing window

        if keyword_set( recreate ) then wdelete,winum else begin
           wset,winum
           if keyword_set( show ) then wshow,winum,ICON=0
           if keyword_set( erase ) then erase
        endelse

        if (N_elements( xmsiz ) EQ 1) OR (N_elements( ymsiz ) EQ 1) then begin
           if N_elements( xmsiz ) ne 1 then xmsiz = !D.x_vsize
           if N_elements( ymsiz ) ne 1 then ymsiz = !D.y_vsize
           if (xmsiz GT !D.x_vsize) OR (ymsiz GT !D.y_vsize) then begin
              window, winum, XSIZ=xmsiz>!D.x_vsize, YSIZ=ymsiz>!D.y_vsize, _EXTRA=extra
           endif
           return
        endif

        if (N_elements( xksiz ) EQ 1) OR (N_elements( yksiz ) EQ 1) then begin
           if N_elements( xksiz ) ne 1 then xksiz = !D.x_vsize
           if N_elements( yksiz ) ne 1 then yksiz = !D.y_vsize
           if (xksiz NE !D.x_vsize) OR (yksiz NE !D.y_vsize) then begin
              window, winum, XSIZ=xksiz, YSIZ=yksiz, _EXTRA=extra
           endif
        endif

     endelse
  endelse

return

FULL:	message,"all 32 of non-FREE windows are being used!",/INFO
	stop," please delete unused windows and then: IDL> .con" + string(7b)
        get_window, winum, XSIZ=xksiz, YSIZ=yksiz, XIS=xisiz, YIS=yisiz, $
                    SHOW=show, _EXTRA=extra, ERASE=erase, XMS=xmsiz, YMS=ymsiz

end
