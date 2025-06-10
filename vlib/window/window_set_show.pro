;+
; NAME:
;	window_set_show
; PURPOSE:
;	Perform wset and wshow on window, first checking if valid,
;	but if not valid then create a new window.
;	Option to only check if valid, and only set+show if valid.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1990.
;	F.V.1992, mod to handle new IDL-X feature that #>31 are for /FREE only.
;	F.V.1995, mod to return if not windowing system,
;		and on Macintosh cannot check window states so skip it.
;	F.V.1998, mod to use _EXTRA keyword instead of XPOS and YPOS explicitly.
;-

pro window_set_show, winum, DELAY=delay, CHECK_VALID=check, CURSOR_IN_CENTER=cen_cursor, $
					 ZERO_BUTTON_STATUS=zero_button_status, ERASE=erase
	device,WIN=win_flags

        if winum ge N_elements( win_flags ) then return

	if NOT win_flags[winum] then begin

           if keyword_set( check ) then print," skipping window ",winum," (not valid)." else begin
              if( winum LT 32 ) then begin
                 title = "Resurrected Window #" + strtrim( winum, 2 ) + " (was deleted?)"
                 window, winum, TITLE=title, XSIZ=378, YSIZ=256
              endif
           endelse

        endif else begin

           wset, winum
           wshow, winum, ICON=0
           if keyword_set( erase ) then erase
        endelse

	if N_elements( delay ) EQ 1 then  wait, delay

	if (!D.name EQ "SUN") OR keyword_set( cen_cursor ) then begin
		if (!D.name EQ "SUN") then device,/CURSOR_CROSS
		tvcrs, 0.5, 0.5, /NORM
	   endif

	if keyword_set( zero_button_status ) then !mouse.button = 0
end
