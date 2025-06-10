;+
;Frank Varosi NASA/GSFC 1989.
;F.V. added zoom factor = 0.5 option.
;F.V. 2011, use wmenux.
;-

function set_zoom, maxz, ozfac, INIT=init, MENU_WINDOW=menu_window, ZOOM_ONLY=zoom_only

	if N_elements( menu_window ) EQ 1 AND (!D.name EQ "SUN") then begin
		wset,menu_window
		wshow,menu_window
	   endif

        if N_elements( maxz ) ne 1 then maxz = 4
	if N_elements( init ) EQ 1 then begin
           if N_elements( ozfac ) ne 1 then ozfac = init
           initz = fix( init )
        endif else initz=1

	if keyword_set( zoom_only ) then begin

		menu = ['Zoom Factors:', strtrim( indgen(maxz)+1 ) ]
		zoom_factor = wmenux( menu, INIT=initz, TITLE=0,/NO_SEL )
                if N_elements( ozfac ) eq 1 then begin
                   if( zoom_factor LE 0 ) then return, ozfac
                endif else return, zoom_factor > 1

	  endif else begin

		menu = ['Zoom Factors:', '0.5', strtrim( indgen(maxz)+1 ) ]
		zoom_factor = wmenux( menu, INIT=initz+1, TITLE=0,/NO_SEL ) -1
                if N_elements( ozfac ) eq 1 then begin
                   if( zoom_factor LT 0 ) then return, ozfac else if(zoom_factor LE 0 ) then return,0.5
                endif else if(zoom_factor LE 0 ) then zoom_factor = 0.5
	   endelse

return, zoom_factor
end
