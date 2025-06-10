pro delete_window, win_specs

   common zoomw1, zoom_w, zfact, zwxs, zwys, old_w, x,y
   common histog_plot, plot_window, minval, maxval, maxhd
   common color_scale, xpos, ypos, scale_window, colorbar, region

	menu = [ "Select window to delete:" ]

	if N_elements( zoom_w ) EQ 1 then begin
		if (zoom_w GE 0) then  menu = [ menu, "zoom window" ]
	   endif

	if N_elements( scale_window ) EQ 1 then begin
		if (scale_window GE 0) then  menu = [ menu, "color scale" ]
	   endif

	if N_elements( plot_window ) EQ 1 then begin
		if (plot_window GE 0) then  menu = [ menu, "histogram plots" ]
	   endif
	
	if N_struct( win_specs ) GT 0 then begin
		ww = where( win_specs.windo GE 0, nw )
		if (nw GT 0) then menu = [ menu, win_specs[ww].winame ]
	   endif

	if N_elements( menu ) LE 1 then begin
		print," none to delete"
		return
	  endif else menu = [menu,"none"]

	sel = wmenux( menu, INIT=1, TIT=0,/NO_SEL )

	if (sel LT 1) then begin
		print," none selected"
		return
	   endif

	winame = next_word( menu[sel] )

	CASE winame OF

	"zoom": BEGIN
			wdelete, zoom_w
			zoom_w = -zoom_w -1
		  END

	"color": BEGIN
			wdelete, scale_window
			scale_window = -scale_window -1
		   END

	"histogram": BEGIN
			wdelete, plot_window
			plot_window = -plot_window -1
		     END

	else: BEGIN
		if N_struct( win_specs ) GT 0 then begin

			w = where( [win_specs.winame] EQ menu(sel), nw )

			if (nw GT 0) then begin
				wdelete, win_specs[w[0]].windo
				win_specs[w[0]].windo = -1
			  endif else print," nothing selected"

		   endif else print," nothing selected"
		END
	ENDCASE
return
end
