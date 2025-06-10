pro hardcopy_window, window

   common hardcopy_window, Nhc, psgrey_map
   common adjct_map, color_map

	wset, window
	image = tvrd( 0,0, !D.x_vsize, !D.y_vsize )

	s = size( image )
	print," stored window in image of size:",fix( s(1:2) )
	fileps = ""

	menu_PSmode = [	"Select type of HardCopy:"	,$
			"grey scale (32 Levels)"	,$
			"color (256 pseudo-colors)"	]

	sel = wmenux( menu_PSmode, INIT=1, TIT=0 )
	if (sel LE 0) then return

	PS_mode = next_word( menu_PSmode[ sel ] )

	Magf = select_number( "Magnification of output ?",1,4,INIT=1 )	
	Nxsect = Magf
	Nysect = Magf

	save_devnam = !D.name
	save_p = !P
	set_plot,"PS"

	CASE PS_mode OF

	"grey": BEGIN
		if N_elements( color_map ) LE 0 then $
					color_map = bindgen( !D.table_size )
		Ncol = N_elements( color_map )
		psgrey_map = reverse( bytscl( indgen( Ncol ) ) )
		image = psgrey_map( image )
		device, COLOR=0, BITS=8
	     END

	"color": BEGIN
		print," enter color graphics FILE NAME,"
		print," or enter nothing to take default name"
		read," :",fileps
		fileps = strtrim( fileps, 2 )
		device, /COLOR, BITS=8
	     END
	ENDCASE

	device,/INCH, XOFF=xoff(mode), YOFF=yoff(mode), XSIZ=xsiz, YSIZ=ysiz

	if (fileps EQ "") then begin
		if N_elements( Nhc ) NE 1 then Nhc = 0
		Nhc = Nhc+1
		fileps = "window" + strtrim( Nhc,2 )
	   endif

	fileps = fileps + "-" + PS_mode + ".ps"
	print," writing graphics to disk file: " + fileps
	print," this will take a few seconds..."
	!P.font = 0
	device, FILE=fileps

	image_sections, image, Nxsect, Nysect, ix, Lx, iy, Ly

	for k=0,N_elements( ix )-1 do begin

		erase
		tv, image( ix(k):Lx(k) , iy(k):Ly(k) )
		empty
	  endfor

	device,/CLOSE
	print_graphics, fileps, PS_mode
	set_plot,save_devnam
	!P = save_p
return
end
