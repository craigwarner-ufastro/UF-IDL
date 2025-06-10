;+
; NAME:
;	probe_image
;
; PURPOSE:
;	Interactively select box Location/size with cursor (Left mouse button),
;	compute and optionally print, mean, st.dev. of image pixels in box.
;	Options include variable box/rectangle and display of
;	integrated X & Y profiles as stacked plots. (Quit with right button).
;
; CALLING:
;	probe_image, image, xi,yi, box_size, mean, stdev, npix
;
; INPUTS:
;	image = 2D array
;
; OUTPUTS:
;	xi, yi = Location of box that was analyzed (lower left corner).
;	box_size = size of box that was analyzed.
;	mean = the mean of pixel values in box region.
;	stdev = the standard deviation of pixel values in box region.
;	npix = # of pixels in image region.
;
; KEYWORDS:
;	BOX_WIDTH = size of box, 1 or 2 integers.
;	RADIUS = optional way to specify box in terms of it radius (or radii).
;	WINDOW = # of window in which image is displayed, default = current.
;	MAGF = magnification/reduction of displayed image, default = 1.
;	XPOS, YPOS = position of displayed image in window, default = (0,0).
;	XOFF, YOFF = optional offset of image in some bigger image,
;			used when printing pixel locations only, default=(0,0).
;
;	/VARIABLE_BOX : allow user to create a rubber-band box with
;			Left - Middle mouse button combination marking corners.
;	/DOUBLE_BOX : box is drawn as 2 rectangles,
;			outer is dark, (min color index),
;			inner is bright (max color index),
;			to help assure visibility against any background.
;	/CONTINUOUS : cursor location is read continuously, no-wait for click.
;	/FWHM_PRINT : print the Full-Width-Half-Max of data in box.
;	/PROFILES : average box of data in X & Y projections and make 2 plots.
;	/SHOWINDOW : make image display window visible.
;	/DISPLAY_IMAGE : first scale and display the image in a window.
;	/LOG10 : use Log base 10 scaling.
;	/PRINT : causes mean and st.dev. to be printed, default = 0,
;		if PRINT=2 then pixel values in box are also printed.
;		If /LOOP then results are always printed.
;
; EXTERNAL CALLS:
;	pro box_draw
;	pro box_erase
;	function box_create	(if /VARIABLE_BOX)
;	pro tvs		(if /DISPLAY)
;	pro get_window	(if /PROFILES or /DISPLAY)
;	function VarType
;	function FullWid_HalfMax	(if /FWHM_PRINT or /PROFILES)
;	function probe_imag_menu
; COMMON BLOCKS:
;	common probe_image(n)  where  n = blank, 0, ..., 5.
; PROCEDURE:
;	Loop over cursor query code.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;	F.V. 1993, profile plots option.
;	F.V. 1994, added options for profile plot: see function probe_imag_menu.
;-

pro probe_image, image, xi,yi, box_size, mean, stdev, npix, CONTINUOUS=contin, $
			PROFILES=profiles, FWHM_PRINT=fwhmpr, PRINT=print, $
			XPOS=xpos, YPOS=ypos, XOFF=xoff, YOFF=yoff, MAGF=Magf, $
			WINDOW=winum, DISPLAY_IMAGE=dispim, SHOWINDOW=showin, $
			BOX_WIDTH=box_width, RADIUS=radius, LOG10=Log10, $
			VARIABLE_BOX=varybox, DOUBLE_BOX=dbox, CURSET=curset

  common probe_image, winprof
  common probe_image0, wxsiz, wysiz
  common probe_image1, image_subset, xmin, ymin
  common probe_image2, Log_prof, aver_prof
  common probe_image3, save_P, save_X, save_Y
  common probe_image4, Nhc, hcinfo, fps
  common probe_image5, xy_region

	sim = size( image )

	if (sim(0) NE 2) then begin
		message,"first arg. must be an image (2-D array)",/INFO
		return
	   endif

	if N_elements( curset ) NE 1 then curset=1
	if N_elements( wxsiz ) NE 1 then wxsiz = 500
	if N_elements( wysiz ) NE 1 then wysiz = 500
	if N_elements( xpos ) NE 1 then xpos=0
	if N_elements( ypos ) NE 1 then ypos=0
	if N_elements( xoff ) NE 1 then xoff=0
	if N_elements( yoff ) NE 1 then yoff=0
	if N_elements( Magf ) NE 1 then Magf=1
	if N_elements( contin ) NE 1 then contin=0
	if N_struct( xy_region ) NE 1 then $
		xy_region = { x_min:0, y_min:0, x_size:100, y_size:100, mode:0 }

	if keyword_set( dispim ) then begin

		if N_elements( winum ) EQ 1 then begin
			if N_elements( Magf ) EQ 1 then begin
				sw = sim(1:2) * Magf + [xpos,ypos]
				window, winum, XS=sw(0), YS=sw(1)
			  endif else begin
				wset, winum
				wshow, winum, ICON=0
			   endelse
		   endif
		tvs, image, xpos, ypos, MAG=Magf, LOG10=Log10

	  endif else if keyword_set( showin ) AND $
			(N_elements( winum ) EQ 1) then begin

		if (winum GE 0) then begin
			wset, winum
			wshow, winum, ICON=0
		  endif else begin
			message,"invalid window number, cannot set/show",/INFO
			return
		   endelse
	   endif

	if keyword_set( varybox ) then begin
		print," "
		if (xy_region.mode EQ 1) then begin
			message,"in FIXED box mode:",/INFO
			print," click LEFT for box, or MIDDLE for menu"
		 endif else begin
			message,"in VARIABLE box mode:",/INFO
			print," define box with LEFT then MIDDLE buttons,"
			print," or MIDDLE button for menu, or RIGHT to quit."
		  endelse
		if N_elements( Nhc ) NE 1 then Nhc = 1
	  endif else begin
		if N_elements( box_width ) GT 0 then box_size = box_width
		if N_elements( radius ) GT 0 then box_size = 2*radius+1
		if N_elements( box_size ) LT 1 then box_size = 5
		box_size = box_size > 3
		boxh = box_size/2
		if N_elements( boxh ) LT 2 then boxh = [boxh,boxh]
	   endelse

	if keyword_set( curset ) then tvcrs,0.5,0.5,/NORM

REPEAT BEGIN

	if (xy_region.mode EQ 1) then begin

		wait,0.3
		cursor,x,y,/DEV

		CASE !mouse.button OF

			4:	return

			2: BEGIN
				CASE probe_imag_menu(x,y) OF
					"PROF":		goto,PROF
					"SKIP":		goto,SKIP
					else:
				 ENDCASE
			     END

			else: BEGIN
				END
		 ENDCASE

	 endif else if keyword_set( varybox ) then begin

	  BOXC:	wait,0.3

		CASE box_create( xl,yl, xh,yh, DOUBLE=dbox ) OF

		  (-2): BEGIN

			if keyword_set( profiles ) AND $
			  (N_elements( winprof ) EQ 1) AND $
			  (N_elements( image_subset ) GT 1) then begin

				cursor,x,y,/DEV,/NOWAIT

				CASE probe_imag_menu(x,y) OF
					"PROF":		goto,PROF
					"SKIP":		goto,SKIP
					else:
				 ENDCASE

			   endif else print," define box: LEFT -> MIDDLE"

			goto,BOXC

			END

		  (-4):	return

		  else:
		 ENDCASE

		pos0 = [xpos,ypos]
		posxy = round( ( [ xl,yl, xh,yh ] - [pos0,pos0] )/Magf )
		xmin = posxy(0) > 0
		ymin = posxy(1) > 0
		Limit = sim(1:2)-1
		xmax = posxy(2) < Limit(0)
		ymax = posxy(3) < Limit(1)
		if (xmax LE 0) then xmax = xmin+1
		if (ymax LE 0) then ymax = ymin+1
		if (xmin GE Limit(0)) then xmin = xmax-1
		if (ymin GE Limit(1)) then ymin = ymax-1

	  endif else begin

		if (NOT contin) then wait,0.3
		cursor,x,y,/DEV, CHANGE=(contin EQ 1), WAIT=(contin EQ 0)
		if (!mouse.button EQ 4) then return
		Limit = sim(1:2) -1 -boxh
		xi = ( round( (x-xpos)/Magf ) > boxh(0) ) < Limit(0)
		yi = ( round( (y-ypos)/Magf ) > boxh(1) ) < Limit(1)
		if keyword_set( dbox ) then begin
			box_erase2
			box_draw2, RADIUS = (box_size*Magf)/2, $
				POS = ([xi,yi]+0.5)*Magf + [xpos,ypos]
		 endif else begin
			box_erase
			box_draw, RADIUS = (box_size*Magf)/2, $
				POS = ([xi,yi]+0.5)*Magf + [xpos,ypos]
		  endelse
		xmin = fix( xi - boxh(0) )
		ymin = fix( yi - boxh(1) )
		xmax = fix( xi + boxh(0) )
		ymax = fix( yi + boxh(1) )
		if keyword_set( print ) then $
			print, [xi+xoff,yi+yoff], $
				FORM="($,x,'(',i5,',',i5,')',x)"
	   endelse

	CASE xy_region.mode OF
		0: BEGIN
			xy_region.x_min = xmin
			xy_region.y_min = ymin
			xy_region.x_size = xmax - xmin +1
			xy_region.y_size = ymax - ymin +1
		     END
		1: BEGIN
			Limit = sim(1:2)-1
			Limb = Limit-2
			xmin = ( xy_region.x_min > 0 ) <  Limb(0)
			ymin = ( xy_region.y_min > 0 ) <  Limb(1)
			xmax = (xy_region.x_size + xmin -1) < Limit(0)
			ymax = (xy_region.y_size + ymin -1) < Limit(1)
			if keyword_set( dbox ) then begin
				box_erase2	
				box_draw2, POS=[xmin,ymin]*Magf + [xpos,ypos], $
				   SIZE=[xy_region.x_size,xy_region.y_size]*Magf
			 endif else begin
				box_erase	
				box_draw, POS=[xmin,ymin]*Magf + [xpos,ypos], $
				   SIZE=[xy_region.x_size,xy_region.y_size]*Magf
			  endelse
		     END
		else:
	 ENDCASE

	image_subset = image(xmin:xmax,ymin:ymax)
	sisub = size( image_subset )
	nx = sisub(1)
	if (sisub(0) EQ 2) then ny = sisub(2) else ny=1

PROF:	imtot = total( image_subset )
	npix = N_elements( image_subset )
	mean = imtot/npix
	stdev = sqrt( total( (image_subset - mean)^2 ) / (npix-1) )
	sisub = size( image_subset )

	if keyword_set( profiles ) AND (sisub(0) EQ 2) then begin

		impy = total( image_subset, 1 )
		impx = total( image_subset, 2 )
		nx = N_elements( impx )
		ny = N_elements( impy )

		if (!D.name EQ "X") OR (!D.name EQ "SUN") then begin
			wincur = !D.window
			get_window, winprof, TIT="Profiles", $
				XS=wxsiz, YS=wysiz, XP=10, YP=900-wysiz
			wshow, winprof, ICON=0
			save_pinfo = 1
			if N_struct( save_P ) LT 2 then begin
				save_P = replicate( !P, 2 )
				save_X = replicate( !X, 2 )
				save_Y = replicate( !Y, 2 )
			   endif
		   endif else save_pinfo = 0

		if keyword_set( aver_prof ) then begin
			impx = impx/ny
			impy = impy/nx
			action = "averaged"
			minLog = 0.01
		 endif else begin
			action = "summed"
			minLog = 1
		  endelse

		miny = min( impy, MAX=maxy )
		minx = min( impx, MAX=maxx )

		tity = "Y profile (X " + action + ")    FWHM =" + $
	 	 string( FullWid_HalfMax( impy-miny,/GAUS,/SKY ),FORM="(F7.1)" )
		titx = "X profile (Y " + action + ")    FWHM =" + $
		 string( FullWid_HalfMax( impx-minx,/GAUS,/SKY ),FORM="(F7.1)" )
		!P.multi = [0,0,2]

		if keyword_set( Log_prof ) then begin
			if (ny GT 1) then plot, impy, /XSTY, TIT=tity, $
						/YTYPE, YRAN=[minLog,maxy]
			if (save_pinfo) then begin
				save_P(0) = !P
				save_X(0) = !X
				save_Y(0) = !Y
			   endif
			if (nx GT 1) then plot, impx, /XSTY, TIT=titx, $
						/YTYPE, YRAN=[minLog,maxx]
			if (save_pinfo) then begin
				save_P(1) = !P
				save_X(1) = !X
				save_Y(1) = !Y
			   endif
		  endif else begin
			if (ny GT 1) then plot, impy, /XSTY, TITLE=tity
			if (save_pinfo) then begin
				save_P(0) = !P
				save_X(0) = !X
				save_Y(0) = !Y
			   endif
			if (nx GT 1) then plot, impx, /XSTY, TITLE=titx
			if (save_pinfo) then begin
				save_P(1) = !P
				save_X(1) = !X
				save_Y(1) = !Y
			   endif
		   endelse

		!P.multi = 0
		if (VarType( image_subset,/CODE ) LE 3) then format = "(5i10)" $
							else format = "(5G10.3)"
		header = "    corner          box-size" + $
			"       Mean    St.Dev.      Min       Max     Total"

		stats = "("+string( [xmin+xoff,ymin+yoff],FORM="(2i6)" )+" )"+$
			" ("+string( [nx,ny],FORM="(2i5)" )+" )" + $
			string( [ mean, stdev ], FORM="(5G10.3)" ) + $
			string( [ min( image_subset, MAX=maxi ), $
						maxi, imtot ], FORM=format )

		if (!D.name EQ "X") OR (!D.name EQ "SUN") then begin
			printw, header, LINE=1
			printw, stats, LINE=0
			if VarType( profiles ) EQ "STRING" then $
				printw, profiles, LINE= 1 +wysiz/2/!D.y_ch_size
			wset,wincur
		  endif else begin
			xyouts, 0.1, -0.05, header, /NORM
			xyouts, 0.1, -0.1, stats, /NORM
			if VarType( profiles ) EQ "STRING" then $
				xyouts, 0.1, 1.1, profiles, /NORM, SIZE=1.1
			xyouts, 0.0, -0.17, /NORM, "coments:  " + hcinfo
			xyouts, 0.0, -0.22, /NORM, systime()
			psclose
			Nhc = ( Nhc+1 ) MOD 20
			print_graphics, fps
		   endelse
	   endif

	if keyword_set( fwhmpr ) then begin
		print," "
		print," FWHM =", FullWid_HalfMax( image_subset, PEAK=pxy,/SKY,$
						  GAUSSIAN_FIT=(fwhmpr EQ 2), $
						  LORENTZIAN_FIT=(fwhmpr EQ 3) )
		if N_elements( pxy ) EQ 2 then $
			print," peak Loc =", pxy + [xmin,ymin]
	   endif

	if keyword_set( print ) then begin

	   if (nx GE 100) OR (ny GE 100) then format = $
	   '(2x,"mean=",G9.3,2x,"stdev=",G7.2,2x,"sum[",i3,"x",i3,"]=",G9.3)' $
	   else format = $
	     '(2x,"mean=",G9.3,2x,"stdev=",G7.2,2x,"sum[",i2,"x",i2,"]=",G9.3)'

	   print, mean, stdev, nx,ny, imtot, FORM=format

	   if (print GE 2) AND (sisub(0) EQ 2) then begin
		js = sisub(2)-1
		print, format="(4x,7i10)", indgen(sisub(1)) + xmin + xoff
		jy = ymin+yoff
		for j = js, 0, -1 do  $
			print, j+jy, image_subset(*,j), FORM="(i4,7G10.3)"
	     endif
	 endif
SKIP:

   ENDREP UNTIL (!mouse.button EQ 4)
end
