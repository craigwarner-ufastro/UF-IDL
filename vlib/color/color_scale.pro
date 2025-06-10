;+
; NAME:	
;	color_scale
; PURPOSE:
;	Display a color bar with numerical scale in its own window,
;	or optionally in some other window (see tvs.pro for example).
; CALLING:
;	color_scale, minvalue, maxvalue, topval, scale_type, title
; INPUTS:
;	minvalue = min value (bottom) of scale.
;	maxvalue = max value (top) of scale.
;	topval = # of colors used in table, default is !D.table_size-2 .
;	scale_type = string: LINEAR or LOG10, default is do nothing.
;	title = string.
; KEYWORDS:
;	LOGMIN = number greater than zero.
;	POSITION = default is [440,100]
;	SPEC_STRUCT = optional way to pass specification in structure (see code).
;	/LARGE causes window to be created extra large with empty space.
;	WINDOW = optional window # to display color bar in (at POSITION=).
;	HISTOGRAM = optional data histogram array to plot rotated to right of scale.
;	/INVERT_COLMAP : invert the order of values in color bar.
; OUTPUTS:
;	No explicit outputs.
;	A new window is created in which color bar with scale is displayed,
;	or if keyword WINDOW=win then color bar and scale are display in win.
; COMMON BLOCKS:
;	common color_scale, xpos, ypos, scale_window, colorbar, region
; SIDE EFFECTS:
;	A window is created and then kept for next display of color scale.
; PROCEDURE:
;	Straightforward but complex.
; EXTERNAL CALLS:
;	pro printw
;	pro get_windowm
;	function N_struct
;	function unique
; HISTORY:
;	written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1996, fixed bugs, improved, added WINDOW keyword (used by pro tvs).
;	F.V. 2004, added HISTOGRAM keyword to plot histogram with scale (used by pro tvs).
;	F.V. 2008, plot if # elems. histodata > 1, and Yran=[1,nval]
;	F.V. 2011, added keyword option /REFRESH to redisplay colorbar.
;	F.V. 2012, fixed rare bugs in HISTOGRAM option, and format bug.
;	F.V. 2014, fixed mistake in HISTOGRAM option: YRANGE < !D.n_colors
;	F.V. 2018, do not change value of topval, just use it.
;	F.V. 2022: fixed the display of colormap and scaling for PS hardcopy (no histogram)
;	F.V. 2022: added keyword option /INVERT_COLMAP to invert the color map order.
;-

pro color_scale, minvalue, maxvalue, topval, scale_type, title, $
                 POSITION=position, LOGMIN=minLog, WINDOW=windo, INVERT_COLMAP=invcmap, $
                 SPEC_STRUCT=spec, REFRESH=refresh, LARGE=Large, HISTOGRAM=histodata

   common color_scale, xpos, ypos, scale_window, colorbar, cbox
   common color_scale1, xsize, ysize
   common color_scale2, cbxpos, cbypos

   if keyword_set( refresh ) then begin
      if N_elements( cbxpos ) eq 1 and N_elements( scale_window ) eq 1 then begin
         get_window, scale_window, XP=xpos, YP=ypos, XS=xsize, YS=ysize,/SHOW
         tv, colorbar, cbxpos, cbypos
      endif
      return
   endif

	if N_elements( xpos ) NE 1 then xpos = 440
	if N_elements( ypos ) NE 1 then ypos = 400
	XsizeHistoPlot = 104

	if N_struct( spec ) EQ 1 then begin
		minvalue = spec.min
		maxvalue = spec.max
		scale_type = spec.scaling
		title = spec.title
		topval = spec.topval
		minLog = spec.minLog
	   endif

	if strpos( strupcase( scale_type ), "LOG10") ge 0 then begin
           if N_elements( minLog ) NE 1 then minLog=1
           minval = aLog10( minvalue > (minLog > 1.e-37) )
           maxval = aLog10( maxvalue > (minLog > 1.e-37) )
        endif else begin
           minval = float( minvalue )
           maxval = float( maxvalue )
        endelse

	if N_elements( position ) EQ 2 then begin
		xposav = xpos
		xpos = position[0]
		yposav = ypos
		ypos = position[1]
	   endif
		
	if (xpos LT 0) OR (ypos LT 0) then return
	if N_elements( topval ) NE 1 then topval = !D.n_colors-2
	if( topval gt !D.n_colors-2 ) then topval = !D.n_colors-2
	scb = size( colorbar )

	if (scb[0] NE 2) then begin
           colorbar = transpose( byte( indgen( 256, 40 )))
           scb = size( colorbar )
        endif

        Lcb = scb[2]-1
        if( topval LT Lcb ) then scb[2] = topval + 1

        if keyword_set( invcmap ) then cbd = rotate( colorbar[*,0:topval], 2 ) $
                                  else cbd = colorbar[*,0:topval]

        if !D.name EQ "PS" then begin

           if N_elements( position ) NE 4 then position=[[0,0],[1,1]]
           region = [ [position[0:1]], [position[0:1] + position[2:3]] ]
           if max( position ) LT 2 then begin
              dr = convert_coord( region,/NORM,/TO_DEV )
              region = dr[0:1,*]
           endif
           region = transpose( region )
           region[0] = region[1] + !D.x_px_cm * 0.3
           region[1] = region[0] + !D.x_px_cm * 1.2
           tv, cbd, region[0], region[2],XSIZ=region[1]-region[0],YSIZ=region[3]-region[2],/DEV

        endif else if N_elements( windo ) EQ 1 then begin

           if (!D.flags AND 256) EQ 256 then wset, windo
           if N_elements( position ) NE 2 then position=[0,0]
           region = transpose( [ [position], [position + scb[1:2]] ] )
           cbxpos = position[0]
           cbypos = position[1]
           tv, cbd, cbxpos, cbypos

        endif else begin

           if keyword_set( Large ) then nline=12 else nline=2
           ymin = !D.y_ch_size * nline
           xmin = !D.x_ch_size/2
           cbox = [ xmin, scb[1]+xmin, ymin, scb[2]+ymin, topval]
           xsize = cbox[1] + !D.x_ch_size * 11
           ysize = cbox[3] + !D.y_ch_size * 6
           region = cbox
           if N_elements( histodata ) gt 2 then xsize = xsize + XsizeHistoPlot

           get_window, scale_window,TITLE="Color Scale",/SHOW,/ERAS,$
                       XPOS=xpos, YPOS=ypos, XSIZE=xsize, YSIZE=ysize
           cbxpos = xmin
           cbypos = ymin
           tv, cbd, cbxpos, cbypos
           in_cb_win = 1
        endelse

	if N_elements( xposav ) EQ 1 then xpos = xposav
	if N_elements( yposav ) EQ 1 then ypos = yposav
	range = maxval-minval
	crange = region[3] - region[2]

	if (minval LT 0) AND (maxval GT 0) then begin

		if (abs( minval ) GT maxval) then			$
			scalinc = 10.^floor( aLog10( abs( minval ) ) )	$
		  else  scalinc = 10.^floor( aLog10( maxval ) )
		if fix( range/scalinc ) LT 5 then scalinc = scalinc/2
		if fix( range/scalinc ) GT 14 then scalinc = 2*scalinc
		NLab = 1 + fix( maxval/scalinc )
		valp = [ scalinc * findgen( NLab ), maxval ]
		NLab = 1 + fix( abs( minval )/scalinc )
		valn = [ -scalinc * findgen( NLab ), minval ]
		vals = ( [ reverse( valn ), valp ] > minval ) < maxval
		Locs = round( crange * (vals-minval)/range ) + region[2] - !D.y_ch_size/3

	  endif else if (minval GE maxval) then begin

		vals = [ minval, maxval ]
		Locs = round( crange * [0,1] ) + region[2] - !D.y_ch_size/3

	   endif else begin

		scalinc = 10.^floor( aLog10( range ) )
		if fix( range/scalinc ) LT 5 then scalinc = scalinc/2
		if fix( range/scalinc ) GT 14 then scalinc = 2*scalinc
		NLab = 1 + fix( range/scalinc )
		vals = scalinc * findgen( NLab )
		MLab = 1 + fix( minval/scalinc )
		vals = vals + scalinc*MLab
		vals = ( vals > minval ) < maxval
		vals = [ minval, vals, maxval ] < maxval
		Locs = round( crange * (vals-minval)/range ) + region[2] - !D.y_ch_size/3
	    endelse

if !DEBUG GT 3 then stop

	NLab = N_elements( Locs )
	LLab = NLab-1
	Locmin = Locs[0]
	Locmax = Locs[LLab]
	valmin = vals[0]
	valmax = vals[LLab]

	if total( ( Locs[1:*]-Locs ) LE !D.y_ch_size ) GT NLab/2. then begin
		even = ( 2 * indgen( (NLab/2) > 2 ) ) < LLab
		Locs = Locs[even]
		vals = vals[even]
	   endif

	while ( N_elements( Locs ) GT 2 ) AND ( Locs[1]-Locs[0] LE !D.y_ch_size ) do begin
		Locs = Locs[1:*]
		vals = vals[1:*]
	  endwhile

	LLab = N_elements( Locs )-1

	while (N_elements( Locs ) GT 2) AND (Locs[LLab]-Locs[LLab-1] LE !D.y_ch_size) do begin
		Locs = Locs[0:LLab-1]
		vals = vals[0:LLab-1]
		LLab = LLab-1
	  endwhile

	Locs[0] = Locmin
	Locs[LLab] = Locmax
	vals[0] = valmin
	vals[LLab] = valmax

	u = unique( vals, nval )
	vals = vals(u)
	Locs = Locs(u)

	if (region[2] GT !D.y_ch_size * 6) and keyword_set( in_cb_win ) then  $
					printw,"________________",XOFF=0,LINE=6
	format = "(G9.3)"
	if (nval GT 1) then begin
           if min( vals[1:*] - vals ) LE abs( vals[nval-1]/100 ) then format = "(G10.4)"
        endif

	valsout = strtrim( string( vals, FORM=format ), 2 )
	x = region[1] + !D.x_ch_size/2
        if !D.name EQ "PS" then chsiz = 0.77 else chsiz = 1

	for i=0,N_elements( Locs )-1 do xyouts, x, Locs[i],/DEV, FONT=0, valsout[i],SIZE=chsiz

        if !D.name EQ "PS" then begin
           yLab = region[3]+!D.y_ch_size
           xyouts,/DEV, region[0], yLab, scale_type,SIZE=0.77
           if N_elements( title ) EQ 1 then xyouts,/DEV,region[0], yLab+!D.y_ch_size*1.2, title
           return
        endif else begin
           printw, scale_type, LINE=-4, /ERASE
           if N_elements( title ) EQ 1 then printw, title, /ERASE,SIZE=2
        endelse

        if (!DEBUG GT 2) AND !DEBUG then stop

	if N_elements( histodata ) gt 3 then begin
           nhval = N_elements( histodata )
           ihval = (nhval - 1) < !D.n_colors
           hv = indgen( nhval )
           xmin = region[1] + !D.x_ch_size * (1+max( strlen( valsout ) ))
           if N_elements( xsize ) eq 1 then xmax = xsize - 7 else xmax =  xmin + XsizeHistoPlot
           posit = [ xmin, region[2], xmax, region[3] ]
           plot, histodata, hv, PS=10, POS=posit,/DEV,/NOERASE, $
                 /XSTYLE,/YSTYLE,XTICKNAME=replicate(" ",22),YTICKNAME=replicate(" ",22), $
                 XMARG=[0,0], YMARG=[0,0], YRAN=[0,ihval], XRAN=[0,max(histodata[1:ihval-3])>1]
        endif
end
