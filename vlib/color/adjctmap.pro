;+
; NAME:
;	adjctmap
;
; PURPOSE:
;	Interactively adjusts color tables using mouse input.
;	This version is designed (by F.V.) to work with procedures:
;		color_map_Load,
;		color_struct,
;		color_st_Load,
;		hist_equal_ct,
;	by saving the color table mapping and parameters in common.
;	Also, the color bar is put on horizontal axis (more intuitive).
;
; CALLING:
;	adjctmap
;
; KEYWORDS:
;	XPOSITION = x position of interaction window on screen
;	YPOSITION = y position of interaction window on screen
;
; SIDE EFFECTS:
;	Color tables are modified (rgb_orig is mapped into rgb_curr).
;
; COMMON BLOCKS:
;	adjct_map, color_map  : keeps pixel value to color table mapping.
;	adjct_ramp, oldx, oldy  : keeps RAMP mapping parameters.
;	NOTE: common colors - color table common block (rgb_orig , rgb_curr),
;			is modified by pro color_map_Load.
;
; EXTERNAL CALLS:
;	pro menus  - to handle selection of operation types.
;	pro color_map_Load  - apply pixel value to color table mapping,
;				and load color tables (tvlct).
;	function select_number  -  called for steps option only.
;
; PROCEDURE:
;	A new window is created and a graph of the color output value
;	versus pixel value is created. User can adjust this transfer function
;	a number of ways using the mouse (explained in help option).
;
; HISTORY:
;	DMS, March, 1988, written.
;	DMS, April, 1989, modified cursor handling to use less CPU
;	Frank Varosi Oct.1989, added the STEPS option #3 (not all finished).
;	FV Nov.89, put color bar on horizontal axis (more intuitive).
;	FV Jan.90, saved color_map (=p) in common
;		  and call color_map_Load instead of tvlct (map is remembered).
;	FV Feb.90, saved oldx & oldy in common adjct_ramp (ramp is remembered).
;	FV Nov.90, use modified version of pro menus for initial choice setup.
;	FV Dec.91, apply smoothing to reduce jitter in draw curve option.
;	FV 1992, added keywords XPOS and YPOS.
;	FV 1993, changed name from adjct to adjctmap (so as not to confuse).
;	FV 2011, added options /REDRAW to refresh the color bar and WINDOW1/2.
;-

pro  adjctmap, XPOSITION=axpos, YPOSITION=aypos, REDRAW=redraw, WINDISP=windisps

  common adjct_map, color_map
  common adjct_ramp, oldx, oldy

  on_error,2                    ;Return to caller if an error occurs
  nc = !d.table_size            ;# of colors avail
  nc1 = nc-1
  if (nc eq 0) then message, 'Device has static color tables, can''t adjust'

  xsize = 400                   ;Window width
  ysize = 360
  save_p = !p
  !p.noclip=1                   ;No clipping
  !p.font=-1
  old_window = !d.window	;Previous window
  windisp = -1

  if keyword_set( redraw ) then begin
     if N_elements( windisps ) gt 0 then begin
        windisp = windisps[0]
        if( windisp ge 0 ) then begin
           wset, windisp
           windpix = tvrd( 0, 0, !D.x_size-1, !D.y_size-1 )
           device,GET_WINDOW_POS=wpos
           axpos = (wpos[0] - xsize - 30) > 10
           aypos = wpos[1] > 120
         endif
     endif
  endif

  slope = 1.0
  inter = 0.0
  big = 1.0e6                   ;Large slope
  title = 'Color Table or Intensity Transformation'
  if N_elements( axpos ) ne 1 then axpos = 200
  if N_elements( aypos ) ne 1 then aypos = 300
  window, XS=xsize, YS=ysize, XP=axpos, YP=aypos, TIT=title, /FREE
  winajct = !D.window

  tvcrs,0.5,0.5,/NORM
  choices = ["Ramp ", "Segments", "Draw ", "Steps", "Help"]

  instr =['Left = 1st endpoint, Middle = other endpoint, Rt = Done'	,$
          'Left button for 1st pnt, Middle = following, Rt = Done'	,$
          'Left button down to draw, up to load colors, Rt = Done'	,$
          'Middle = # steps, Rt = Done', ' ']

  if N_elements( color_map ) GE nc then begin
     p = float( color_map[0:nc1] )
     p[0] = p[1]
     p[nc1] = p[nc1-1]
  endif else  p = findgen(nc)

  color_map_Load, p, nc1

  xx = findgen(nc)
  plot, xx, p, XSTYLE=1, XRANGE=[0,nc], XTIT='Pixel Value', $
        YSTYLE=1, YRANGE=[0,nc], YTIT='Color Number'

  xp = !X.window * !D.x_vsize
  yp = !Y.window * !D.y_vsize
  nb = xp[1]-xp[0]
  ramp = bytscl( indgen(nb) , TOP=nc1 )
  nv = 8
  colorbar = bytarr( nb, nv )
  for i=0,nv-1 do colorbar[*,i] = ramp
  xp0 = xp[0]+1
  yp0 = yp[0]-nv+1
  yp1 = yp[1]
  tv,colorbar,xp0,yp0           ;display color bars, top and bottom.
  tv,colorbar,xp0,yp1
  menu_func = 0                 ;Output orig choices on first call to menus, will be set to =1.

  while 1 do begin              ;Main loop

     mode = menus( menu_func, choices, instr, INIT=0 ) ;Get choice
     !err = 0

     CASE mode OF

        0: BEGIN		;Ramp option
           isub = 0
           if N_elements( oldx ) LE 0 then oldx = [0,nc1]
           if N_elements( oldy ) LE 0 then oldy = [0,nc1]
           x = oldx & y = oldy
           cursor,x1,y1         ;data coords, wait

           while !err ne 4 do begin
              if !err ne 0 then begin
                 if !err eq 1 then isub = 0
                 if !err eq 2 then isub = 1
                 x(isub) = x1 & y(isub) = y1

                 if total(abs(oldx-x)+abs(oldy-y)) ne 0 then begin
                    oldx = x & oldy = y
                    x(isub) = x1 & y(isub) = y1
                    dx = x[1] - x[0]
                    dy = float(y[1] - y[0])
                    if dx ne 0 then slope = dy/dx else slope = big
                    inter = y[1] - slope * x[1]
                    plots,xx,p,col=0
                    p = long(findgen(nc) * slope + inter) > 0 < nc1
                    plots,xx,p,col=nc1
                    color_map_Load, p, nc1
                    if keyword_set( redraw ) then begin
                       tv,colorbar,xp0,yp0
                       tv,colorbar,xp0,yp1
                       color_scale,/REFRESH
                       if ( windisp ge 0 ) then begin
                          wset, windisp
                          tv, windpix
                       endif
                       wset, winajct
                    endif
                 endif
              endif
              cursor,x1,y1,/CHANGE ;Next point
           endwhile
           oldx = x & oldy = y
        END

        1: BEGIN		;Segments
           p0 = 0
           x = [0.,0.] & y=x
           n = 0
           while (!err ne 4) do begin
              cursor,x1,y1,/WAIT
              if !err eq 1 then n = 0
              if (!err and 3) ne 0 then begin
                 x1 = x1 < (nc1) > 0 & y1 = y1 < nc1 > 0
                 x(p0) = x1 & y(p0) = y1
                 dx = x(p0) - x(1-p0)
                 dy = y(p0) - y(1-p0)
                 n = n + 1
                 if (n ge 2) and (dx ne 0) then begin
                    slope = dy/dx
                    inter = y(p0) - slope * x(p0)
                    x0 = x(1-p0) < x(p0)
                    pp = (findgen(abs(dx)+1)+x0) *slope +inter
                    plots,xx,p,col=0
                    p(x0) = pp
                    plots,xx,p,col=nc1
                    color_map_Load, p, nc1
                    if keyword_set( redraw ) then begin
                       tv,colorbar,xp0,yp0
                       tv,colorbar,xp0,yp1
                       color_scale,/REFRESH
                       if ( windisp ge 0 ) then begin
                          wset, windisp
                          tv, windpix
                       endif
                       wset, winajct
                    endif
                 endif
                 p0 = 1-p0      ;Swap endpoints
              endif
           endwhile
        END	

        2: while !err ne 4 do begin ;Draw curve of mapping
           cursor,x0,y0,/WAIT       ;Get 1st point
           x0 = (x0 < nc1) > 0
           y0 = (y0 < nc1) > 0
           x0s = x0
           while (!err eq 1) do begin
              cursor,x1,y1,/NOWAIT ;Next pnt
              x1 = (x1 < nc1) > 0
              y1 = (y1 < nc1) > 0
              if (x1 ne x0) then begin ;Draw
                 i0 = fix(x0 < x1)
                 i1 = fix((x1 > x0) + 0.9999)
                 i00 = i0 - 1 > 0
                 i11 = i1 + 1 < nc1
                 xxx = xx(i00:i11)
                 plots,xxx,p(i00:i11),col=0 ;Erase old segment
                 slope = (y1 - y0) / (x1 - x0)
                 inter = y1 - slope * x1
                 p(i0) = xx(i0:i1) * slope + inter
                 plots,xxx,p(i00:i11),col=nc1
                 x0 = x1
                 y0 = y1
              endif             ;Draw
           endwhile
           i0 = fix(x0s < x0)
           i1 = fix((x0 > x0s) + 0.9999)
           if (i1 GT i0+3) then begin ;smooth out the jitters
              plots,xx,p,col=0
              p(i0) = smooth( p(i0:i1), 3 )
              if (i1 GT i0+5) then p(i0) = smooth( p(i0:i1), 5 )
              if (i1 GT i0+7) then p(i0) = smooth( p(i0:i1), 7 )
              plots,xx,p,col=nc1
           endif
           color_map_Load, p, nc1
           if keyword_set( redraw ) then begin
              tv,colorbar,xp0,yp0
              tv,colorbar,xp0,yp1
              color_scale,/REFRESH
              if ( windisp ge 0 ) then begin
                 wset, windisp
                 tv, windpix
              endif
              wset, winajct
           endif
        endwhile

        3: BEGIN
           nstep = float( select_number( "# steps?",1,14,init=10 ) )
           plots,xx,p,col=0
           m = fix( nc/(nstep+1) )
           n = nc - m
           p = (nc1/nstep) * fix( nstep * findgen( n )/n )
           p = [ p , replicate( nc1, m ) ]
           plots,xx,p,col=nc1
           tvcrs,.5,.5,/NORM
           color_map_Load, p, nc1
           if keyword_set( redraw ) then begin
              tv,colorbar,xp0,yp0
              tv,colorbar,xp0,yp1
           endif

           while (!err NE 4) do begin
              cursor,x0,y0,/WAIT ;Get 1st point
              x0 = x0 < (nc1) > 0 & y0 = y0 < nc1 > 0
              CASE !err OF
                 1: BEGIN
                 END
                 2: BEGIN
                    nstep = select_number( "# steps?",1,14,init=nstep )
                    nstep = float( nstep )
                    plots,xx,p,col=0
                    m = fix( nc/(nstep+1) )
                    n = nc - m
                    p = (nc1/nstep) * fix( nstep * findgen( n )/n )
                    p = [ p , replicate( nc1, m ) ]
                    plots,xx,p,col=nc1
                    tvcrs,.5,.5,/NORM
                 END
                 else:
              ENDCASE
              color_map_Load, p, nc1
              if keyword_set( redraw ) then begin
                 tv,colorbar,xp0,yp0
                 tv,colorbar,xp0,yp1
                 color_scale,/REFRESH
                 if ( windisp ge 0 ) then begin
                    wset, windisp
                    tv, windpix
                 endif
                 wset, winajct
              endif
           endwhile
        END

        4: BEGIN
           print,' '
           print,'All functions: Right button ends function.'
           print,'               Clicking Right button twice exits procedure.'
           print,'Ramp - Left button controls one endpoint of ramp,'
           print,'       Middle controls other endpoint.
           print,'	    Hint: Move the cursor along an axis with button depressed.'
           print,'Segments - Left button begins a new segment.'
           print,'       Middle button marks a vertex and continues a segment.'
           print,'Draw - Depressing the Left button and moving it marks a graph.'
           print,'       Releasing it updates the color tables with the curve.'
           print,'Steps - Middle button for menu selecting # steps'
           tvcrs,.5,.5,/NORM
        END

        -1: BEGIN               ;Quit entry
           wdelete              ;Done with window
           if ( windisp ge 0 ) then begin
              wset, windisp
              tvcrs,!d.x_vsize/2, !d.y_vsize/2,/dev ;Put cursor in middle
           endif else if old_window ge 0 then wset,old_window
           !p = save_p
           return
        END

        else:

     ENDCASE

  endwhile

end
