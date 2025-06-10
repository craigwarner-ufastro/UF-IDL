;+
; NAME:
;	tvs
; PURPOSE:
;	Scale and display an image, with options to magnify/reduce, rotate,
;	take Log base 10, etc. (see keywords), and display scale on color bar.
;	If input image array is actually a stack or matrix of images,
;	the images are displayed in rows & columns.
;	If device is PostScript, can specify X & Y SIZE in DEVICE or NORMAL.
; CALLING:
;	tvs, image, xp, yp
; INPUTS:
;	image = 2D, 3D, or 4D array (single image, stack or matrix of images).
;	xp = horizontal position in window, device units. default = 0.
;	yp = vertical position in window, device units. default = 0.
; KEYWORDS:
;	MAGF = display magnification of image, integer if greater than one,
;		floating point if less than one (display reduction),
;		and default is selected to fill the current window.
;	MINVAL = default is use the minimum of image.
;	MAXVAL = default is use the maximum of image.
;	NSIGMA_RANGE = # of standard deviations around mean of image,
;			to use as minimum & maximum range for scaling,
;			used only of non-zero, overrides MINVAL and MAXVAL.
;	/ZSCALE : use the zscale_range( image ) algorithm to obtain MIN and MAX.
;	/ERASE : erase window before displaying image(s).
;	TOPVAL =  max color # in lookup table, default = !D.table-size-2.
;	BOTTOM_VAL = min color # in lookup table for display, default = 0.
;	LOG10 = display aLog10( image > LOG10 ), default is Linear.
;	POWER = display image^POWER (default=1).
;	ROTATION = integer, 0 to 7, passed to the IDL rotate function (def=0).
;	SMOOTH = box car width of moving average, default=0 = no smoothing.
;	/ALL_PIXELS : apply smooth to all pixels by extending image first.
;	/ITERATE : apply smooth(*,3) for (width-1)/2 times, and this iteration
;		approaches convolution by Gaussian of FWHM=sqrt( (width-1)/2 ).
;	MEDIAN = box width of median filter, default=0.
;	SIGMA_FILTER = box size of function sigma_filter applied to image.
;	/COLOR_BAR_SCALE : display in separate window a color-bar with scale.
;	COLOR_BAR_SCALE=2 : display color-bar scale next to image (right side).
;	NAME = string, optional, name to put in color-bar window.
;	XSIZE, YSIZE = x & y size of image displayed on PostScript output.
;	/NORMALIZED : for PostScript, assumes sizes & positions are
;			in normalized coordinates, this is the default,
;			so to use device coordinates set NORMALIZED=0.
;	/INVERT_COLMAP : invert the order of values in colors, usually for PS hardcopy.
; EXTERNAL CALLS:
;	function scale_image
;	pro color_scale
; PROCEDURE:
;	To display stack of images, call tvs for each, with staggered positions.
; HISTORY:
;	Frank Varosi NASA/GSFC 1992.
;	F.V. 1996, option COLOR_BAR=2 to display color bar scale next to image.
;	F.V. 1996, check for PostScript mode & use keywords X-YSIZE, /NORMAL.
;	F.V. 2005: added histogram of value to plot in color bar.
;	F.V. 2008: use my function histo interface to histogram.
;	F.V. 2016: fixed keyword COL=2 option: needs empty and short wait.
;	F.V. 2017: added median filter keyword, and keyword to specify window for display.
;	F.V. 2020: compate maximum magnification Limit to avoid accidental memory hogging.
;	F.V. 2021: added keyword SIGMA_FILTER= box size of function sigma_filter.
;	F.V. 2022: added keyword /INVERT_COLMAP to invert the color map order.
;	F.V. 2022: added keyword /WHITEBACKG for PS hardcopy to make white background.
;	F.V. 2023: generalized NAME= to handle vector of strings corresponding to images.
;-

pro tvs, image, xpin, ypin, MAGF=Magf, REDF=Redf, MINVAL=minv,MAXVAL=maxv,WINDOW=dwin,$
         TOPVAL=topval, BOTTOM_VAL=botv, VERTICAL=vert, HORIZONTAL=horiz, ZSCALE=zscale,$
         GAP=gap, NAME=name, ERASE=erw, RANGE_MEDIAN=ranmed, LOOPWAIT=Loopwait, $
         LOG10=Logmin, POWER=pow, ROTATION=rotidx, MEDIAN=medfbox, SIGMA_FILTER=sigfilt,$
         SMOOTH=smooth, ALL_PIXELS=all, ITERATE=iterate, ANG_ROT=rotang,SIZENAME=namesize,$
         COLOR_BAR_SCALE=cbars, NSIGMA_RANGE=nsigma, INVERT_COLMAP=invcmap, ABSVAL=absval,$
         XSIZE=xsize,YSIZE=ysize, NORMALIZED=normcoor,NBIN_CBAR=nbin, WHITEBACKG=whiteback

  szim = size( image )

  if (szim[0] LT 2) then begin
     message,"need an image or data cube",/INFO
     print," "
     print,"syntax:   tvs, image, xp, yp, MAGF= , MINVAL= , MAXVAL=, WINDOW="
     print,"other keywords: LOG10= ,  POWER= ,  ROTATION= , REDF=, /ERASE,"
     print,"                SMOOTH= ,  ALL_PIXELS= ,  ITERATE= , MEDIAN=, /ZSCALE,"
     print,"                NSIGMA= ,  COLOR_BAR_SCALE = 1 or 2"
     return
  endif

  if N_elements( xpin ) eq 1 then xp = xpin else xp=0
  if N_elements( ypin ) eq 1 then yp = ypin else yp=0

  if N_elements( dwin ) eq 1 then begin
     if( dwin ge 0 ) then begin
        get_window, dwin,/SHOW
        wshow, dwin,ICON=0
     endif
  endif

  szwin = float( [ !D.x_vsize, !D.y_vsize ] )
  if keyword_set( erw ) then erase
  if N_elements( Redf ) eq 1 then Magf = 1.0/Redf

  if N_elements( Magf ) eq 1 then begin
     maxMagf = 100
     if( szim[0] ge 2 ) then maxMagf = ceil( sqrt( 3000.*2000./(szim[1]*szim[2]) ) )>2
     if( Magf gt maxMagf ) then Magf = maxMagf
  endif

  if (szim[0] EQ 5) then begin               ; 3-D matrix of images:

     tvs, image[*,*,0,0,0], xp, yp, MAGF=Magf, REDF=Redf, TOP=topval, BOT=botv, LOG10=Logmin,$
				POWER=pow, ROTATION=rotidx, ANG_ROT=rotang, INVERT=invcmap, $
                     SMOOTH=smooth, ALL=all, ITER=iterate, MEDIAN=medfbox, RANGE_MED=ranmed, $
                     NSIG=nsigma, MIN=minv, MAX=maxv, NAME=name, ZSCALE=zscale, COLOR=cbars

     if N_elements( gap ) ne 1 then gap = 0
     nx = szim[1] + gap
     ny = szim[2] + gap

     if( nx gt 2*ny ) then begin ;; vertical groups arrangement

        nix = szim[5]
        niy = szim[3] * szim[4]
        if N_elements( Magf ) NE 1 then Magf = round( (szwin[0]/nix/nx)<(szwin[1]/niy/ny))>0.1

        for k = 0,szim[5]-1 do begin
           for j = 0,szim[4]-1 do begin
              for i = 0,szim[3]-1 do begin
                 x = xp + k * nx * Magf
                 y = yp + (i * ny + j * szim[4] * ny) * Magf
                 tvs, image[*,*,i,j,k], x, y, MAGF=Magf, REDF=Redf, $
                      MIN=minv, MAX=maxv, MEDIAN=medfbox, ZSCALE=zscale, $
                      TOPVAL=topval, BOT=botv, NSIG=nsigma, RANGE_MED=ranmed, $
                      LOG10=Logmin, POW=pow, ROT=rotidx, ANG_ROT=rotang, $
                      SMO=smooth, ALL=all, ITER=iterate, INVERT=invcmap
              endfor
           endfor
        endfor

     endif else begin            ;; horizontal groups arrangement

        nix = szim[3] * szim[4]
        niy = szim[5]
        if N_elements( Magf ) NE 1 then Magf= round((szwin[0]/nix/nx) < (szwin[1]/niy/ny))>0.1

        for k = 0,szim[5]-1 do begin
           for j = 0,szim[4]-1 do begin
              for i = 0,szim[3]-1 do begin
                 x = xp + j * i * nx * Magf
                 y = yp + k * ny * Magf
                 tvs, image[*,*,i,j,k], x, y, MAGF=Magf, REDF=Redf, $
                      MIN=minv, MAX=maxv, MEDIAN=medfbox, ZSCALE=zscale, $
                      TOPVAL=topval, BOT=botv, NSIG=nsigma, RANGE_MED=ranmed, $
                      LOG10=Logmin, POW=pow, ROT=rotidx, ANG_ROT=rotang, $
                      SMO=smooth, ALL=all, ITER=iterate, INVERT=invcmap
              endfor
           endfor
        endfor
     endelse

     return

  endif else if (szim[0] EQ 4) then begin ;matrix of images:

     if N_elements( gap ) ne 1 then gap = 0
     nx = szim[1] + gap
     ny = szim[2] + gap
     nix = szim[3]
     niy = szim[4]
     if N_elements( Magf ) NE 1 then Magf = round( (szwin[0]/nix/nx) < (szwin[1]/niy/ny))>0.1

     tvs, image[*,*,0,0], xp, yp, MAGF=Magf, REDF=Redf, TOP=topval, BOT=botv, LOG10=Logmin, $
				POWER=pow, ROTATION=rotidx, ANG_ROT=rotang, INVERT=invcmap, $
                     SMOOTH=smooth, ALL=all, ITER=iterate, MEDIAN=medfbox, RANGE_MED=ranmed,$
                     NSIG=nsigma, MIN=minv, MAX=maxv, NAME=name, ZSCALE=zscale, COLOR=cbars
     nim = nix * niy
     k = 0

     for j = 0,niy-1 do begin
        for i = 0,nix-1 do begin
           x = xp + i * nx * Magf
           y = yp + j * ny * Magf
           tvs, image[*,*,i,j], x, y, MAGF=Magf, REDF=Redf, $
                MIN=minv, MAX=maxv, MEDIAN=medfbox, ZSCALE=zscale, $
                TOPVAL=topval, BOT=botv, NSIG=nsigma, RANGE_MED=ranmed, $
                LOG10=Logmin, POW=pow, ROT=rotidx, ANG_ROT=rotang, $
                SMO=smooth, ALL=all, ITER=iterate, INVERT=invcmap
           if N_elements( name ) eq nim then xyouts, x, y,/DEV, name[k++]
        endfor
     endfor

     return

  endif else if (szim[0] EQ 3) then begin ;stack of images:

     if keyword_set( Loopwait ) then begin
        if N_elements( Loopwait ) gt 1 then begin
           waitLoop = Loopwait[0]
           nLoop = Loopwait[1]
        endif else begin
           waitLoop = Loopwait
           if( waitLoop LT 0 ) then nLoop=1000 else nLoop=9
        endelse
        nim = szim[3]
        s = ''
        for n=0,nLoop-1 do begin
           for k=0,nim-1 do begin
              tvs, image[*,*,k], xp, yp, MAGF=Magf,REDF=Redf, TOP=topval,BOT=botv,COL=cbars,$
                   LOG10=Logmin, POWER=pow, ROTATION=rotidx, ANG_ROT=rotang, $
                   SMOOTH=smooth, ALL=all, ITER=iterate, MEDIAN=medfbox, RANGE_MED=ranmed,$
                   NSIG=nsigma, MIN=minv, MAX=maxv, NAME=name, ZSCALE=zscale, INVERT=invcmap
              if( waitLoop LT 0 ) then begin
                 read,s
                 if( s eq 'q') then return
              endif else wait, waitLoop
           endfor
        endfor
        return
     endif

     if N_elements( gap ) ne 1 then gap = 0
     nx = szim[1] + gap
     ny = szim[2] + gap
     Nim = szim[3]
     if N_elements( Magf ) NE 1 then Magf = sqrt( szwin[0]*szwin[1]/(nx*ny*Nim) )
     if( Magf LT 1 ) then Magf = ( 1.0/round( 1.0/Magf ) ) > 0.1
     if keyword_set( vert ) then nix=1 else nix = fix( szwin[0]/float(szim[1])/Magf ) > 1
     if keyword_set( horiz ) then niy=1 else niy = Nim/nix +1

     tvs, image[*,*,0], xp, yp, MAGF=Magf, REDF=Redf, TOP=topval, BOT=botv, $
				LOG10=Logmin, POWER=pow, ROTATION=rotidx, ANG_ROT=rotang,$
          SMOOTH=smooth, ALL=all, ITER=iterate, MEDIAN=medfbox, RANGE_MED=ranmed, $
          NSIG=nsigma, MIN=minv, MAX=maxv, NAME=name, ZSCALE=zscale, INVERT=invcmap

     if N_elements( name ) eq Nim then xyouts, xp, yp,/DEV, name[0],SIZE=namesize,FONT=1

     for j = 0,niy-1 do begin
        for i = 0,nix-1 do begin
           k = nix*j + i
           if (k GT 0) AND (k LT Nim) then begin
              if keyword_set( cbars ) AND (k eq Nim-1) then cbarshow = cbars
              x = xp + i * nx * Magf
              y = yp + j * ny * Magf
              tvs, image[*,*,k], x,y, MAGF=Magf, REDF=Redf, MIN=minv,MAX=maxv, ZSCAL=zscale,$
                              TOPVAL=topval, BOT=botv, MEDIAN=medfbox, NSIG=nsigma,$
                   LOG10=Logmin,POW=pow,ROT=rotidx,ANG_ROT=rotang,RANGE_M=ranmed,$
                   SMO=smooth, ALL=all, ITER=iterate, COLOR=cbarshow, INVERT=invcmap
              if N_elements( name ) eq Nim then xyouts, x,y,/DEV,name[k],SIZ=namesize,FONT=1
           endif
        endfor
     endfor

     return
  endif

; display image of complex valued pixels as two seperate real & imaginary images;

  if (szim[szim[0]+1] EQ 6) then begin

		if keyword_set( absval ) then begin
			imr = abs( float( image ) )
			imi = abs( imaginary( image ) )
		  endif else begin
			imr = float( image )
			imi = imaginary( image )
		   endelse

		minim = min( imr, MAX=maxr ) < min( imi, MAX=maxi )
		maxim = maxr > maxi

		tvs, imr, xp, yp, MAGF=Magf, REDF=Redf, MIN=minim, MAX=maxim, LOG10=Logmin,$
                     TOPVAL=topval, BOT=botv, MEDIAN=medfbox, NSIG=nsigma, $
                     POW=pow, ROT=rotidx, ANG_ROT=rotang, INVERT=invcmap, $
                     SMO=smooth,ALL=all,ITER=iterate, ZSCALE=zscale,RANGE_M=ranmed

		tvs, imi, xp+Magf*szim[1], yp, MAGF=Magf, REDF=Redf, MIN=minim, MAX=maxim,$
                     TOPV=topval, BOT=botv, MEDIAN=medfbox, NSIG=nsigma,LOG=Logmin,$
                     POW=pow, ROT=rotidx, ANG_ROT=rotang, INVERT=invcmap, $
                     SMO=smooth,ALL=all,ITER=iterate, ZSCALE=zscale,RANGE_M=ranmed
		return
  endif

; recursive call to handle keyword option SIGMA_FILTER:

  if keyword_set( sigfilt ) then begin

     tvs, sigma_filter( image, RADIUS=sigfilt, N_SIG=nsigma,/MEDIAN, ITER=iterate ), xp, yp,$
                                   MAGF=Magf, REDF=Redf, MIN=minv, MAX=maxv, NAME=name, $
          TOPV=topval,BOT=botv, ZSCAL=zscale, LOG=Logmin,POW=pow, ROT=rotidx,ANG_ROT=rotang,$
          XSIZ=xsize,YSIZ=ysize, NORMAL=normcoor, COLOR_B=cbars, NBIN_CB=nbin, INVERT=invcmap
     return
  endif
;---------------------------------------------------------------------------------------------
; This is the core of procedure:
;---------------------------------------------------------------------------------------------
	if !D.name eq "PS" then Magf = 1 else begin
           if N_elements( Magf ) NE 1 then Magf = 4 < min( float( szwin )/szim[1:2] )
           if( Magf LT 1 ) then Magf = 1.0 / round( 1/Magf )
           if( Magf LE 0 ) then Magf = 0.1
        endelse

        if keyword_set( zscale ) then begin
           zscran = Zscale_Range( image, NIT=(zscale>1) )
           minc = zscran[0]
           maxc = zscran[1]
        endif else begin
           if N_elements( minv ) EQ 1 then minc = minv
           if N_elements( maxv ) EQ 1 then maxc = maxv
        endelse

        if keyword_set( rotang ) then begin

           imscaled = scale_image( rotmag( image, ANG=rotang, MAG=Magf ), $
                                   REDUCF=Redf, MIN=minc, MAX=maxc, $
                                   TOP=topval, BOT=botv, LOG=Logmin, POWER=pow, $
                                   ROT=rotidx, SM=smooth, ALL=all, ITER=iterate, $
                                   NSIG=nsigma, MED=medfbox, RANGE_MED=ranmed )
        endif else begin

           imscaled = scale_image( image, Magf, REDUCF=Redf, MIN=minc, MAX=maxc, $
                                   TOP=topval, BOT=botv, LOG=Logmin, POWER=pow, $
                                   ROT=rotidx, SM=smooth, ALL=all, ITER=iterate, $
                                   NSIG=nsigma, MED=medfbox, RANGE_MED=ranmed )
        endelse
;---------------------------------------------------------------------------------------------
; option to invert the values, used for PS hardcopy to get white background:
;---------------------------------------------------------------------------------------------
        if keyword_set( invcmap ) then begin

           ncol = !D.n_colors
           if keyword_set( topval ) then ncol = topval
           grey_map = reverse( bindgen( ncol ))
           imscaled = grey_map[ imscaled ]

        endif else if keyword_set( whiteback ) then begin

           wz = where( imscaled LE 0, nz )
           if( nz gt 0 ) then imscaled[wz] = !D.n_colors -1
        endif

        if !D.name eq "PS" then begin

		if keyword_set( normcoor ) then begin

                   if N_elements( xsize ) NE 1 then xsize=1
                   if N_elements( ysize ) NE 1 then ysize=1
                   tv, imscaled, xp,yp, XSIZE=xsize, YSIZE=ysize,/NORM

                endif else tv, imscaled, xp,yp, XSIZE=xsize, YSIZE=ysize,/DEV

	 endif else  tv, imscaled, xp,yp
;---------------------------------------------------------------------------------------------
;options for colorbar with Labeled scale:
;---------------------------------------------------------------------------------------------
	if keyword_set( cbars ) then begin

           if N_elements( name ) NE 1 then name=""

           if N_elements( maxc ) NE 1 then begin
              maxc = max( image, MIN=minv )
              if N_elements( minc ) NE 1 then minc=minv
           endif else if N_elements( minc ) NE 1 then minc = min( image )

           if keyword_set( Logmin ) then begin
              scale="Log10"
              minc = Logmin
           endif else scale=""

           if keyword_set( nbin ) then begin
              nbin = nbin > 7
           endif else nbin = 40
	
           histodata = histogram( imscaled )

           if !D.name EQ "PS" then begin ;show next to image:

              color_scale, minc, maxc, topval, scale, name, LOGMIN=Logmin, $
                           POS=[ [xp, yp], [xsize, ysize * cbars] ], INVERT=invcmap
              empty

           endif else begin     ;show in separate window and optionally next to image:

              device,GET_WINDOW_POS=wpos
              poscb = [ (wpos[0]-220) > 10, (wpos[1]+!D.y_vsize-320) < (!DEVY-330) ]
              winim = !D.window
              color_scale, minc, maxc, topval, scale, name, $
                           LOG=Logmin, HISTO=histodata, POS=poscb, INVERT=invcmap
              empty

              if (cbars GT 1) then begin
                 wait,0.01
                 cbimg = tvrd()
                 szcb = size( cbimg )
                 wset,winim
                 xpos =((szim[1]*Magf +xp+ !D.x_ch_size*(cbars-1)) < (!D.x_size - szcb[1]))>0
                 ypos = (yp - !D.y_ch_size) > 0
                 tv, cbimg, xpos, ypos
              endif else wset,winim
           endelse
        endif
end
