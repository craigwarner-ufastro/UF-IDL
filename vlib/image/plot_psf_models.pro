;+
; NAME:
;	plot_psf_models
; PURPOSE:
;	Display plots of the PSF (image) with model fits overplotted,
;	in either x - y profile cuts, or radial azimuthly collapsed profiles.
;	Also display 2D images of residuals resulting from each model fit.
; CALLING:
;	plot_psf_models, psf, NAME=name, /PRINT_INFO
; INPUTS:
;	psf = 2D array (image) giving point-spread-function (peaked source).
; KEYWORDS (in):
;	NAME = string, name of PSF to use in titles.
;	MFS_LORENTZIAN = parameters of Lorentzian fit to PSF
;	MFS_GAUSSIAN = parameters of Gaussian fit to PSF
;	MFS_MOFFAT = parameters of Moffat fit to PSF
; EXTERNAL CALLS:
;	pro tvs
;	pro get_window
;	function nint
; COMMON BLOCKS:
;	common plot_psf_models, winpfits, winpfitdiag, winpfitresid
; HISTORY:
;	Written, Frank Varosi at UF-astron. 2008 (moved from code in pro show_psf)
;	FV @ UF-astron. 2018 : added _EXTRA keyword option for plots.
;	FV @ UF-astron. 2018 : modified to plot just one model PSF fit.
;	FV @ UF-astron. 2022 : keyword TINFINITY= factor to get extended total flux
;-

pro plot_psf_models, psf, NAME=name, WIDTH_PLOT=plot_width, RADIAL_PLOTS=radialp, PSFMODEL=modpsf, $
                     LOGMIN=minLog, YLOG=yLog, RESID_RANGE=residrange, SYMSIZ=symsiz,TINFINITY=tinf,$
                     MFS_EMPIRICAL=pfitE, MFS_LORENTZIAN=pfitL, MFS_GAUSSIAN=pfitG,MFS_MOFFAT=pfitM,$
                     HARDCOPY=hardcopy, DIAGONALS=pdiags, _EXTRA=extra, VERTICAL=pvertical

  common plot_psf_models, winpfitcuts, winpfitdiag, winpfitresid
  common plot_psf_models1, winpfitradial

        save_pxyz
	xcs = !D.x_ch_size
	ycs = !D.y_ch_size
	sim = size( psf )		
	gxy = gridxy( sim[1], sim[2] )

	if N_elements( mafg ) ne 1 then begin
           magf = 256./total( sim[1:2]/2 )
           if  (magf LT 1)  then magf = 1./nint( 1/magf ) else magf = nint( magf )
        endif

	simm = magf*sim
	ixsiz = simm[1] > 256
	iysiz = simm[2]
	wxsiz = 2*ixsiz
	wysiz = iysiz + 2*ycs

        if N_struct( modpsf ) eq 1 then begin
           if strpos( modpsf.model,"Lor" ) ge 0 then pfitL = modpsf
           if strpos( modpsf.model,"Mof" ) ge 0 then pfitM = modpsf
           if strpos( modpsf.model,"Gau" ) ge 0 then pfitG = modpsf
        endif

        if N_struct( pfitL ) eq 1 then begin
           cx = pfitL.cx
           cy = pfitL.cy
        endif else if N_struct( pfitM ) eq 1 then begin
           cx = pfitM.cx
           cy = pfitM.cy
        endif else if N_struct( pfitG ) eq 1 then begin
           cx = pfitG.cx
           cy = pfitG.cy
        endif else if N_struct( pfitE ) eq 1 then begin
           cx = pfitE.cx
           cy = pfitE.cy
        endif else centroid, psf, cx, cy

        if N_struct( pfitE ) eq 1 then signoise = pfitE.noise else signoise = sky_noise( psf, mv )
        maxpv = psf[cx,cy]
        maxmax = maxpv
        nplots = N_struct( pfitL ) + N_struct( pfitM ) + N_struct( pfitG )

        if N_struct( pfitG ) eq 1 then begin
           psf_Gaus = psf_Gaussian( pfitG.params, NPIX=sim[1:2] ) + pfitg.skyfit
           if keyword_set( tinf ) then begin
              pf = pfitG
              pf.params[1:2] += sim[1:2]*tinf
              psf_Gaus_inf = psf_Gaussian( pf.params, NPIX=sim[1:2]*(tinf+1) ) + pfitg.skyfit
           endif
           maxpG = max(psf_Gaus)
           maxmax = maxpG
           signoisG = sky_noise( psf - psf_Gaus )
           fdx = reform( pfitG.fitdomain[0,*] - cx )
           fdy = reform( pfitG.fitdomain[1,*] - cy )
           if N_elements( name ) ne 1 then name = pfitG.name
        endif
        if N_struct( pfitM ) eq 1 then begin
           psf_Moff = psf_Moffat( pfitM.params, NPIX=sim[1:2] ) + pfitM.skyfit
           if keyword_set( tinf ) then begin
              pf = pfitM
              pf.params[1:2] += sim[1:2]*tinf
              psf_Moff_inf = psf_Moffat( pf.params, NPIX=sim[1:2]*(tinf+1) ) + pfitM.skyfit
           endif
           maxpM = max(psf_Moff)
           maxmax = maxpM
           signoisM = sky_noise( psf - psf_Moff )
           fdx = reform( pfitM.fitdomain[0,*] - cx )
           fdy = reform( pfitM.fitdomain[1,*] - cy )
           if N_elements( name ) ne 1 then name = pfitM.name
        endif
        if N_struct( pfitL ) eq 1 then begin
           psf_Lor = psf_Lorentzian( pfitL.params, NPIX=sim[1:2] ) + pfitL.skyfit
           if keyword_set( tinf ) then begin
              pf = pfitL
              pf.params[1:2] += sim[1:2]*tinf
              psf_Lor_inf = psf_Lorentzian( pf.params, NPIX=sim[1:2]*(tinf+1) ) + pfitL.skyfit
           endif
           maxpL = max(psf_Lor)
           maxmax = maxpL
           signoisL = sky_noise( psf - psf_Lor )
           fdx = reform( pfitL.fitdomain[0,*] - cx )
           fdy = reform( pfitL.fitdomain[1,*] - cy )
           if N_elements( name ) ne 1 then name = pfitL.name
        endif

        if( nplots ge 3 ) then begin
           print,"maxima:", maxpv, maxpL, maxpM, maxpG, FORM="(A,5G11.3)"
           print,FORM="(A,5G11.3)", "totals:", $
                 total(psf), total(psf_Lor), total(psf_Moff), total(psf_Gaus)
           if keyword_set( tinf ) then begin
              print,FORM="(A,5G11.3)", "totals:", $
                    total(psf), total(psf_Lor_inf), total(psf_Moff_inf), total(psf_Gaus_inf)
           endif
           print,FORM="(A,5G11.3)","noise =",signoise, signoisL, signoisM, signoisG
           print,FORM="(A,5G11.3)","maxSNR=", $
                 maxpv/signoise, maxpL/signoisL, maxpM/signoisM, maxpG/signoisG
           maxmax = max( [maxpv, maxpL, maxpM, maxpG] )
           wxsiz = nplots * ixsiz + 3*xcs
           wysiz = iysiz + 2*ycs
           get_window, winpfitresid, TIT="PSF fit Residuals for "+name, XS=wxsiz,YS=wysiz,/SHOW,/ERAS
           if N_elements( residrange ) ne 1 then residrange = maxmax/5
           ixsiz = ixsiz + xcs
           tvs, psf - psf_Lor,        0, MIN=-residrange, MAX=residrange, MAG=magf
           tvs, psf - psf_Moff,   ixsiz, MIN=-residrange, MAX=residrange, MAG=magf
           tvs, psf - psf_Gaus, 2*ixsiz, MIN=-residrange, MAX=residrange, MAG=magf
           posxy = (0.5 + pfitL.fitdomain[*,0]) * magf
           sizxy = (0.5 + pfitL.fitdomain[*,1]) * magf - posxy
           box_draw2,/NOSAV, POS=posxy, SIZE=sizxy
           posxy[0] = posxy[0] + ixsiz
           box_draw2,/NOSAV, POS=posxy, SIZE=sizxy
           posxy[0] = posxy[0] + ixsiz
           box_draw2,/NOSAV, POS=posxy, SIZE=sizxy
           ixsiz = ixsiz + xcs
           printw,"Lorentzian:", PIXOFF=2*xcs
           printw,"Moffat:",     PIXOFF=ixsiz
           printw,"Gaussian:",   PIXOFF=2*ixsiz
        endif

        if N_elements( name ) ne 1 then name = ""
        if N_elements( minLog ) ne 1 then minLog = 1.0
	if keyword_set( yLog ) then !y.range = [minLog, maxmax] else !y.range = [0, maxmax]
	mtoph = [maxpv/2, 10*maxpv]
	mboth = [minLog, maxpv/2]
	mfull = [minLog, 10*maxpv]
	if N_elements( symsiz ) ne 1 then symsiz=1

        if keyword_set( radialp ) then begin

           if keyword_set( hardcopy ) then begin
              !P.multi = [0,1,nplots]
              psfile = name + "_LinFit"+strtrim(nplots,2)+"Pradi.ps"
              if keyword_set( yLog ) then psfile = name + "_LogFit"+strtrim(nplots,2)+"Pradi.ps"
              if( nplots eq 1 ) then psport,/SQUARE,FIL=psfile else psport,/LONG,FIL=psfile
              message,/INFO,"writing plot to file: " + psfile
              if N_elements( symsiz ) ne 1 then symsiz = 3
              if N_elements( Lthick ) ne 1 then Lthick = 4
              !P.thick = 5
              ynamLoc = 1.0
              dsiz = 0.7
              psdot = 4
              if( nplots gt 1 ) then xmar = [5,1]
           endif else begin
              dsiz = 0.5
              ynamLoc = 0.985
              psdot = 4
              if N_elements( symsiz ) ne 1 then symsiz = 1
              if N_elements( Lthick ) ne 1 then Lthick = 2
              if keyword_set( pvertical ) then begin
                 !P.multi = [0,1,nplots]
                 get_window, winpfitradial,TIT="PSF:Fits",XIS=700,XPOS=0,YIS=900,YPOS=0,/SHOW,/ERASE
              endif else begin
                 !P.multi = [0,nplots,1]
                 get_window, winpfitradial,TIT="PSF:Fits",XIS=900,XPOS=0,YIS=700,YPOS=0,/SHOW,/ERASE
              endelse
              if( nplots gt 1 ) then begin
                 xmar = [7,1]
                 !P.charsize = 2.3
              endif
           endelse

           gxy[*,*,0] = gxy[*,*,0] - cx + 0.5
           gxy[*,*,1] = gxy[*,*,1] - cy + 0.5
           rxy = sqrt( gxy[*,*,0]^2 + gxy[*,*,1]^2 )
           if N_elements( plot_width ) eq 1 then !x.range = [0, plot_width]
           maxhalf = replicate( maxpv/2., 2 )
           xminmax = [ 0, max( rxy ) ]
           fdxy = sqrt( fdx[1]^2 + fdy[1]^2 )
           !x.title = "Radius from Centroid (pixels)"
           !x.style = 1
           if N_struct( pfitE ) eq 1 then wc = where( rxy LT sqrt(pfitE.fwhmx^2 + pfitE.fwhmy^2)/2 )
           if N_struct( pfitG ) eq 1 then wc = where( rxy LT sqrt(pfitG.fwhmx^2 + pfitG.fwhmy^2)/2 )
           if N_struct( pfitM ) eq 1 then wc = where( rxy LT sqrt(pfitM.fwhmx^2 + pfitM.fwhmy^2)/2 )
           if N_struct( pfitL ) eq 1 then wc = where( rxy LT sqrt(pfitL.fwhmx^2 + pfitL.fwhmy^2)/2 )
           sr = sort( rxy )

           if N_struct( pfitL ) eq 1 then begin
              tname = pfitL.model
              if( nplots LE 2 ) then tname += (" : " + name)
              plot, rxy, psf > minLog,YLOG=yLog,PSYM=psdot,XMAR=xmar,TIT=tname, _EXTRA=extra,SYMS=dsiz
              oplot, rxy[wc], psf[wc] > minLog, PSYM=4, SYMSIZ=symsiz, THICK=Lthick
              oplot, rxy[sr], psf_Lor[sr], THICK=Lthick
              oplot, xminmax, maxhalf, LIN=1
              fwhmL = sqrt( pfitL.fwhmx^2 + pfitL.fwhmy^2 )/2/sqrt(2)
              oplot, replicate( fwhmL, 2 ), mboth, LINE=1
              oplot, replicate( fdxy, 2 ), mtoph, LINE=1
              if( nplots eq 1 ) then begin
                 xyouts,/NORM,0.6,0.9,"FWHM = "+string([pfitL.fwhmx,pfitL.fwhmy],FORM="(2F9.2)"),FON=0
                 xyouts,/NORM,0.7,0.85,"ChisqFit = "+string( pfitL.chisq,FORM="(F10.5)"),FONT=0
              endif else begin
                 if keyword_set( pvertical ) then begin
                    xyouts,/NORM, 0.6, 0.66 + 0.9/nplots,$
                           "FWHM = "+string([pfitL.fwhmx,pfitL.fwhmy],FORM="(2F9.2)"),FON=0
                    xyouts,/NORM, 0.7, 0.66 + 0.85/nplots,$
                           "ChisqFit = "+string( pfitL.chisq,FORM="(F10.5)"),FONT=0
                 endif else begin
                    xyouts,/NORM, 0.6/nplots, 0.9,$
                           "FWHM = "+string([pfitL.fwhmx, pfitL.fwhmy],FORM="(2F9.2)"),FONT=0
                    xyouts,/NORM, 0.7/nplots, 0.85,$
                           "ChisqFit = "+string( pfitL.chisq,FORM="(F10.5)"),FONT=0
                 endelse
              endelse
           endif

           if N_struct( pfitM ) eq 1 then begin
              tname = pfitM.model
              if( nplots LE 2 ) then tname += (" : " + name)
              plot, rxy, psf > minLog,YLOG=yLog,PSYM=psdot,XMAR=xmar,TIT=tname, _EXTRA=extra,SYMS=dsiz
              oplot, rxy[wc], psf[wc] > minLog, PSYM=4, SYMSIZ=symsiz, THICK=Lthick
              oplot, rxy[sr], psf_Moff[sr], THICK=Lthick
              oplot, xminmax, maxhalf, LIN=1
              fwhmM = sqrt( pfitM.fwhmx^2 + pfitM.fwhmy^2 )/2/sqrt(2)
              oplot, replicate( fwhmM, 2 ), mboth, LINE=1
              oplot, replicate( fdxy, 2 ), mtoph, LINE=1
              if( nplots eq 1 ) then begin
                 xyouts,/NORM,0.6,0.9,"FWHM = "+string([pfitM.fwhmx,pfitM.fwhmy],FORM="(2F9.2)"),FON=0
                 xyouts,/NORM,0.7, 0.85,"ChisqFit = "+string( pfitM.chisq,FORM="(F10.5)"),FONT=0
              endif else begin
                 if keyword_set( pvertical ) then begin
                    xyouts,/NORM,0.6, 0.33 + 0.9/nplots,$
                           "FWHM = "+string([pfitM.fwhmx, pfitM.fwhmy],FORM="(2F9.2)"),FON=0
                    xyouts,/NORM,0.7, 0.33 + 0.85/nplots,$
                           "ChisqFit = "+string( pfitM.chisq,FORM="(F10.5)"),FONT=0
                 endif else begin
                    xyouts,/NORM,1.0/nplots + 0.6/nplots,0.9,$
                           "FWHM = "+string([pfitM.fwhmx, pfitM.fwhmy],FORM="(2F9.2)"),FON=0
                    xyouts,/NORM,1.0/nplots + 0.7/nplots,0.85,$
                           "ChisqFit = "+string( pfitM.chisq,FORM="(F10.5)"),FONT=0
                 endelse
              endelse
           endif

           if N_struct( pfitG ) eq 1 then begin
              tname = pfitG.model
              if( nplots LE 2 ) then tname += (" : " + name)
              plot, rxy, psf > minLog,YLOG=yLog,PSYM=psdot,XMAR=xmar,TIT=tname, _EXTRA=extra,SYMS=dsiz
              oplot, rxy[wc], psf[wc] > minLog, PSYM=4, SYMSIZ=symsiz, THICK=Lthick
              oplot, rxy[sr], psf_Gaus[sr], THICK=Lthick
              oplot, xminmax, maxhalf, LIN=1
              fwhmG = sqrt( pfitG.fwhmx^2 + pfitG.fwhmy^2 )/2/sqrt(2)
              oplot, replicate( fwhmG, 2 ), mboth, LINE=1
              oplot, replicate( fdxy, 2 ), mtoph, LINE=1
              if( nplots eq 1 ) then begin
                 xyouts,/NORM,0.6,0.9,"FWHM = "+string([pfitG.fwhmx,pfitG.fwhmy],FORM="(2F9.2)"),FON=0
                 xyouts,/NORM,0.7,0.85,"ChisqFit = "+string( pfitG.chisq,FORM="(F10.5)"),FONT=0
              endif else begin
                 if keyword_set( pvertical ) then begin
                    xyouts,/NORM,0.6,0.9/nplots,$
                           "FWHM = "+string([pfitG.fwhmx,pfitG.fwhmy],FORM="(2F9.2)"),FON=0
                    xyouts,/NORM,0.7,0.85/nplots,$
                           "ChisqFit = "+string( pfitG.chisq,FORM="(F10.5)"),FONT=0
                 endif else begin
                    xyouts,/NORM,2.0/nplots + 0.6/nplots,0.9,$
                           "FWHM = "+string([pfitG.fwhmx,pfitG.fwhmy],FORM="(2F9.2)"),FON=0
                    xyouts,/NORM,2.0/nplots + 0.7/nplots,0.85,$
                           "ChisqFit = "+string( pfitG.chisq,FORM="(F10.5)"),FONT=0
                 endelse
              endelse
           endif

           if( nplots gt 2 ) then xyouts,/NORM, 0.3, ynamLoc, name, ALIGN=0.5, SIZE=1.5

	 endif else begin

		!P.multi = [0,1,2]
		icx = round( cx )
                icy = round( cy )
		if N_elements( plot_width ) eq 1 then radiplot = plot_width/2
		if N_elements( radiplot ) ne 1 then radiplot = min( sim[1:2] )/2 - 1
		radiplot = radiplot < ( min( sim[1:2] )/2 - 1)
		xr = ( [icx-radiplot, icx+radiplot+1] > 0) < (sim[1]-1)
		yr = ( [icy-radiplot, icy+radiplot+1] > 0) < (sim[2]-1)
		xc = xr[0] + findgen(xr[1]-xr[0]) - cx + 0.5
		yc = yr[0] + findgen(yr[1]-yr[0]) - cy + 0.5

		if keyword_set( hardcopy ) then begin
			psfile = name + "_LinFitPcuts.ps"
			if keyword_set( yLog ) then psfile = name + "_LogFitPcuts.ps"
			psport,/LONG,FIL=psfile
                        message,/INFO,"writing plot to file: " + psfile
                        if N_elements( symsiz ) ne 1 then symsiz=3
                        if N_elements( Lthick ) ne 1 then Lthick=3
                endif else begin
                   if N_elements( symsiz ) ne 1 then symsiz=1
                   if N_elements( Lthick ) ne 1 then Lthick=2
                endelse

		get_window, winpfitcuts, TIT="PSF:Fits",XIS=600,XPOS=0,YIS=800,YPOS=0,/SHOW,/ERASE
		ptit = name + " : FWHM-x = " $
			 + string( pfitE.fwhmx, FORM="(F5.2)" ) + "(e) " $
			 + string( pfitL.fwhmx, FORM="(F5.2)" ) + "(L) " $
			 + string( pfitg.fwhmx, FORM="(F5.2)" ) + "(g) " $
			 + string( pfitM.fwhmx, FORM="(F5.2)" ) + "(M) "

		maxhalf = replicate( maxpv/2., N_elements( xc ) )

		plot, xc, psf[xr[0]:xr[1], cy] > minLog, YLOG=yLog, PS=4,SYMSIZ=symsiz,THIC=Lthick,$
							/XSTY,XTIT="x : pixels",TIT=ptit, _EXTRA=extra
		oplot, xc, psf[xr[0]:xr[1], cy] > minLog, PS=1, SYMSIZ=symsiz, THICK=Lthick
		oplot, [0,0], mtoph, LINE=1, THICK=Lthick
		for i=0,1 do oplot, [fdx[i],fdx[i]], mtoph, LINE=1, THICK=Lthick
		oplot, xc, maxhalf,LINE=1, THICK=Lthick
		oplot, xc, psf_Lor[xr[0]:xr[1], cy], THICK=Lthick
		oplot, xc, psf_Gaus[xr[0]:xr[1], cy], LIN=1, THICK=Lthick
		oplot, xc, psf_Moff[xr[0]:xr[1], cy], LIN=2, THICK=Lthick

		ptit = name + " : FWHM-y = " $
			 + string( pfitE.fwhmy, FORM="(F5.2)" ) + "(e) " $
			 + string( pfitL.fwhmy, FORM="(F5.2)" ) + "(L) " $
			 + string( pfitg.fwhmy, FORM="(F5.2)" ) + "(g) " $
			 + string( pfitM.fwhmy, FORM="(F5.2)" ) + "(M) "

		plot, yc, psf[cx, yr[0]:yr[1]] > minLog, YLOG=yLog, PS=4,SYMSIZ=symsiz,THIC=Lthick,$
							/XSTY,XTIT="y : pixels",TIT=ptit, _EXTRA=extra
		oplot, yc, psf[cx, yr[0]:yr[1]] > minLog, PS=1, SYMSIZ=symsiz, THICK=Lthick
		oplot, [0,0],mtoph,LINE=1, THICK=Lthick
		for i=0,1 do oplot, [fdy[i],fdy[i]], mtoph, LINE=1, THICK=Lthick
		oplot, xc, maxhalf,LINE=1, THICK=Lthick
		oplot, yc, psf_Lor[cx, yr[0]:yr[1]], THICK=Lthick
		oplot, yc, psf_Gaus[cx, yr[0]:yr[1]], LIN=1, THICK=Lthick
		oplot, yc, psf_Moff[cx, yr[0]:yr[1]], LIN=2, THICK=Lthick

                if keyword_set( pdiags ) then begin

                   get_window, winpfitdiag, TIT="PSF:Fits:diagonals", XSIZ=600, XPOS=0, $
                               YSIZ=800, YPOS=0,/SHOW,/ERASE

                   ptit = name + " : FWHM-x = " $
                          + string( pfitE.fwhmx, FORM="(F5.2)" ) + "(e) " $
                          + string( pfitL.fwhmx, FORM="(F5.2)" ) + "(L) " $
                          + string( pfitg.fwhmx, FORM="(F5.2)" ) + "(g) " $
                          + string( pfitM.fwhmx, FORM="(F5.2)" ) + "(M) "

                   xey = sqrt((xc-xc[0])^2 + (yc-yc[0])^2) - sqrt(xc[0]^2 + yc[0]^2)
                   npd = N_elements( xey )
                   psfdiag = fltarr( npd )
                   xs = xr[0]
                   ys = yr[0]
                   for i=0, npd-1 do psfdiag[i] = psf[xs+i,ys+i]
                   xran = [ min(xc)<min(yc), max(xc)>max(yc) ]

                   plot, xey, psfdiag > minLog, YLOG=yLog, PS=4, XRAN=xran,/XSTY, SYMSIZ=symsiz, $
                         XTIT="x = y  : pixels", TIT=ptit, _EXTRA=extra, THICK=Lthick

                   oplot, xey, psfdiag > minLog, PS=1, SYMSIZ=symsiz, THICK=Lthick
                   oplot, [0,0],mtoph,LINE=1, THICK=Lthick
                   for i=0,1 do oplot, [fdx[i],fdx[i]], mtoph, LINE=1, THICK=Lthick
                   oplot, xc, maxhalf,LINE=1, THICK=Lthick
                   for i=0, npd-1 do psfdiag[i] = psf_Lor[xs+i,ys+i]
                   oplot, xey, psfdiag, THICK=Lthick
                   for i=0, npd-1 do psfdiag[i] = psf_Gaus[xs+i,ys+i]
                   oplot, xey, psfdiag, LIN=1, THICK=Lthick
                   for i=0, npd-1 do psfdiag[i] = psf_Moff[xs+i,ys+i]
                   oplot, xey, psfdiag, LIN=2, THICK=Lthick

                   ptit = name + " : FWHM-y = " $
                          + string( pfitE.fwhmy, FORM="(F5.2)" ) + "(e) " $
                          + string( pfitL.fwhmy, FORM="(F5.2)" ) + "(L) " $
                          + string( pfitg.fwhmy, FORM="(F5.2)" ) + "(g) " $
                          + string( pfitM.fwhmy, FORM="(F5.2)" ) + "(M) "

                   xey = sqrt((reverse(xc)-max(xc))^2 + (yc-yc[0])^2) - sqrt(max(xc)^2 + yc[0]^2)
                   npd = N_elements( xey )
                   psfdiag = fltarr( npd )
                   xs = xr[1]
                   ys = yr[0]
                   for i=0, npd-1 do psfdiag[i] = psf[xs-i-1,ys+i]

                   plot, xey, psfdiag > minLog, YLOG=yLog, PS=4, XRAN=xran,/XSTY, SYMSIZ=symsiz, $
                         XTIT="y = -x  :  pixels", TIT=ptit, _EXTRA=extra, THICK=Lthick

                   oplot, xey, psfdiag > minLog, PS=1, SYMSIZ=symsiz, THICK=Lthick
                   oplot, [0,0],mtoph,LINE=1, THICK=Lthick
                   for i=0,1 do oplot, [fdy[i],fdy[i]], mtoph, LINE=1, THICK=Lthick
                   oplot, xc, maxhalf,LINE=1, THICK=Lthick
                   for i=0, npd-1 do psfdiag[i] = psf_Lor[xs-i-1,ys+i]
                   oplot, xey, psfdiag, THICK=Lthick
                   for i=0, npd-1 do psfdiag[i] = psf_Gaus[xs-i-1,ys+i]
                   oplot, xey, psfdiag, LIN=1, THICK=Lthick
                   for i=0, npd-1 do psfdiag[i] = psf_Moff[xs-i-1,ys+i]
                   oplot, xey, psfdiag, LIN=2, THICK=Lthick
                endif

	   endelse

	if keyword_set( hardcopy ) then psclose
        save_pxyz,/RESTORE
end
