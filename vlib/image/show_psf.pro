;+
; NAME:
;	show_psf
; PURPOSE:
;	Display the PSF (image) in pseudo-color with Linear and Log-10 scale,
;	and as surface (Log-10) with contours (SHOW3).
;	Centroid is determined and marked, and optionally
;	the FWHM and other parameters are determined and printed using function fitPSF.
; CALLING:
;	show_psf, psf, NAME=name, /PRINT_INFO
; INPUTS:
;	psf = 2D array (image) giving point-spread-function (peaked source).
; KEYWORDS (in):
;	NAME = string, name of PSF to use in titles.
;	XSIZ_3D, YSIZ_3D = window size for 3D surface plot (def = 512 by 378 ).
;	NPOWER_10 = # of powers of 10 to show on 3D surface.
;	/NO_3D     : skip the 3D surface plot.
;	/FIT_PARMS : fits PSF models Lorentzian, Moffat, Gaussian.
;	/GET_PARMS : same as /FIT
;	/PLOT_FITS : create plots of data x-y cuts and model fits.
;	/RADIAL_PLOTS : make plots of data versus radius from centroid (instead of cuts)
;	WIDTH_PLOT = # pixels width to show in plots (default is all).
;	/PRINT_INFO
; KEYWORDS (out):
;	INFO_PARMS = all parameters of PSF (if /PRINT_INFO)
;	LORENTZIAN_PARAMS = parameters of PSF (if /PRINT_INFO or /GET_PARMS)
;	MOFFAT_PARAMS = parameters of PSF (if /PRINT_INFO or /GET_PARMS)
;	GAUSSIAN_PARAMS = parameters of PSF (if /PRINT_INFO or /GET_PARMS)
; EXTERNAL CALLS:
;	pro tvs
;	pro show3
;	pro centroid
;	pro get_window
;	function nint
;	function fitPSF
;	pro plot_psf_models
; COMMON BLOCKS:
;	common show_psf, winpsf2D, winpsf3D
;	common show_psf2, winpfits
;	common show_psf3, winpfitdiag
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	F.V. UFastro, 2007, added keyword option to plot the x-y profile cuts of psf fits.
;	F.V. UFastro, 2007, modified to use new function fitPSF.
;	F.V. UFastro, 2008, put plotting code into plot_psf_models.pro
;	F.V. UFastro, 2008, added radial plots option.
;	FV 2022 : keyword TINFINITY=factor to get extended total flux (in plot_psf_models.pro)
;-

pro show_psf, psf, NAME=name, FIT_PSF=fitdata, PLOT_FITS=plotfits, YLOG=yLog, PRINT_INFO=print_info,$
              RADIAL_PLOTS=radialp, WIDTH_PLOT=plot_width, NO_3D=no_3D, XSIZ_3D=xsiz, YSIZ_3D=ysiz,$
              RESID_RANGE=residrange, CFWHM=cfwhm, NPOWER_10=Nordmag, GET_PARMS=getp, SYMSIZ=symsiz,$
              DIAGONAL_PLOTS=pdiag, HARDCOPY=hardcopy, SKY_FIT=sky_fit, $
              LORENTZIAN_PAR=parmL, GAUSSIAN_PAR=parmG, MOFFAT_PAR=parmM, VERTICAL=pvertical, $
              MFS_LORENTZIAN=pfitL, MFS_GAUSSIAN=pfitG, MFS_MOFFAT=pfitM, _EXTRA=extra, $
              POISSON_WEIGHTS=poiswt, DOMAIN_FIT_FACTOR=dfitf, TINFINITY=tinf

  common show_psf, winpsf2D, winpsf3D

	winold = !D.window
	xcs = !D.x_ch_size
	ycs = !D.y_ch_size
	sim = size( psf )		
	magf = 256./total( sim[1:2]/2 )
	if  (magf LT 1)  then magf = 1./nint( 1/magf ) else magf = nint( magf )
	simm = magf*sim
	ixsiz = simm[1] > 256
	iysiz = simm(2)
	wxsiz = 2*ixsiz
	wysiz = iysiz + 2*ycs

	get_window, winpsf2D,TIT="PSF:2D",XS=wxsiz,XP=!DEVX-wxsiz,YS=wysiz,YP=!DEVY-wysiz,/ERAS,/SHOW
	maxpsf = max( psf )
	Logmax = ceil( 2*aLog10( maxpsf ) )/2.0
	if N_elements( Nordmag ) NE 1 then Nordmag = 4
	Logmin = Logmax - Nordmag
	minLog = 10.^Logmin
	tvs, MAG=magf, psf

        if N_elements( cfwhm ) ne 1 then cfwhm = avg( fw_hm( psf ) )
	centroid, psf, cx,cy, FWHM=cfwhm
	stat = draw_mark( cx*magf, cy*magf, /NOSAVE )

	tvs, LOG=minLog, MAG=magf, psf, ixsiz
	stat = draw_mark( cx*magf + ixsiz, cy*magf, /NOSAVE )

	xyouts, xcs, iysiz+0.5*ycs, /DEV,"Linear:",FONT=0
	xyouts, ixsiz*1.7, iysiz+0.5*ycs, /DEV,"Log10:",FONT=0

        if N_elements( name ) EQ 1 then begin
           dpos = strpos( name,".f",/REVERSE_SEARCH )
           if( dpos LE 0 ) then dpos = strlen( name )
           psfname = strmid( name, 0, dpos )
           xyouts, !D.x_vsize/2, !D.y_vsize-1.5*ycs, psfname, ALIGN=0.5,/DEV,FONT=0
        endif
	empty
	if N_elements( psfname ) ne 1 then psfname="Fit_PSF"
	if keyword_set( plotfits ) then fitdata=1
	if keyword_set( print_info ) then fitdata=1

	if keyword_set( getp ) OR keyword_set( fitdata ) then begin
		signoise = sky_noise( psf, skye )
                if keyword_set( sky_fit ) then fwhmi = fullwid_halfmax( psf-skye ) $
                else fwhmi = fullwid_halfmax( psf)
		pfitE ={ model:"empirical", name:psfname, cx:cx,cy:cy, fwhmx:fwhmi[0],fwhmy:fwhmi[1],$
				noise:signoise, skybkgrd:skye }
;;		pfitA = fitPSF( psf, /AIRY, POIS=poiswt, SKY=sky_fit, DOMAIN=dfitf, NAME=psfname )
;;		parmA = pfitA.params
		pfitG = fitPSF( psf, /GAUSSIAN, POIS=poiswt, SKY=sky_fit, DOMAIN=dfitf, NAME=psfname )
		parmG = pfitG.params
		pfitL = fitPSF( psf, /LORENTZ, POIS=poiswt, SKY=sky_fit, DOMAIN=dfitf, NAME=psfname )
		parmL = pfitL.params
		pfitM = fitPSF( psf, /MOFFAT, POIS=poiswt, SKY=sky_fit, DOMAIN=dfitf, NAME=psfname )
		parmM = pfitM.params
		posxy = (0.5 + pfitL.fitdomain[0:1]) * magf
		sizxy = (0.5 + pfitL.fitdomain[2:3]) * magf - posxy
		box_draw,/NOSAV, POS=posxy, SIZE=sizxy
		posxy[0] = posxy[0] + ixsiz
		box_draw2,/NOSAV, POS=posxy, SIZE=sizxy
	   endif

	if keyword_set( print_info ) OR keyword_set( plotfits ) then begin
           if N_elements( psfname ) EQ 1 then print,"Name: ",psfname
           print,"sigma noise = ", signoise, FORM="(A,G9.3)"
           print," sky bkgrnd < ", skye, FORM="(A,G9.3)"
           print,"   max. SNR = ", maxpsf/signoise, FORM="(A,G9.3)"
           if keyword_set( poiswt ) then $
              print,"Fits with Poisson data weighting:" $
           else	print,"Fits with uniform data weighting:"
           print,"        Empirical   Lorentzian    Moffat      Gaussian"
           print,"cent-x:",       cx, pfitL.cx,    pfitM.cx,    pfitG.cx,    FORM="(A,5F11.2)"
           print,"cent-y:",       cy, pfitL.cy,    pfitM.cy,    pfitG.cy,    FORM="(A,5F11.2)"
           print,"FWHM-x:", fwhmi[0], pfitL.fwhmx, pfitM.fwhmx, pfitG.fwhmx, FORM="(A,5F11.2)"
           print,"FWHM-y:", fwhmi[1], pfitL.fwhmy, pfitM.fwhmy, pfitG.fwhmy, FORM="(A,5F11.2)"
           print,"ChiSq.:",        1, pfitL.chisq, pfitM.chisq, pfitG.chisq, FORM="(A,5G11.2)"
           if keyword_set( sky_fit ) then $
              print,"SkyFit:", 0, pfitL.skyfit, pfitM.skyfit, pfitG.skyfit, FORM="(A,5G11.2)"
           if keyword_set( print_info ) then begin
              if( print_info gt 1 ) then begin
                 print,"Rpar-x:", 0, pfitL.radx, pfitM.alphax, pfitG.sigx, FORM="(A,5F11.2)"
                 print,"Rpar-y:", 0, pfitL.rady, pfitM.alphay, pfitG.sigy,FORM="(A,5F11.2)"
                 print,"Power :", 0, pfitL.power,  pfitM.beta, 0, FORM="(A,5F11.2)"
                 print,"Pscl-x:", 0, pfitL.psx,             0, 0, FORM="(A,5G11.2)"
                 print,"Pscl-y:", 0, pfitL.psy,             0, 0, FORM="(A,5G11.2)"
              endif
           endif
        endif

	if keyword_set( plotfits ) then begin

           plot_psf_models, psf, NAME=psfname, WIDTH_PLOT=plot_width, RADIAL_PLOTS=radialp, $
                            LOGMI=minLog,YLOG=yLog, RESID_RAN=residrange, SYMSIZ=symsiz, DIAG=pdiag,$
                            MFS_LORENTZIAN=pfitL, MFS_GAUSSIAN=pfitG, MFS_MOFFAT=pfitM, $
                            MFS_E=pfitE, HARDC=hardcopy, _EXTRA=extra, VERTIC=pvertical, TINF=tinf
        endif

	if keyword_set( no_3D ) then begin
		if (winold GE 0) then wset,winold
		return
	   endif

	if N_elements( plot_width ) eq 1 then radiplot = plot_width/2
	if N_elements( radiplot ) ne 1 then radiplot = min( sim[1:2] )/2 - 1
	icx = fix( cx )
	icy = fix( cy )

        save_pxyz
        !P.font = -1
	!P.multi = 0
	if N_elements( xsiz ) NE 1 then xsiz=512
	if N_elements( ysiz ) NE 1 then ysiz=378
	get_window, winpsf3D, TIT="PSF:3D", XISIZ=xsiz,XPOS=!DEVX-xsiz,YISIZ=ysiz,YPOS=0,/SHOW,/ERASE
	tvcrs,0.5,0.5,/NORM
	!Z.title = "Log_10  ( PSF )"
	!P.charsize = 3

	if min( sim[1:2] ) GT (2*radiplot) then begin
		xr = ( [icx-radiplot, icx+radiplot+1] > 0) < (sim[1]-1)
		yr = ( [icy-radiplot, icy+radiplot+1] > 0) < (sim[2]-1)
		!Z.range = [ Logmin, Logmax ]
		psfsub = psf[xr[0]:xr[1],yr[0]:yr[1]]
		minp = min( psfsub )
		if( minp gt 0 ) then !Z.range = [ Logmin > aLog10(minp), Logmax ]
		show3, aLog10( psfsub > minLog )
	 endif else begin
		!Z.range = [Logmin,Logmax]
		show3, aLog10( psf > minLog )
	  endelse

        empty
	if (winold GE 0) then wset,winold
        save_pxyz,/RES
end
