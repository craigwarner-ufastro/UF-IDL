;+
; NAME:
;	plot_stats
;
; PURPOSE:
;	Plot array of statistics of an images (from function im_stats).
;
; CALLING:
;	plot_stats, stats, IMAGE_LIST= ,  PLATE_SCALE=
;
; INPUTS:
;	stats = array image statistic structures (function im_stats).
;
; KEYWORDS:
;	IMAGE_LIST = optional arrays of image structures containing stats.
;
; OUTPUT:
;	none.
; HISTORY:
;	Written, Frank Varosi Univ. of FL. 2011
;	F.V. 2012, added more plot options.
;-

pro plot_stats, stats, IMAGE_LIST=image_List, PLATE_SCALE=arcsecpix, WINDOW=iwin

  common plot_stats, pwindow
  common plot_stats1, rangeFWHM 

  if N_struct( image_List ) gt 0 then begin
     if max( tag_names( image_List ) eq "STATS" ) then begin
        menu = ["Choose images for which statistics are desired:",$
                "Select individual images ?"	,$
                "Select group of images ?"	,$
                "Lasso images with rubber-box ?"	,$
                "Pick images with common point ?" ,$
                "All images"			]
        inums = pick_images( image_List, MENU=menu, INIT=5 )
        if (inums[0] LT 0) then return
        plot, image_List[inums].stats, PLATE_SCALE=arcsecpix
        return
     endif else begin
        message,/INFO,"Structure does not contain STATS structure"
        return
     endelse
  endif

  Nstat = N_struct( stats )
  if (Nstat LE 0) then return

  tags = tag_names( stats )
  tag_expt = max( tags EQ "EXPTIME" )
  tag_rms = max( tags EQ "RMS" )
  tag_chop = max( tags EQ "CHOPPA" )

  dname = stats[0].object  + " : " + stats[0].filter  + " : " + strmid(stats[0].name,0,20)
  minmjd = min( stats.mjd, MAX=maxmjd )

  if( maxmjd eq minmjd ) then begin
     tsec = findgen( Nstat )
     xtit = "frame #"
  endif else begin
     xtit = "Seconds"
     tsec = ( stats.mjd - minmjd )*24*60*60
  endelse

  if( tag_chop ) then begin
     chopinfo = "Chop: PA=" + string( stats[0].chopPA, FORM="(F5.0)" ) + $
                ", Throw=" + string( stats[0].chopThrow, FORM="(F4.0)" )
  endif else chopinfo = ""

  menu = ["Select which statistic to plot:",$
          "FWHM (x & y)",$
          "SNR (max & rms)",$
          "Centroids (x & y)",$
          "Noise (Std. Dev.)",$
          "Background Level",$
          " ","HARDCOPY ALL"," ",$
          "Set Range for FWHM plot",$
          "Unset Range",$
          " ","Done"]
  initsel = 1

MENU:
  if keyword_set( hardcopy ) then begin
     request = next_word( menu[ hardcopy ] )
     print,hardcopy,":",request
     hardcopy = (hardcopy + 1) mod 6
  endif else begin
     psclose
     request = next_word( menu[ wmenux( menu, INIT=initsel, TIT=0, WIN=iwin ) ] )
     initsel = ((initsel + 1) mod 6) > 1
  endelse

DOIT:
  CASE request OF

     "FWHM": BEGIN
        if N_elements( rangeFWHM ) ne 2 then rangeFWHM = [0,0]
        if max( rangeFWHM ) gt min( rangeFWHM ) then ystyle=1 else ystyle=0
        ytit = "FWHM (pixels)"
        fwhmxy = transpose( stats.fwhm_xy )
        if max( tags eq "ARCS_PIXEL" ) then begin
           if min( stats.arcs_pixel ) gt 0 then begin
              ytit = "FWHM (arcsec)"
              fwhmxy = transpose( stats.fwhm_xy * stats.arcs_pixel )
           endif else message,/INFO,"Arcsec / pixel plate scale is zero ?"
        endif else if keyword_set( arcsecpix ) then begin
           ytit = "FWHM (arcsec)"
           fwhmxy = transpose( stats.fwhm_xy * arcsecpix )
        endif
        ptit = "FWHM (" + stats[0].fitmethod + ") of " + dname
        stit = "X=diamonds : Y=crosses"
        ts2 = [tsec,tsec]
        get_window, pwindow,XISIZ=770,YISIZ=500,TIT="Plot Stats",XPOS=200,YPOS=200,/SHOW
        plot, ts2, fwhmxy, PS=3,/XSTY,XTIT=xtit,YTIT=ytit,TIT=ptit,CHARSIZ=1.3,YRAN=rangeFWHM,YSTY=ystyle
        oplot, tsec, fwhmxy[*,0], ps=-4
        oplot, tsec, fwhmxy[*,1], ps=-7, LIN=1
        xyouts,/NORM,0.02,0.02,stit,SIZE=1.3
        xyouts,/NORM,0.69,0.02,chopinfo,SIZE=1.3
     END

     "SNR": BEGIN
        sigman = stats.sigma_noise

        if (tag_expt) then begin
           expTime = stats.expTime
           if( min( expTime ) gt 0 ) then begin
              if( min( expTime ) LT max( expTime ) ) then begin
                 scaleByTime = yes_no_menu("Scale by Max. Exp. Time",/BINARY)
              endif
           endif
           if keyword_set( scaleByTime ) then begin
              maxExpTime = max( expTime )
              expTratio = expTime / maxExpTime
              sigman = sigman * sqrt( expTratio )
           endif
        endif

        SNRmax = replicate( 1.0, Nstat )
        SNRrms = SNRmax
        w = where( [sigman] GT 0, nsw )

        if (nsw GT 0) then begin
           SNRmax[w] = (stats[w].max - stats[w].sky_Level) / sigman[w]
           if (tag_rms) then SNRrms[w] = stats[w].rms / sigman[w]
        endif

        ptit = "SNR of " + dname
        stit = "MAX=diamonds : RMS=crosses"
        get_window, pwindow,XISIZ=770,YISIZ=500,TIT="Plot Stats",XPOS=200,YPOS=200,/SHOW
        plot, tsec, SNRmax, PS=-4,/XSTY,XTIT=xtit,YTIT="SNR (max & RMS)",TIT=ptit,CHARSIZ=1.3
        oplot, tsec, SNRrms, ps=-7, LIN=1
        xyouts,/NORM,0.02,0.02,stit,SIZE=1.3
        xyouts,/NORM,0.69,0.02,chopinfo,SIZE=1.3
     END

     "Noise": BEGIN
        ptit = "Most Freq. Noise in " + dname
        get_window, pwindow,XISIZ=770,YISIZ=500,TIT="Plot Stats",XPOS=200,YPOS=200,/SHOW
        plot, tsec, stats.sigma_noise, PS=-4,/XSTY,XTIT=xtit,YTIT="Std. Dev. (ADU/sec)",TIT=ptit,CHARSIZ=1.3
     END

     "Background": BEGIN
        ptit = "Most Freq. Bkgd. in " + dname
        get_window, pwindow,XISIZ=770,YISIZ=500,TIT="Plot Stats",XPOS=200,YPOS=200,/SHOW
        plot, tsec, stats.sky_Level, PS=-4,/XSTY,XTIT=xtit,YTIT="Background (ADU/sec)",TIT=ptit,CHARSIZ=1.3
     END

     "Centroids": BEGIN
        ptit = "Centroids of " + dname
        get_window, pwindow,XISIZ=770,YISIZ=600,TIT="Plot Stats",XPOS=200,YPOS=200,/SHOW
        !P.multi = [0,1,2]
        plot, tsec, stats.cent_xy[0], PS=-4,/XSTY,YTIT="X - pixels",TIT=ptit,CHARSIZ=1.3,YSTY=16,YMAR=[2,2]
        plot, tsec, stats.cent_xy[1], PS=-4,/XSTY,XTIT=xtit,YTIT="Y - pixels",CHARSIZ=1.3,YSTY=16,YMAR=[4,0]
        !P.multi[*] = 0
        xyouts,/NORM,0.02,0.02,chopinfo,SIZE=1.3
     END

     "HARDCOPY": BEGIN
        hardcopy = 1
        dir = find_dir("save")
        PSfile = dir + "Pstats-" + strmid(stats[0].name,0,20) + ".ps"
        print," writing graphics to disk file: " + PSfile
        psland, FILE=PSfile
     END

     "Set": BEGIN
        rangeFWHM = float( get_words( get_text_input("enter Range for FWHM plot (zero to free)", $
                                                     DEF=strconcat(rangeFWHM))))
        if N_elements( rangeFWHM ) gt 2 then rangeFWHM = rangeFWHM[0:1]
        print,rangeFWHM
        initsel = 1
        request = "FWHM"
        goto,DOIT
     END

     "Unset": BEGIN
        rangeFWHM = [0,0]
        initsel = 1
        request = "FWHM"
        goto,DOIT
     END

     "Done": return

     else:
  ENDCASE

  goto,MENU
end
