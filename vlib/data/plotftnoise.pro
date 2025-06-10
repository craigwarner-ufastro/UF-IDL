pro plotchopnoise, cfreq, cnoise, fhd, OPLOT=overp, _EXTRA=extra, STRUCT=ims, ITOT=itot

  if N_struct( ims ) gt 0 then begin
     cfreq = ims.chopfreq
     cnsec = ims.skynoise
     ftms = ims[0].frametime
     filter = ims[0].filter
     rd = ims[0].readmode
     uts = ims[0].UTstart
     obsdate = ims[0].date + "-" + strmid( uts, 0, 5 )
     humid = round( avg( ims.humout1 + ims.humout2 )/2 )
  endif else begin
     obsdate = sxpar( fhd,"DATE-OBS") + "-" + strmid( sxpar( fhd, "UT1"), 0, 5 )
     filter = sxpar( fhd, "FILTER1")
     if( strpos( filter, "Open" ) ge 0 ) then filter = sxpar( fhd, "FILTER2")
     rd = sxpar( fhd, "READMODE")
     ftms = sxpar( fhd, "FRMTIME")
     ftsec = ftms/1000           ;;diffs take 2 frames
     cnsec = cnoise*2*sqrt(ftsec)
     humid = round( sxpar(fhd,"humout1") + sxpar(fhd,"humout2") )/2
  endelse

  ftinfo = strtrim( fix( round( ftms )), 2 ) + "ms : "
  ptit = filter + " : " + ftinfo + rd + " : " + obsdate
  xtit = "Chop Freq. ( Hz )  :  " + strtrim( humid, 2 ) + " % Humidity"
  get_window,0,/SHOW
  plot,cfreq,cnsec,ps=4,xtit=xtit,ytit="St.Dev. Noise (ADU) / second", TIT=ptit, $
       charsiz=1.5,THICK=2,XTICKINT=1,_EXTRA=extra,XRAN=[0,4],/Xsty

  pif=[1,10]
  non_lin_lsq,cfreq,cnsec,pif,FUNC="inversefreq"
  cf = findgen(100)/5 + 0.5
  oplot,cf,inversefreq(cf,pif),THICK=2
  cnLimit = inversefreq(100,pif)
  oplot,[0,100], replicate( cnLimit, 2 ),LINE=1

  cnpinc = 100 * (cnsec - cnLimit)/cnLimit
  get_window,1,/SHOW
  plot,cfreq,cnpinc,ps=-4,xtit=xtit,ytit="Increase of Sky Noise ( % )", TIT=ptit, $
       charsiz=1.5, THICK=2,XTICKINT=1,XRAN=[0,4],/Xsty

  if N_struct( ims ) gt 0 then begin
     otime = ims.objtime
     etime = ims.elapsedtime
     if N_elements( itot ) ne 1 then itot = N_elements( ims[0].totsnr ) - 1
     itot = (itot > 10) < (N_elements( ims[0].totsnr ) - 1)
     get_window,2,/SHOW
     plot,cfreq,ims.totsnr[itot]/sqrt(otime),ps=-4,xtit=xtit,ytit="Star SNR (ADU) / second", TIT=ptit, $
       charsiz=1.5, THICK=2,XTICKINT=1,XRAN=[0,4],/Xsty    
     for i=itot-1,itot-10,-1 do oplot,cfreq,ims.totsnr[i]/sqrt(otime),ps=-5
     get_window,3,/SHOW
     plot,cfreq,ims.totsnr[itot]/etime,ps=-4,xtit=xtit,ytit="Star SNR (ADU) / ELAPSED sec.", TIT=ptit, $
       charsiz=1.5, THICK=2,XTICKINT=1,XRAN=[0,4],/Xsty    
     for i=itot-1,itot-10,-1 do oplot,cfreq,ims.totsnr[i]/etime,ps=-5
  endif

end
