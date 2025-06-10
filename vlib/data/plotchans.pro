pro plotchans, sigmas, means, SUMMARY=summary, HEADER=fhd, NDAT=ndat, PMEANS=pmeans, $
               YRAN=yran, COMMENT=comment, WINDOW=winplot, UNSUBS=unsubs, _EXTRA=extra

  if N_struct( summary ) gt 0 then begin
     szs = size( summary )
     if szs[0] gt 1 then begin
        !P.multi = [0,1,2]
        get_window,winplot,XS=640,YS=1024,/SHOW
        plotchans, SUMMARY=summary[*,0],YRAN=yran, COMMENT=comment, WINDOW=winplot, _EXTRA=extra
        plotchans, SUMMARY=summary[*,1],YRAN=yran, COMMENT=comment, WINDOW=winplot, _EXTRA=extra
        !P.multi=0
        return
     endif
     sample = summary[0].sample
     filter = summary[0].filter
     fname = summary[0].name
     fc = summary[0].coadds
     ft = summary[0].frametime
     date = summary[0].date + " : " + summary[0].time
     ptit = sample+" : FT="+string(ft,FORM="(F4.1)")+"ms, FC="+strtrim(fc,2)
  endif else if N_elements( fhd ) gt 1 then begin
     if N_elements( fname ) ne 1 then fname = sxpar(fhd,"FILENAME")
     if strpos(fname,"_") gt 0 then fname = strmid( fname, 0, strpos(fname,"_") )
     date = sxpar(fhd,"DATE") + " : " + sxpar(fhd,"UTSTART")
     ft = sxpar(fhd,"FRMTIME")
     fc = sxpar(fhd,"FRMCOADD")
     if N_elements( sample ) ne 1 then sample = sxpar(fhd,"READMODE")
     ptit = sample + " : FT=" + string(ft,FORM="(F4.1)") + "ms, FC=" + strtrim(fc,2)
  endif else ptit=""

  if( strpos( fname,"none") LT 0 ) then ptit += (" : " + fname)
  ptit += (" : " + date)
  if N_elements( winplot ) ne 1 then winplot = 0

  if N_struct( summary ) gt 0 then begin

     nchan = N_struct( summary )
     chans = summary.channel - 0.5
     if N_elements( unsubs ) ne 1 then unsubs = 1
     unsubs = unsubs > 1
     usfac = sqrt( unsubs )

     if N_elements( yran ) ne 2 then begin
        sigmax = max( summary.sigV ) > max( summary.sigH ) > max( summary.sigma )
        yran = [0,sigmax] * usfac
     endif

     get_window,winplot,/SHOW
     plot, chans, summary.sigma * usfac, PS=10, /XSTY, XRAN=[0,nchan], YRAN=yran, XTICKS=nchan, $
           XTIT="Channel #",YTIT="Std.Dev.( noise ) ADU / Frame",TIT=ptit, _EXTRA=extra,THICK=2

     oplot, chans, summary.sigV * usfac, PS=10,LIN=2, _EXTRA=extra,THICK=3
     oplot, chans, summary.sigH * usfac, PS=10,LIN=1, _EXTRA=extra,THICK=3

     if N_elements( comment ) eq 1 then xyouts,/NORM,0.07,0.01,comment

     if keyword_set( pmeans ) then begin
        get_window,1,/SHOW
        plot, chans, summary.mean, PS=10, /XSTY, XRAN=[0,nchan], XTICKS=nchan, $
              XTIT="Channel #",YTIT="Mean Value ADU / Frame",TIT=ptit
     endif

     return

  endif else begin

     sz = size( sigmas )
     nvreg = 0

     if sz[0] LT 2 then begin
        ndat=1
        nchan = sz[1]
     endif else if sz[0] GT 2 then begin
        nvreg = sz[1]
        nchan = sz[2]
        ndat = sz[3]
     endif else begin
        if keyword_set( ndat ) then begin
           if( ndat eq sz[2]) then nchan = sz[1] else begin
              nvreg = sz[1]
              nchan = sz[2]
           endelse
        endif else begin
           nvreg = sz[1]
           nchan = sz[2]
        endelse
     endelse

     chans = indgen(nchan)+1

     if( nvreg gt 1 ) then begin

        get_window,winplot
        plot,chans,sigmas[0,*,0],ps=10, /XSTY, _EXTRA=extra, $
			XTIT="Channel #",YTIT="Std.Dev.( noise ) ADUs",TIT=ptit

        for j=0,ndat-1 do begin
           for k=0,nvreg-1 do oplot,chans,sigmas[k,*,j],ps=10
        endfor

        get_window,1
        plot,chans,means[0,*,0],ps=10, /XSTY, $
             XTIT="Channel #",YTIT="Mean Val",TIT=ptit

        for j=0,ndat-1 do begin
           for k=0,nvreg-1 do oplot,chans,means[k,*,j],ps=10
        endfor

     endif else begin

        get_window,winplot
        plot,chans,sigmas[*,0],ps=10, /XSTY, $
             XTIT="Channel #",YTIT="Std.Dev.( noise ) ADUs",TIT=ptit
        for j=0,ndat-1 do oplot,chans,sigmas[*,j],ps=10
        
        get_window,winplot+1
        plot,chans,means[*,0],ps=10, /XSTY, $
             XTIT="Channel #",YTIT="Mean Val",TIT=ptit
        for j=0,ndat-1 do oplot,chans,means[*,j],ps=10
     endelse
  endelse

end
