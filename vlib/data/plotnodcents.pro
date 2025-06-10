pro plotnodcents, cxv, cyv, HEADER=fhdn, TIME=timnods, FILE=fileall, DATA=imnods, LINEFIT=Linefit

  szx = size( cxv )
  szy = size( cyv )

  if szx[0] eq 2 and szy[0] eq 2 then begin
     numnods = szx[2]
     nfhrec = N_elements( fhdn )
     if( nfhrec LT 100 ) then begin
        message,/INFO,"must give HEADER=  FITS header array of strings"
        return
     endif
  endif else begin
     if Keyword_set( fileall ) then imnods = mef_reduce( fileall, fhdn, /SEP )
     szim = size( imnods )
     help,imnods
     if szim[0] ne 4 then begin
        message,/INFO,"must give FILE=  or DATA= 4D array of nodsets"
        return
     endif
     numnods = szim[4]
     cxv = fltarr( 2, numnods )
     cyv = cxv
     for n=0,numnods-1 do begin
        for i=0,1 do begin
           centroid, imnods[*,*,i,n],cx,cy
           cxv[i,n]=cx
           cyv[i,n]=cy
           print,i,n
        endfor
     endfor
  endelse

  szt = size( timnods )

  if szt[0] ne 2 then begin
     nodtim = sxpar(fhdn,"nodtime")
     noddel = sxpar(fhdn,"noddelay")
     help,nodtim,noddel
     nodtimm = nodtim + noddel/2
     if( nodtimm LT 1 ) then begin
        message,/INFO,"must give HEADER=  FITS header array of strings"
        return
     endif
     timnods = nodtimm * findgen( 2, numnods )/60
  endif

  save_pxyz
  !P.multi=[0,1,2]
  !P.charsize = 1.4
  !P.thick = 2
  symsiz = 1.4
  ptit = sxpar(fhdn,"obsmode") + " : " + sxpar(fhdn,"DATE-OBS")+" - " + sxpar(fhdn,"UTSTART")

  if keyword_set( hardcopy ) then begin
     psland,FILE=hardcopy
  endif else get_window,0,/SHOW,/ERASE

  plot,timnods,cxv,/ysty,YTIT="X - cntrd (pix)",TIT=ptit,YMAR=[2,2],/xsty
  oplot,timnods[0,*],cxv[0,*],ps=4,SYMSIZ=symsiz
  oplot,timnods[1,*],cxv[1,*],ps=6,SYMSIZ=symsiz

  plot,timnods,cyv,/ysty,XTIT="Time (minutes)",YTIT="Y - cntrd (pix)",YMAR=[4,0],/xsty
  oplot,timnods[0,*],cyv[0,*],ps=4,SYMSIZ=symsiz
  oplot,timnods[1,*],cyv[1,*],ps=6,SYMSIZ=symsiz

  if keyword_set( hardcopy ) then psclose
  save_pxyz,/RESTORE
end
