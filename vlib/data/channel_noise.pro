pro channel_noise, data, sigmas, means, sigVert, sigHoriz, IMAGE_LIST=iml, DIFFSEQ=diffseq, $
                   NCHANNEL=nchan, COADDS=coadds, SUBTRACTIONS=subs, HEADER=fhd, $
                   PERSECOND=persec, USE_VARMAP=usevarmap, NVREGION=nvreg, PLOT=plotit, $
                   PROCESS=process, VERBOSE=verbose, SUMMARY=summary, $
                   ROWSTART=rowstart, ROWEND=rowend, STRUCT_FRAME=imstruct, _EXTRA=extra
  frametime=1
  savetime=1
  fname = ""
  filter = ""

  if N_struct( iml ) gt 1 then begin

     persec = 1
     fhd = *iml[0].pfhd
     fname = iml[0].winame
     if strpos(fname,":") gt 0 then fname = strtrim( strmid( fname, strpos(fname,":")+1 ), 2)
     framecoadds = iml[0].info.framecoadds
     chopcoadds = iml[0].info.chopcoadds
     if( chopcoadds gt 0 ) then coadds = framecoadds * chopcoadds else coadds = framecoadds
     frametime = iml[0].info.frametime
     savetime = iml[0].info.savetime
     rdmode = iml[0].info.readmode
     sample = rdmode
     vgate = iml[0].info.Vgate
     mcesim = iml[0].info.MCEsim
     date = iml[0].info.date
     time = iml[0].info.time
     filter = iml[0].info.filter
     sziml = size( iml )

     if( sziml[0] gt 1 ) then begin
        if keyword_set( process ) then begin
           subs = 1
           data = reform( iml[0,*].image - iml[1,*].image )
           if keyword_set( diffseq ) then begin
              subs = 2
              data = data[*,*,1:*] - data[*,*,0:*]
           endif
           if strpos( rdmode,"RAW") ge 0 then sample = strmid( rdmode,0,strpos(rdmode,"_RAW"))
        endif else begin
           channel_noise, IMAGE_LIST=reform(iml[0,*]), DIFF=diffseq,/PERSEC, SUMMARY=sumWells
           sumWells.sample = sumWells.sample + " : Wells"
           channel_noise, IMAGE_LIST=reform(iml[1,*]), DIFF=diffseq,/PERSEC, SUMMARY=sumClamps
           sumClamps.sample = sumClamps.sample + " : Clamps"
           summary = [[sumWells],[sumClamps]]
           return
        endelse
     endif else begin
        if keyword_set( diffseq ) then begin
           subs = 2
           data = iml[1:*].image - iml.image
        endif else begin
           subs = 1
           data = iml.image
        endelse
        if strpos( rdmode,"RAW") ge 0 then sample = strmid( rdmode, 0, strpos(rdmode,"_RAW"))
     endelse

  endif else if N_struct( imstruct ) eq 1 then begin

     data = imstruct.frame
     dname = strupcase( imstruct.name )
     fc = imstruct.fc
     scaled = 1
     subs = 1
     if( strpos( dname,'DIF') ge 0 ) then subs = 2
     chopcoadds = fc.coadds
     coadds = fc.framecoadds * fc.coadds
     frametime = fc.frametime
     savetime = fc.saveperiod
     fhd = *imstruct.pfhdr
     rdmode = sxpar(fhd,"READMODE")
     sample = rdmode
     vgate = sxpar(fhd,"VGATE")
     fname = sxpar(fhd,"FILENAME")
     date = sxpar(fhd,"DATE")
     time = sxpar(fhd,"UTSTART")

  endif else begin

     if N_elements( subs ) ne 1 then subs=0

     if N_elements( fhd ) gt 1 then begin
        framecoadds = sxpar(fhd,"FRMCOADD")
        chopcoadds = sxpar(fhd,"NCYCLES")
        if( chopcoadds gt 1 ) then coadds = framecoadds * chopcoadds else coadds = framecoadds
        frametime = sxpar(fhd,"FRMTIME")
        savetime = sxpar(fhd,"SAVETIME")
        rdmode = sxpar(fhd,"READMODE")
        sample = rdmode
        vgate = sxpar(fhd,"VGATE")
        fname = sxpar(fhd,"FILENAME")
        date = sxpar(fhd,"DATE")
        time = sxpar(fhd,"UTSTART")
     endif
  endelse

  if( subs LT 1 ) then subs = 1
  if N_elements( fhd ) LE 0 then fhd = 0
  if N_elements( coadds ) ne 1 then coadds=1
  if N_elements( nchan ) ne 1 then nchan=16
  if N_elements( nvreg ) ne 1 then nvreg=1

  if N_elements( date ) LE 0 then begin
     date = systime()
     time = systime()
  endif

  savsrctime = coadds * frametime/1000
  sz = size( data )

  if( sz[0] LT 2 ) then begin
     message,"missing 2D or 3D data array."
     return
  endif

  npixch = sz[1]/nchan
  npixvr = sz[2]/nvreg
  ndat = sz[3]
  if sz[0] eq 2 then ndat = 1

  if keyword_set( verbose ) then $
        help,coadds,chopcoadds,frametime,savetime,savsrctime,subs,filter,rdmode, ndat

  if( nvreg gt 1 ) then begin
     sigmas = fltarr( nvreg, nchan, ndat )
     rowstart = 0
     rowend = sz[2]-1
  endif else begin
     sigmas = fltarr( nchan, ndat )
     if N_elements( rowstart ) ne 1 then rowstart = 1
     if N_elements( rowend ) ne 1 then rowend = sz[2]-2
     rowstart >= 0
     rowstart <= (sz[2]-10)
     rowend >= (rowstart+10)
     rowend <= (sz[2]-1)
     npixvr = rowend - rowstart + 1
  endelse

  sigVert = sigmas
  sigHoriz = sigmas
  means = sigmas	

  for j=0,ndat-1 do begin

     for i=0,nchan-1 do begin

        chandat = float( data[ i*npixch:(i+1)*npixch-1, rowstart:rowend, j ] )

        if nvreg gt 1 then begin

           for k=0,nvreg-1 do begin
              regdat = chandat[ *, k*npixvr:(k+1)*npixvr-1 ]
              sigmas[k,i,j] = stdev( regdat, mv )
              means[k,i,j] = mv
              sigVert[k,i,j] = stdev( total( regdat, 1 ) ) / sqrt( npixch )
           endfor

        endif else begin

           if keyword_set( usevarmap ) then begin

              sigmas[i,j] = sky_noise( chandat, sky )
              means[i,j] = sky

           endif else begin

              sigmas[i,j] = stdev( chandat, mv )
              means[i,j] = mv

           endelse

           sigVert[i,j] = stdev( total( chandat, 1 ) ) / sqrt( npixch )
           sigHoriz[i,j] = stdev( total( chandat, 2 ) ) / sqrt( npixvr )

        endelse
     endfor
     if !DEBUG then begin
        help, nvreg, npixvr
        stop
     endif
  endfor

  sfactor = 1.0 / sqrt(subs)
  mfactor = 1.0

  if keyword_set( scaled ) then sfactor = sfactor * sqrt( coadds ) else begin
     mfactor = mfactor / coadds
     sfactor = sfactor / sqrt( coadds )
  endelse

  if keyword_set( persec ) then begin   ;;compensate if data was scaled to ADU / second.
     mfactor = mfactor * savsrctime
     sfactor = sfactor * savsrctime
  endif

  if keyword_set( verbose ) then help,mfactor,sfactor
  means = means * mfactor
  sigmas = sigmas * sfactor
  sigVert = sigVert * sfactor
  sigHoriz = sigHoriz * sfactor

  if N_elements( vgate ) ne 1 then return

  summ1 = { channel:0, mean:0.0, sigma:0.0, sigV:0.0, sigH:0.0, pfhd:ptr_new( fhd ), readmode:rdmode, $
            vgate:vgate, sample:sample, name:fname, date:date, time:time, chopcoadds:chopcoadds, $
            coadds:coadds, savetime:savetime, frametime:frametime, subs:subs, ndat:ndat, filter:filter }

  summary = replicate( summ1, nchan )
  summary.channel = 1 + indgen(nchan)

  if( ndat gt 1 ) then begin
     summary.mean = total( means, 2 )/ndat
     summary.sigma = total( sigmas, 2 )/ndat
     summary.sigV = total( sigVert, 2 )/ndat
     summary.sigH = total( sigHoriz, 2 )/ndat
  endif else begin
     summary.mean = means
     summary.sigma = sigmas
     summary.sigV = sigVert
     summary.sigH = sigHoriz
  endelse

  if keyword_set( plotit ) then plotchans, sigmas, means, HEAD=fhd,NDAT=ndat,SUMMAR=summary,_EXTRA=extra
end
