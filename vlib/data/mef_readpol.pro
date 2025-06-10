;+
; NAME
;	MEF_readPol
; PURPOSE:
;	Read MEF file of Polarimetry data and return all frames in multi-dimensional array.
;	Requires special read Loop because each saveset has an extension header.
;	Note that chop differences are not computed,
;	but the order of src & ref is interchanged for nod-beam B to be same as nod-beam A.
;	There are options to coadd frames within each nod beam,	or coadd all nodsets.
; CALLING:
;	data = MEF_readPol( MEF_file, mainHeader, extensionHeaders )
;
; INPUTS:
;	MEF_file = string, file name.
;
; KEYWORDS:
;	/PERSECOND : convert to ADU counts per second instead of counts per frametime
;	/SUM_BEAMS : coadd frames in each nod beam.
;	/SUM_NODS  : sum all the nodbeams and nodsets (assumes /COADD_NODBEAMS)
;
;	NODSETS  = override actual number of nod sets in file  (if file param has error)
;	NODBEAMS = override actual number of nod beams in file (if file param has error)
;
;	/STRUCT = return data as elements in structure with obs info.
;	        (default return when /SUM_BEAMS or /SUM_NODS specified).
; OUTPUTS:
;	Returns data in multi-dimensional array of Long-words or floating point.
;	if keywords /SUM_BEAMS or /SUM_NODS then structure is returned that contains data.
;
;	mainHeader = main FITS header of file.
;
;	extensionHeaders = array of Extension headers from file.
;
; MODIFICATION HISTORY:
;
;  Written  June 2003, by Frank Varosi, Department of Astronomy, University of Florida.
;  Mod June 2011, FV, UF, specific version of MEF_read converted for Polarimetry.
;  Mod June 2012, FV, UF, updated to be consistent with MEF_read.
;  Mod Sept 2019, FV, UF, updated to read newer type MEF files using ARC controller.
;  Mod Feb. 2020, FV, UF, simplified to keyword options SUM_BEAMS and SUM_NODS
;  Mod Aug. 2020, FV, UF, new key option NODGROUPS= (returns arrays of structures).
;  Mod July 2022, FV, UF, handle case of on array chopping for noise statistics.
;
;# $Name:  $ $Id: mef_readpol.pro,v 1.2 2014/01/30 00:14:50 varosi Exp $
;-

function MEF_readPol, MEF_file, mainHeader, extenHeaders, PERSECOND=persec, STRUCT=struct, $
                      SUM_BEAMS=sumbeams, SUM_NODS=sumnods, NODGROUPS=nodgroups, FCB=fcb, $
                      NODBEAMS=nodbeams, NODSETS=nodsets, PADMASKS=padmasks, VERBOSE=verbose

  common MEF_readPol, pmBot, pmTop   ;; approx. y-pixel defs of polarim. mask regions

  if N_elements( pmBot ) ne 6 or N_elements( pmTop ) ne 6 then begin
     pmTop = [ 39, 71, 111, 143, 184, 216 ]
     pmBot = [ 17, 49,  89, 121, 161, 194 ]
  endif

  if N_elements( padmasks ) ne 1 then padmasks = 3
  pmT = pmTop - padmasks -1
  pmB = pmBot + padmasks
  if keyword_set( verbose ) then print,"Vertical pixels in mask regions: ",pmT - pmB

	fits_open, MEF_file, fcb
        message,/INFO,"->"
        print, fcb.filename
        print, fcb.nextend," extensions in MEF file"
        mainHeader = fcb.hmain
	cammode = strupcase( sxpar( mainHeader, "CAMMODE" ) )

        if( strpos( cammode, "POLA") LT 0 ) then begin
           fits_close, fcb
           return, MEF_read( MEF_file, mainHeader, extenHeaders,/STRUCT, FCB=fcb, $
                             SUM_BEAMS=sumbeams, SUM_NODS=sumnods, /CHOPDIF, /COADD, $
                             PERSEC=persec, NODBEAMS=nodbeams, NODSETS=nodsets, VERB=verbose )
        endif

        instrument = sxpar( mainHeader, "INSTRUME")
	obsmode = sxpar( mainHeader, "OBSMODE")
	readmode = sxpar( mainHeader, "READMODE")
	welldepth = sxpar( mainHeader, "WELLDpth")
	savesets = fix( sxpar( mainHeader, "SAVESETS"))
        frameCoadds = fix( sxpar(mainHeader,"FRMCOADD"))
        saveCoadds = frameCoadds
        chopCoadds = fix( sxpar(mainHeader,"CHPCOADD"))
        chopFreq = float( sxpar(mainHeader,"CHPFREQ"))
        object = sxpar(mainHeader,"OBJECT")
        exptime = float( sxpar(mainHeader,"EXPTIME") )

        fw1 = sxpar( mainHeader,"filter1")
        fw2 = sxpar( mainHeader,"filter2")
        if( strpos( fw1,"Open" ) ge 0 ) then filter = fw2 else filter = fw1

        airmass = float( sxpar(mainHeader,'AIRMASS'))
        humid = float(sxpar( mainHeader,"HUMOUT1") + sxpar( mainHeader,"HUMOUT2"))/2
        pwv1 = float( sxpar( mainHeader,"PWVMON1"))
        pwv2 = float( sxpar( mainHeader,"PWVMON2"))

        if( pwv1 gt 0 and pwv2 gt 0 ) then pwv = (pwv1 + pwv2)/2 else begin
           if( pwv1 gt 0 ) then pwv = pwv1 else pwv = pwv2 
        endelse

        NsavSets = fix( sxpar( mainHeader,"NSAVSETS"))
        Nnodsets = fix( sxpar( mainHeader, "NNODSETS"))
	Ncycles = fix( sxpar( mainHeader,"NCYCLES"))
        
;; if Ncycles is in FITS header then this is new type of file,
;;  in which Ncycles is the # of chop cycles during each nod beam,
;;  using ARC controller that saves all frames so DAS does no coadding of chop cycles,
;;  and then chopCoadds parameter is simply the # of readouts per chop beam.
;;  So then # HWP angles must be 4 (which it normally is)

        if( Ncycles gt 0 ) then begin
           nHWPangs = 4
           print,"ARC controller readouts: ",Ncycles," Chop Cycles per Nod Beam."
        endif else begin
           nHWPangs = NsavSets
           if( chopCoadds gt 0 ) then saveCoadds = frameCoadds * chopCoadds
        endelse

        if NOT keyword_set( nodsets ) then nodsets = fix( sxpar( mainHeader, "NODSETS")) > 1
	if NOT keyword_set( nodbeams ) then nodbeams = fix( sxpar( mainHeader, "NNODS")) > 1

        print,cammode,"  :  ",obsmode,"  :  ",readmode,"  :  ",welldepth,"  :  ",object
	print,"On Src Time =", exptime," sec.",FORM="(A,F9.1,A)"
        print,"PWV=",pwv," mm  :  Humidity=", humid,$
              " % : Airmass=",airmass,FORM="(A,F6.1,A,F5.1,A,F5.2)"
        print,"SaveCoadds= ",strtrim( saveCoadds,2)," : SaveSets= ",strtrim( savesets,2),$
              " : NodBeams= ",strtrim( nodbeams,2)," : NodSets= ",strtrim( nodsets,2)

        if( NsavSets LT savesets ) then begin
           message,/INFO,"Obs. was probably ABORTED:"
           print," # actual savesets [",NsavSets,"] < [",savesets,"] configured."
        endif

        if( Nnodsets LT nodsets ) then begin
           message,/INFO,"Obs. was probably ABORTED:"
           print," # actual Nod Sets = [",Nnodsets,"] < [",nodsets,"] # configured."
        endif

	frameTime = float( sxpar( mainHeader, "FRMTIME"))
        saveSrcTime = saveCoadds * frameTime/1000

        print,"Save Src Time (sec) =", saveSrcTime,"  :  Frame Time (ms) =", frameTime, $
              "  :  Chop Freq. (Hz) =", chopFreq,FORM="(3(A,F7.2))"
        slit = sxpar(mainHeader,'SLIT')
        grating = sxpar( mainHeader, "grating")
        aperture = sxpar( mainHeader, "aperture")
	print,"Filters: ", sxpar( mainHeader,"FILTER1"),"  +  ",sxpar( mainHeader,"FILTER2")+$
              "  :  grating=", grating," : Slit=",slit," : Aperture=",aperture
        nehr = max( (fcb.start_data[1:*] - fcb.start_header[1:*])/80 )
        if keyword_set( verbose ) then print,"Longest exten. header = ",nehr," records."
        extenHeaders = strarr( nehr+1, nHWPangs, nodbeams, nodsets )
	iex=0
;;--------------------------------------------------------------------------------------
;; read data from file and store in multi-dim array with src=0 and ref=1
;;--------------------------------------------------------------------------------------
	for iNodSet = 1, Nnodsets do begin
           for iNodbeam = 1, nodbeams do begin

              ib1 = iNodbeam-1
              is1 = iNodSet-1

              for iang = 0, nHWPangs-1 do begin

                 iex = iex+1
                 fits_read, fcb, data, exhd, EXTEN=iex,/NO_ABORT

                 extenHeaders[0,iang,ib1,is1] = exhd
                 nodbeam = strtrim( sxpar( exhd, "NOD" ), 2 )
                 if( iex LE 1 ) then nodbeam = "A"          ;;override old bug (now fixed)
                 nodset = sxpar( exhd, "NODSET" )
                 if keyword_set( verbose ) then begin
                    print,"Wplate ang.=", string( sxpar( exhd,"WPLATE"),FORM="(F5.1)"), $
                          " ,   NodSetBeam =", string( nodset, FORM="(I3)") + nodbeam, $
                          " ,   Ext #", string( iex, FORM="(I4)")
                 endif
                 sd = size( data )

                 if( iex EQ 1 ) then begin

                    if sd[3] ne 2 then message,"data obs.mode is NOT chopping ?"
                       
                    ;;create full data array of dimension requested:
                    dataf = fltarr( sd[1],sd[2], 2, nHWPangs, nodbeams, nodsets )

                    refnoise = fltarr( sd[1], 6, nHWPangs, (nodsets-1)>1 )
                    skynoise = fltarr( 6, 2, nHWPangs, (nodsets-1)>1 )
                 endif

                 if sd[0] eq 4 then begin
                    ;; new data acq using ARC controller (DAS does not coadd chops)
                    coadds = sd[4] * saveCoadds

                    if( strpos( nodbeam,"A") ge 0 ) then begin
                       dataf[0,0,0,iang,ib1,is1] = total( data, 4 )/coadds
                    endif else begin
                       dataf[0,0,0,iang,ib1,is1] = total( data[*,*,1,*], 4 )/coadds
                       dataf[0,0,1,iang,ib1,is1] = total( data[*,*,0,*], 4 )/coadds
                    endelse

                 endif else begin  ;; MCE type data (coadding of savesets done by MCE):

                    if( strpos( nodbeam,"A") ge 0 ) then begin
                       dataf[0,0,0,iang,ib1,is1] = float( data )/ saveCoadds
                    endif else begin
                       dataf[0,0,0,iang,ib1,is1] = float( data[*,*,1] )/ saveCoadds
                       dataf[0,0,1,iang,ib1,is1] = float( data[*,*,0] )/ saveCoadds
                    endelse
                 endelse

              endfor
           endfor
        endfor

        fits_close,fcb
        sdf = size( dataf )
        ndim = sdf[0]
        Lxp = sdf[1]-1
;;--------------------------------------------------------------------------------------
;; compute noise statistics in each polarim. mask (mk):
;;--------------------------------------------------------------------------------------
        if( nodsets ge 2 ) then begin
           for ins = 0, nodsets-2 do begin
              for iang = 0, nHWPangs-1 do begin
                 dsrc = total( reform( dataf[*,*,0,iang,*,ins:ins+1] ), 3 )/nodbeams
                 dref = total( reform( dataf[*,*,1,iang,*,ins:ins+1] ), 3 )/nodbeams
                 for mk = 0, N_elements( pmBot )-1 do begin
                    dsrcm = dsrc[*,pmB[mk]:pmT[mk],*]
                    drefm = dref[*,pmB[mk]:pmT[mk],*]
                    dsdiff = dsrcm[*,*,1] - dsrcm[*,*,0]
                    drdiff = drefm[*,*,1] - drefm[*,*,0]
                    skynoise[mk,0,iang,ins] = sky_noise( dsdiff, mv )
                    skynoise[mk,1,iang,ins] = sky_noise( drdiff, mv )
                    refn = refnoise[*,0]
                    srcn = refn
                    for ix = 0,Lxp do begin
                       srcn[ix] = stdev( dsdiff[((ix-1)>0):((ix+1)<Lxp),*] )
                       refn[ix] = stdev( drdiff[((ix-1)>0):((ix+1)<Lxp),*] )
                    endfor
                    ;; if chopping is on det.array then src/ref beams interchange:
                    refnoise[*,mk,iang,ins] = refn < srcn
                    if !DEBUG then begin
                       print,ins, iang, mk, skynoise[mk,*,iang,ins], avg(srcn), avg(refn),$
                             avg( refnoise[*,mk,iang,ins] ),FORM='(3i5,5f9.2)'
                       sz = size( dsrcm )
                       magf=3
                       npy = sz[2] + 4
                       get_window,28,XSIZ=magf*sz[1], YSIZ=magf*npy*4 + 4,/SHOW
                       tvs, dsrcm - drefm,/ERAS,MAG=magf,GAP=4,/VERT
                       tvs, dsdiff, 0, 2 * magf * npy,MAG=magf,/ZS
                       tvs, drdiff, 0, 3 * magf * npy,MAG=magf,/ZS
                       s=''
                       read,s
                    endif
                 endfor
              endfor
           endfor

           if keyword_set( sumnods ) then begin
              if( nodsets gt 2 ) then begin       ;;compute average noise:
                 refnoise = total( refnoise, 4 ) / ( nodsets-1 )
                 skynoise = total( skynoise, 4 ) / ( nodsets-1 )
              endif
              ;; rescale average noise sums according to Central Limit Theorem:
              refnoise /= sqrt( nodsets )
              skynoise /= sqrt( nodsets )
           endif
           ;; rescale noise after diffs according to Central Limit Theorem:
           refnoise /= sqrt( 2 )
           skynoise /= sqrt( 2 )
        endif

        if keyword_set( persec ) then begin
           calibfac = float( 1.e3 / frameTime )
           if keyword_set( verbose ) then help,calibfac
           refnoise *= calibfac
           skynoise *= calibfac
        endif
;;--------------------------------------------------------------------------------------
;; options to sum nodsets, or groups nodsets, which returns structure var.
;; default is sum to nod-beams, then specify /STRUCT to get structure var.
;;--------------------------------------------------------------------------------------
        if keyword_set( sumbeams ) or $
           keyword_set( sumnods ) or keyword_set( nodgroups ) then begin

           if keyword_set( nodgroups ) then begin

              nodgroups >= 2
              help, nodgroups

              if( nodsets/nodgroups ne float(nodset)/nodgroups ) then begin
                 factor, nodsets, p, n
                 nodgroups = p[0]
                 help, nodgroups
              endif

              nodspergroup = nodsets / nodgroups
              nsum = nodbeams * nodspergroup
              datasum = fltarr( sdf[1], sdf[2], sdf[3], sdf[4], nodgroups )

              for k=0, nodgroups-1 do begin
                 inod = k * nodspergroup
                 Lnod = inod + nodspergroup -1
                 datasum[*,*,*,*,k] = total( total( dataf[*,*,*,*,*,inod:Lnod], 6), 5)/nsum
              endfor

           endif else begin

              if keyword_set( sumbeams ) and keyword_set( sumnods ) then begin

                 datasum = total( total( dataf, ndim ), ndim-1 ) / (sdf[ndim]*sdf[ndim-1])

              endif else if keyword_set( sumnods ) then begin

                 datasum = total( dataf, ndim ) / sdf[ndim]

              endif else  datasum = reform( total( dataf, ndim-1 ) / sdf[ndim-1] )

              nodgroups = 1
           endelse

           dataf = 0
           sds = size( datasum )

           ;; Use 360 element array to store all FITS headers so they will be same size:
           sheader = strarr( N_elements( mainHeader ) > 360 )
           sheader[0] = mainHeader
           if keyword_set( calibfac ) then datasum *= calibfac

           datastruc1 = { data:datasum[*,*,*,*,0], $
                          refnoise:refnoise[*,*,*,0], skynoise:skynoise[*,*,*,0], $
                          pmBot:pmB, pmTop:pmT, header:sheader, $
                          filename:MEF_file, MJD:sxpar(mainHeader,'MJD-OBS'), $
                          date:sxpar(mainHeader,'date-obs'), UT:sxpar(mainHeader,'UT1'),$
                          cammode:cammode, obsmode:obsmode, aperture:aperture, $
                          filter:filter, slit:slit, grating:grating, $
                          readmode:readmode, welldepth:welldepth, object:object, $
                          airmass:airmass, pwv:pwv, humidity:humid,$
                          frameTime:frameTime, saveTime:saveSrcTime, exptime:exptime, $
                          savesets:savesets, saveCoadds:saveCoadds, chopFreq:chopFreq, $
                          nodset:0, nodsets:nodsets, ncycles:ncycles, $
                          nodgroup:0, nodgroups:nodgroups, $
                          xhds:extenHeaders[*,*,*,0] }

           if( sds[0] eq 5 )then begin

              nnods = sds[5]
              Lk = nnods-2
              datastructs = replicate( datastruc1, nnods )

              for k=0,nnods-1 do begin
                 datastructs[k].data = datasum[*,*,*,*,k]
                 datastructs[k].refnoise = refnoise[*,*,*,k<Lk]
                 datastructs[k].skynoise = skynoise[*,*,*,k<Lk]
                 datastructs[k].xhds = extenHeaders[*,*,*,k]
                 if keyword_set( nodgroups ) then begin
                    datastructs[k].nodgroup = k+1
                 endif else datastructs[k].nodset = k+1
              endfor

              return, datastructs

           endif else return, datastruc1

        endif else begin

           if keyword_set( calibfac ) then dataf *= calibfac

           if keyword_set( struct ) then begin

              datastruct = { data:dataf, refnoise:refnoise, skynoise:skynoise, $
                             pmBot:pmB, pmTop:pmT, header:sheader, $
                             filename:MEF_file, MJD:sxpar(mainHeader,'MJD-OBS'), $
                             date:sxpar(mainHeader,'date-obs'), UT:sxpar(mainHeader,'UT1'),$
                             cammode:cammode, obsmode:obsmode, aperture:aperture, $
                             filter:filter, slit:slit, grating:grating, $
                             readmode:readmode, welldepth:welldepth, object:object, $
                             airmass:airmass, pwv:pwv, humidity:humid,$
                             frameTime:frameTime, saveTime:saveSrcTime, exptime:exptime, $
                             savesets:savesets, saveCoadds:saveCoadds, chopFreq:chopFreq, $
                             nodset:0, nodsets:nodsets, ncycles:ncycles, $
                             nodgroup:0, nodgroups:nodgroups, $
                             xhds:extenHeaders[*,*,*,0] }

              return, datastruct

           endif else return, dataf
        endelse
end
