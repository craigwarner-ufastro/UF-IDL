;+
; NAME
;	MEF_read
; PURPOSE:
;	Read contents of MEF file and return all frames (savesets) in multi-dimensional array.
; CALLING:
;	data = MEF_read( MEF_file, mainHeader, extensionHeaders )
;
; INPUTS:
;	MEF_file = string, file name.
;
; KEYWORDS:
;	/CHOPDIFF : compute the differences of Src - Ref for each saveset and return.
;	/PERSECOND : convert to ADU counts per second instead of counts per frametime
;	/GAIN : if /CHOPDIFF then divide Src - Ref by averaged &
;                                              normalized Refs (approx. gain matrix).
;	        also, if 0 < GAIN < 1 then use value as fraction of gain matrix to apply.
;	        (if S1R3 default fraction of gain matrix is 1/3 for CC and 0.7 for Trecs).
;
;	NODSETS  = override actual number of nod sets in file  (if file param has error)
;	NODBEAMS = override actual number of nod beams in file (if file param has error)
;
;	/STRUCT = return data as elements in structure with obs info.
;
; OUTPUTS:
;	Returns all frames in multi-dimensional array of Long-words or floating point.
;
;	mainHeader = main FITS header of file, but altered to correspond with returned array.
;
;	extensionHeaders = array of Extension headers from file.
;
; MODIFICATION HISTORY:
;
;  Written  June 2003, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Oct  2006, by:    Frank Varosi, UF
;  Mod July 2011, FV@UF: added /GAIN gain matrix correction, and /PERSECOND scaling option
;  Mod Oct 2011, FV@UF: if /GAIN and readmode = S1R3 then use ( 2 + gain )/3 adjustment.
;  Mod May 2012, FV@UF: if MCE SIM = 8 then frameCoadds=1 and do NOT scale to per seconds.
;  Mod Sep 2019, FV@UF: to read new MEF files generated with ARC controller
;  Mod Feb 2020, FV@UF, simplified to keyword options SUM_BEAMS and SUM_NODS
;  Mod May 2020, FV@UF, option /STRUCT to return data in structure.
;  Mod May 2021, FV@UF, fixed bug: had forgotten to divide by saveCoadds.
;  Mod Jan 2022, FV@UF, set appropriate values for Trecs data files.
;
;# $Name:  $ $Id: mef_read.pro,v 1.11 2014/01/30 00:11:26 varosi Exp $
;-

function MEF_read, MEF_file, mainHeader, extensionHeaders, PERSECOND=persec, STRUCT=struct, $
                   COADDSAVES=coaddss, SUM_BEAMS=sumbeams, SUM_NODS=sumnods, FCB=fcb, $
                   CHOPDIFF=chopDiff, GAIN_FF=gainff, $
                   NODBEAMS=nodbeams, NODSETS=nodsets, VERBOSE=verbose

	fits_open, MEF_file, fcb
        message,/INFO, fcb.filename
        print, fcb.nextend," extensions in MEF file"
        mainHeader = fcb.hmain
	fits_read, MEF_file, d, mainHeader, EXTEN=0
	cammode = strtrim( strupcase( sxpar( mainHeader, "CAMMODE")), 2)

        if( strpos( cammode, "POLA") ge 0 ) then begin
           fits_close, fcb
           return, MEF_readPol( MEF_file, mainHeader, extensionHeaders, $
                                SUM_BEAMS=sumbeams, SUM_NODS=sumnods, FCB=fcb, $
                                PERSEC=persec, NODBEAMS=nodbeams, NODSETS=nodsets )
        endif

	instrument = sxpar( mainHeader, "INSTRUME")
	obsmode = sxpar( mainHeader, "OBSMODE")
	readmode = sxpar( mainHeader, "READMODE")
	welldepth = sxpar( mainHeader, "WELLDpth")
	savesets = fix( sxpar( mainHeader, "SAVESETS"))
	Nsavsets = fix( sxpar( mainHeader, "NSAVSETS"))
        object = sxpar(mainHeader,"OBJECT")
        exptime = float( sxpar(mainHeader,"EXPTIME"))
        if( exptime LE 0 ) then exptime = float( sxpar(mainHeader,"OBJTIME"))

        fw1 = sxpar( mainHeader,"filter1")
        fw2 = sxpar( mainHeader,"filter2")
        if( strpos( fw1,"Open" ) ge 0 ) then filter = fw2 else filter = fw1

        airmass = float( sxpar(mainHeader,'AIRMASS'))
        humid = float(sxpar( mainHeader,"HUMOUT1") + sxpar( mainHeader,"HUMOUT2"))/2
        if( humid LE 0 ) then humid = float(sxpar( mainHeader,"HUMIDITY"))
        pwvm = float( sxpar( mainHeader,"PWVMON"))  ;; old keyword before 2014

        if( pwvm gt 0 ) then pwv = pwvm else begin
           pwv1 = float( sxpar( mainHeader,"PWVMON1"))
           pwv2 = float( sxpar( mainHeader,"PWVMON2"))
           if( pwv1 gt 0 and pwv2 gt 0 ) then pwv = (pwv1 + pwv2)/2 else begin
              if( pwv1 gt 0 ) then pwv = pwv1 else pwv = pwv2 
           endelse
        endelse

;; if Ncycles is in FITS header then this is new type of file,
;;  using ARC controller that saves all chops (no chop coadding)
;;  and then chopCoadds parameter is simply the # of readouts per chop beam.

        frameCoadds = fix( sxpar(mainHeader,"FRMCOADD"))
        saveCoadds = frameCoadds
        chopCoadds = fix( sxpar(mainHeader,"CHPCOADD"))
        chopFreq = float( sxpar(mainHeader,"CHPFREQ"))
        chopBeams = fix( sxpar(mainHeader,"CHPBEAMS"))
	Ncycles = fix( sxpar( mainHeader, "NCYCLES"))
        MCEsim = sxpar(mainHeader,"MCE_SIM")

        if( MCEsim eq 8 ) then begin  ;;do not scale to seconds if not coadding
           frameCoadds = 1
           persec = 0
           help,frameCoadds,persec
        endif

        if( chopBeams LE 0 ) then begin
           chopBeams = 1
           if( chopFreq gt 0 and chopCoadds gt 0 ) then chopBeams = 2
        endif

        if( Ncycles LE 0 ) then begin ;; this is original version MEF file with MCE-4:
           if( chopCoadds gt 0 ) then saveCoadds = frameCoadds * chopCoadds
        endif else print,"ARC controller readouts: ",Ncycles," Chop Cycles per Nod Beam."

	if NOT keyword_set( nodsets ) then nodsets = fix(sxpar( mainHeader, "NODSETS" )) > 1
	if NOT keyword_set( nodbeams ) then nodbeams = fix(sxpar( mainHeader, "NNODS" )) > 1
        Nnodsets = fix( sxpar( mainHeader, "NNODSETS" ))
        slit = sxpar(mainHeader,'SLIT')
        grating = sxpar( mainHeader, "grating")
        aperture = sxpar( mainHeader, "aperture")

        if( strlen( cammode ) LT 5 ) then begin
           cammode = "IMAGING"
           if( strpos( grating,"Mirror") LT 0 ) then cammode = "SPECTROSCOPY"
        endif

        print,cammode,"  :  ",obsmode,"  :  ",readmode,"  :  ",welldepth,"  :  ",object
	print,"On Src Time =", exptime," sec.",FORM="(A,F9.1,A)"
        print,"PWV=",pwv," mm  :  Humidity=", humid,$
              " % : Airmass=",airmass,FORM="(A,F6.1,A,F5.1,A,F5.2)"
        print,"SaveCoadds= ",strtrim( saveCoadds,2)," : SaveSets= ",strtrim( savesets,2),$
              " : NodBeams= ",strtrim( nodbeams,2)," : NodSets= ",strtrim( nodsets,2)

        if strpos( readmode,"S1R3") ge 0 then begin
           s1r3 = 1
           if keyword_set( gainff ) then begin
              if( gainff LT 1 ) then gfrac = gainff else begin
                 gfrac = 0.7
                 if strpos( strupcase(instrument),"CANARICAM") ge 0 then gfrac = 1.0/3.0
              endelse
              help,gfrac
           endif
        endif

        if( Nsavsets LT savesets ) then begin
           message,/INFO,"Obs. was probably ABORTED:"
           print," # actual savesets [",Nsavsets,"] < [",savesets,"] configured."
        endif

        if( Nnodsets LT nodsets ) then begin
           message,/INFO,"Obs. was probably ABORTED:"
           print," # actual Nod Sets = [",Nnodsets,"] < [",nodsets,"] # configured."
           nodsets = Nnodsets
        endif

	frameTime = sxpar( mainHeader, "FRMTIME")
        saveSrcTime = saveCoadds * frameTime/1000

        print,"Save Src Time (sec) =", saveSrcTime,"  :  Frame Time (ms) =", frameTime, $
              "  :  Chop Freq. (Hz) =", chopFreq,FORM="(3(A,F7.2))"
	print,"Filters: ", sxpar( mainHeader,"FILTER1"),"  +  ",sxpar( mainHeader,"FILTER2")+$
              "  :  grating=", grating," : Slit=",slit," : Aperture=",aperture
        
        ;; allocate at least 110 strings for extension headers to keep the same:
        nehr = max( (fcb.start_data[1:*] - fcb.start_header[1:*])/80 ) > 110
        if keyword_set( verbose ) then print,"Longest exten. header = ",nehr," records."
        extensionHeaders = strarr( nehr+1, nodbeams > 2, Nnodsets )
	iex=0

        for inod = 1, nodsets do begin
           for nbeam = 1, nodbeams do begin

              iex = iex+1
              fits_read, fcb, data, exhd, EXTEN=iex,/NO_ABORT

              kb = nbeam-1
              knod = inod-1
              extensionHeaders[0,kb,knod] = exhd

              sd = size( data )
              if( Nsavsets LE 0 ) then Nsavsets = sd[4]

              if( Nsavsets LT sd[4] ) then begin
                 data = data[*,*,*,0:Nsavsets-1]
                 sd = size( data )
              endif

              nodbeam = strtrim( sxpar( exhd, "NOD" ), 2 )
              if( iex LE 1 ) then nodbeam = "A"          ;;override old bug (now fixed)
              nodset = sxpar( exhd, "NODSET" )
              if keyword_set( verbose ) then begin
                 print,"NodBeam=", nodbeam, $
                       " ,   NodSet =", string( nodset, FORM="(I4)"), $
                       " ,   iNod =", string( inod, FORM="(I4)"), $
                       " ,   Ext # =", string( iex, FORM="(I4)")
              endif
              ;; create the array that will be returned by function:
              if( iex EQ 1 ) then begin
                 if( chopBeams gt 1 ) then begin
                    if keyword_set( chopDiff ) then begin
                       if keyword_set( gainff ) then begin
                          datafull = fltarr( sd[1], sd[2], Nsavsets, nodbeams, nodsets )
                       endif else begin
                          datafull = Lonarr( sd[1], sd[2], Nsavsets, nodbeams, nodsets )
                       endelse
                    endif else begin
                       datafull = Lonarr( sd[1], sd[2], sd[3], Nsavsets, nodbeams,nodsets)
                    endelse
                 endif else datafull = Lonarr( sd[1], sd[2], Nsavsets, nodbeams, nodsets )
              endif

              if( chopBeams gt 1 ) then begin
                 if( strpos( nodbeam,"A") ge 0 ) then begin
                    src = reform( data[*,*,0,*] )
                    ref = reform( data[*,*,1,*] )
                 endif else begin
                    src = reform( data[*,*,1,*] )
                    ref = reform( data[*,*,0,*] )
                 endelse
                 if keyword_set( chopDiff ) then begin
                    if keyword_set( gainff ) then begin
                       if( gainff GT 1 ) then begin
                          mref = median( ref, DIM=3 )
                       endif else mref = total( ref, 3 )
                       gain = ( total(mref)/N_elements(mref) ) / mref
                       if keyword_set( s1r3 ) then gain = gfrac * gain + (1 - gfrac)
                       for jset = 0, Nsavsets-1 do begin
                          datafull[0,0,jset,kb,knod] =(src[*,*,jset] - ref[*,*,jset])*gain
                       endfor
                    endif else datafull[0,0,0,kb,knod] = src - ref
                 endif else begin
                    datafull[*,*,0,*,kb,knod] = src
                    datafull[*,*,1,*,kb,knod] = ref
                 endelse
              endif else datafull[0,0,0,kb,knod] = reform( data )

           endfor
        endfor

        fits_close, fcb
	sdf = size( datafull )
        ndim = sdf[0]

        if( nodsets LE 1 ) then begin
           sumnods = 0
           if( nodbeams LE 1 ) then sumbeams = 0 else ndim += 1
        endif

        ;; Use 360 element array to store all FITS headers so they will be same size:
        sheader = strarr( N_elements( mainHeader ) > 360 )
        sheader[0] = mainHeader

        if keyword_set(sumbeams) or keyword_set(sumnods) or keyword_set(coaddss) then begin

           if keyword_set(sumbeams) and keyword_set(sumnods) and keyword_set(coaddss) then begin

              nsum = sdf[ndim] * sdf[ndim-1] * sdf[ndim-2] * saveCoadds
              datasum = total( total( total( datafull, ndim ), ndim-1), ndim-2) / nsum

           endif else if keyword_set( sumbeams ) and keyword_set( sumnods ) then begin

              nsum = sdf[ndim]* sdf[ndim-1] * saveCoadds
              datasum = total( total( datafull, ndim ), ndim-1) / nsum

           endif else if keyword_set(sumnods) and keyword_set(coaddss) then begin

              nsum = sdf[ndim] * sdf[ndim-2] * saveCoadds
              datasum = reform( total( total( datafull, ndim ), ndim-2 ) / nsum )

           endif else if keyword_set(sumbeams) and keyword_set(coaddss) then begin

              nsum = sdf[ndim-1] * sdf[ndim-2] * saveCoadds
              datasum = reform( total( total( datafull, ndim-1 ), ndim-2 ) / nsum )

           endif else if keyword_set( sumnods ) then begin

              nsum = sdf[ndim] * saveCoadds
              datasum = total( datafull, ndim) / nsum

           endif else if keyword_set(sumbeams) then begin

              nsum = sdf[ndim-1] * saveCoadds
              datasum = reform( total( datafull, ndim-1 ) / nsum )

           endif else begin  ;; just coadd the savesets:

              if keyword_set( chopDiff ) then begin
                 datasum = reform( total( datafull, 3 ) / (sdf[3]*saveCoadds) )
              endif else datasum = reform( total( datafull, 4 ) / (sdf[4]*saveCoadds) )

           endelse

           datafull = 0   ;;de-allocate here because more likely to save memory

           if keyword_set( persec ) then begin
              calibfac = float( 1.e3 / frameTime )
              if keyword_set( verbose ) then help,calibfac
              datasum *= calibfac
           endif

           if keyword_set( struct ) then begin

              return, { data:datasum, header:sheader, filename:MEF_file, $
                        MJD:sxpar(mainHeader,'MJD-OBS'), $
                        date:sxpar(mainHeader,'date-obs'), UT:sxpar(mainHeader,'UT1'), $
                        cammode:cammode, obsmode:obsmode, aperture:aperture, $
                        filter:filter, slit:slit, grating:grating, $
                        readmode:readmode, welldepth:welldepth, object:object, $
                        airmass:airmass, pwv:pwv, humidity:humid, $
                        frameTime:float(frameTime), saveTime:float(saveSrcTime), $
                        exptime:exptime, $
                        savesets:savesets, saveCoadds:saveCoadds, chopFreq:chopFreq, $
                        nodsets:nodsets, ncycles:ncycles, xhds:extensionHeaders[*,*,0] }

           endif else return, datasum

        endif else begin
        
           if keyword_set( persec ) then begin
              calibfac = float( 1.0 / saveSrcTime )
           endif else calibfac = 1.0 / saveCoadds

           if keyword_set( verbose ) then help,calibfac
           datafull *= calibfac

           if keyword_set( struct ) then begin

              return, { data:datafull, header:sheader, filename:MEF_file, $
                        MJD:sxpar(mainHeader,'MJD-OBS'), $
                        date:sxpar(mainHeader,'date-obs'), UT:sxpar(mainHeader,'UT1'), $
                        cammode:cammode, obsmode:obsmode, aperture:aperture, $
                        filter:filter, slit:slit, grating:grating, $
                        readmode:readmode, welldepth:welldepth, object:object, $
                        airmass:airmass, pwv:pwv, humidity:humid, $
                        frameTime:float(frameTime), saveTime:float(saveSrcTime), $
                        exptime:exptime, $
                        savesets:savesets, saveCoadds:saveCoadds, chopFreq:chopFreq, $
                        nodsets:nodsets, ncycles:ncycles, xhds:extensionHeaders }

           endif else return, datafull
        endelse
 end
