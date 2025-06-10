;+
; NAME:
;	print_stats
;
; PURPOSE:
;	Print array of statistics of an images (from function im_stats).
;
; CALLING:
;	print_stats, stats, /TITLES, /ALPHABETIC, IMAGE_LIST=
;
; INPUTS:
;	stats = array image statistic structures (function im_stats).
;
; KEYWORDS:
;	IMAGE_LIST = optional arrays of image structures containing stats.
;
;	/TITLES causes title line to be printed first.
;	/ALPHABETIC causes list to be alphabetized by the names.
; OUTPUT:
;	none.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	F.V.2011, added option (question) to scale noise by exposure time.
;	F.V.2013, use Ncoadds in structure, to back-compute noise/frame.
;-

pro print_stats, stats, TITLES=titles, ALPHABETIC=alphab, IMAGE_LIST=image_List

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
              stats = image_List[inums].stats
              if keyword_set( alphab ) then stats = stats[ fsort( stats.name ) ]
              print_stats, stats, TIT=titles
              return
           endif else begin
              message,/INFO,"Structure does not contain STATS structure"
              return
           endelse
        endif

	Nstat = N_struct( stats )
	if (Nstat LE 0) then return

	tags = tag_names( stats )
	tag_fwhm_xy = max( tags EQ "FWHM_XY")
	tag_expt = max( tags EQ "EXPTIME")
	tag_rms = max( tags EQ "RMS")
        tag_arcs = max(tags EQ "ARCS_PIXEL")

        names = stats.name
        maxch = max( strlen( names ), MIN=minch )
        nchar = maxch > 2
        addnch = maxch - minch
        if (addnch gt 0) then names = names + string( replicate( 32B, addnch ) )
        names = strmid( names, 0, nchar )
        printfwhm = 0
        if (tag_fwhm_xy) AND max( stats.fwhm ) gt 0 then printfwhm = 1
	sigman = stats.sigma_noise
        if (tag_expt) then expTime = stats.expTime else expTime = fltarr( Nstat )
        signfrm = sigman * expTime / sqrt( stats.Ncoadds )
        if max( expTime gt 0 ) then printexpt=1 else printexpt=0

	if keyword_set( titles ) then begin
           if( printfwhm ) then begin
              t1 = "st.dev.    SNR     SNR  estimated          FWHM    "
              t2 = " noise     rms     max  sky Level        X        Y"
           endif else begin
              t1 = "st.dev.    SNR     SNR  estimated"
              t2 = " noise     rms     max  sky Level"
           endelse
           if( printexpt ) then begin
              t1 +=("  Exp.Time  Noise/   Filter    Object   Readmode  FrameTime")
              t2 +=("  Seconds   Frame")
           endif
           print," "
           print,"name" + string( replicate(32b,nchar-1) ) + t1
           print, string( replicate(32b,nchar+3) ) + t2
        endif

        if( min( expTime ) gt 0 ) then begin
           if( min( expTime ) LT max( expTime ) ) then begin
              scaleByTime = yes_no_menu("Scale by Max. Exp. Time",/BIN)
           endif
        endif

        if keyword_set( scaleByTime ) then begin
           maxExpTime = max( expTime )
           expTratio = expTime / maxExpTime
           sigman = sigman * sqrt( expTratio )
           expTime[*] = maxExpTime
        endif

        if (tag_fwhm_xy) then fwhm_xy = stats.fwhm_xy else begin
           fwhm_xy = transpose([[stats.fwhm],[stats.fwhm]])
        endelse

        if( tag_arcs ) then begin
           if min(stats.arcs_pixel) LE 0 then tag_arcs=0 else fwhm_xy *= stats.arcs_pixel
        endif
        
        form1 = strtrim( nchar, 2 )
	SNRmax = replicate( 1.0, Nstat )
	SNRrms = SNRmax
	w = where( [sigman] GT 0, nsw )

	if (nsw GT 0) then begin
           SNRmax[w] = (stats[w].max - stats[w].sky_Level) / sigman[w]
           if (tag_rms) then SNRrms[w] = stats[w].rms / sigman[w]
        endif

        for i=0,Nstat-1 do begin

           if( printexpt ) then begin

              if( printfwhm ) then begin

                 if( tag_arcs ) then begin
                    sfwhm = strconcat( string( fwhm_xy[*,i], FORM="(F8.2)")+'"')
                 endif else sfwhm = strconcat( string( fwhm_xy[*,i], FORM="(2F9.1)") )

                 print, names[i], sigman[i], SNRrms[i], SNRmax[i], $
                        stats[i].sky_Level, sfwhm, expTime[i], signfrm[i], $
                        stats[i].filter, stats[i].object, $
                        stats[i].readmode, stats[i].frameTime, $
                        FORM="(A"+form1+",G9.3,2F8.1,G11.3,A18,2F9.2,X,2(X,A10),X,A8,F6.1)"

              endif else begin

                 print, names[i], sigman[i], SNRrms[i], SNRmax[i], $
                        stats[i].sky_Level, expTime[i], signfrm[i], $
                        stats[i].filter, stats[i].object, $
                        stats[i].readmode, stats[i].frameTime, $
                        FORM="(A"+form1+",G9.3,2F8.1,G11.3,2G9.2,X,2(X,A10),X,A8,F6.1)"
              endelse

           endif else begin

              if (printfwhm) then begin

                 if( tag_arcs ) then begin
                    sfwhm = strconcat( string( fwhm_xy[*,i], FORM="(F8.2)")+'"')
                 endif else sfwhm = strconcat( string( fwhm_xy[*,i], FORM="(2F9.1)") )

                 print, names[i], sigman[i], SNRrms[i], SNRmax[i], $
                        stats[i].sky_Level, sfwhm, FORM="(A"+form1+",G9.3,2F8.1,G11.3,A18)"

              endif else begin

                 print, names[i], sigman[i], SNRrms[i], SNRmax[i], $
                        stats[i].sky_Level, FORM="(A"+form1+",G9.3,2F8.1,G11.3)"
              endelse
           endelse
        endfor
return
end
