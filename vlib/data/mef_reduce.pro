;+
; NAME
;	MEF_reduce
;
; PURPOSE:
;	Read contents of MEF file and reduce the data to final image, with options.
;       Data is scaled to ADU counts per second.
;
; CALLING SEQUENCE:
;
;	image = MEF_reduce( MEF_file, hdmain )
;
; INPUTS:
;	MEF_file = string, file name.
;
; KEYWORD PARAMETERS:
;
;	/GAIN : divide Src - Ref by the average & normalized Ref data (approx. gain matrix).
;	        also, if 0 < GAIN < 1 then use value as fraction of gain matrix to apply.
;	        (default fraction of gain matrix is 1/3 for CC and 0.7 for Trecs).
;	/TWO_NODBEAMS : keep A and B nodbeam (add all A beams but do not add to sum of all B beams)
;	/SEPERATE_NODS : keep each nodset (do not add them)
;	/DISPLAY :  display each nod beam image with tvscl.
;	/TVSHOW :   display each nod beam image with tvs,/ZSCALE (from vlib)
;	NODSETS  = specify actual number of nod sets in file  (if file param has error)
;	NODBEAMS = specify actual number of nod beams in file (if file param has error)
;
; OUTPUTS:
;	Returns image of reduced data frames, normalize to counts per frame time.
;
;	hdmain = main FITS header of file.
;
; MODIFICATION HISTORY:
;
;  Written  June 2003, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Dec  2006, FV @ UF, added /GAIN keyword option.
;  Modified July 2011, FV @ UF, data is scaled to counts (ADU) per second.
;
;# $Name:  $ $Id: mef_reduce.pro,v 1.10 2012/02/14 21:17:58 varosi Exp $
;-

function MEF_reduce, MEF_file, hdmain, GAIN_FF=gainff, SEPERATE_NODS=sepnods, TWO_NODBEAMS=two_beams, $
                     NODBEAMS=nodbeams, NODSETS=nodsets, DISPLAY=display, TVSHOW=tvshow

	fits_read, MEF_file, d, mainHeader, EXTEN=0

	obsmode = sxpar( mainHeader, "OBSMODE" )
	cammode = sxpar( mainHeader, "CAMMODE" )
	readmode = sxpar( mainHeader, "READMODE" )
	savesets = sxpar( mainHeader, "SAVESETS" )
	Nsavsets = sxpar( mainHeader, "NSAVSETS" )
	frameCoadds = sxpar(mainHeader,"FRMCOADD")
        chopCoadds = sxpar(mainHeader,"CHPCOADD")

        saveCoadds = frameCoadds
        if( chopCoadds gt 0 ) then saveCoadds = frameCoadds * chopCoadds

	if NOT keyword_set( nodsets ) then nodsets = sxpar( mainHeader, "NODSETS" ) > 1
	if NOT keyword_set( nodbeams ) then nodbeams = sxpar( mainHeader, "NNODS" ) > 1

        print,MEF_file
        help,obsmode,cammode,readmode,saveCoadds,savesets,nodbeams,nodsets

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

	frameTime = sxpar( mainHeader, "FRMTIME" )
        saveSrcTime = saveCoadds * frameTime/1000

        print,"Save Src Time (s) =", saveSrcTime
	print,"On Src Time (s)   =", sxpar( mainHeader, "OBJTIME" )
	print,"Frame Time (ms)   =", frameTime
	print,"Chop Freq. (Hz)   = ", sxpar( mainHeader, "CHOPFREQ" )
	print,"Filter 1 = ", sxpar( mainHeader, "FILTER1" )
	print,"Filter 2 = ", sxpar( mainHeader, "FILTER2" )
	iex = 0

	for inod = 0, nodsets-1 do begin
           for ibeam = 0, nodbeams-1 do begin

              iex = iex+1
              fits_read, MEF_file, data, exhd, EXTEN=iex
              nodbeam = strtrim( sxpar( exhd, "NOD" ), 2 )
              if( iex LE 1 ) then nodbeam = "A"          ;;override old bug (now fixed)
              nodset = sxpar( exhd, "NODSET" )
              print,"NodBeam=", nodbeam, $
                    " ,   NodSet =", string( nodset, FORM="(I4)"), $
                    " ,   iNod =", string( inod, FORM="(I4)"), $
                    " ,   Ext # =", string( iex, FORM="(I4)")
              szd = size( data )
              nsav = szd[4]
              chopbeams = szd[3]

              if( chopbeams GT 1 ) then begin

                 if( strpos( nodbeam,"A") ge 0 ) then begin
                    src = reform( data[*,*,0,*] )
                    ref = reform( data[*,*,1,*] )
                 endif else begin
                    src = reform( data[*,*,1,*] )
                    ref = reform( data[*,*,0,*] )
                 endelse

                 diff = total( src - ref, 3 )/(nsav*saveSrcTime)

                 if keyword_set( gainff ) then begin
                    if( gainff GT 1 ) then begin
                       mref = median( ref, DIM=3 )
                    endif else mref = total( ref, 3 )
                    gain = ( total(mref)/N_elements(mref) ) / mref
                    if keyword_set( s1r3 ) then gain = gfrac * gain + (1 - gfrac)
                    diff = diff * gain
                 endif

              endif else diff = reform( total( data, 4 ) )/(nsav*saveSrcTime) 

              if keyword_set( two_beams ) then begin

                 if iex EQ 1 then chopdiffs = dblarr( szd[1], szd[2], nodbeams )
                 chopdiffs[*,*,ibeam] = chopdiffs[*,*,ibeam] + diff

              endif else if keyword_set( sepnods ) then begin

                 if iex EQ 1 then chopdiffs = fltarr( szd[1], szd[2], nodbeams, nodsets )
                 chopdiffs[*,*,ibeam,inod] = diff

              endif else begin

                 if iex EQ 1 then sig_image = dblarr( szd[1], szd[2] )
                 sig_image = sig_image + diff
              endelse

              if keyword_set( display ) then begin
                 tvscl, hist_equal(diff)
                 wait,display
              endif else if keyword_set( tvshow ) then begin
                 tvs, diff,/ERASE,/COL,/ZSCALE
                 wait,tvshow
              endif
           endfor
        endfor

	if keyword_set( two_beams ) then begin

		mkhdr, newhd, chopdiffs
		mainHeader = [ mainHeader[0], newhd[1:2+s[0]], mainHeader[3:*] ]
		return, chopdiffs / nodsets

	 endif else if keyword_set( sepnods ) then begin

		mkhdr, newhd, chopdiffs
		mainHeader = [ mainHeader[0], newhd[1:2+s[0]], mainHeader[3:*] ]
		return, chopdiffs

	  endif else begin

		mkhdr, newhd, sig_image
		mainHeader = [ mainHeader[0], newhd[1:2+s[0]], mainHeader[3:*] ]
		return, sig_image / (nodbeams * nodsets)

	   endelse
end
