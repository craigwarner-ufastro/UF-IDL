;+
; NAME
;       trecsfitshead
;
; PURPOSE:
;       Convert string FITS header into an IDL structure and return it.
;
; CALLING SEQUENCE:
;
;	fheadstruct = trecsfitshead( filename )
;
;	fheadstruct = trecsfitshead( HEADER=fits_header_strings )
;
;	fheadstructarray = trecsfitshead( DIR=directory )
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
;	DIRECTORY = directory, process all files in directory.
;
;	FTYPES = default is *.fits.accsig
;
;	HEADER = if string array then convert this fits header info to a structure.
;
; OUTPUTS:
;	Structure variable or array of such, containing fits header info.
;
; MODIFICATION HISTORY:
;
;  Written July 2003, by:    Frank Varosi, Department of Astronomy, University of Florida.
;-

function trecsfitshead, filename, DIRECTORY=directory, FTYPES=ftypes, HEADER=fits_header

	if keyword_set( directory ) then begin

		if N_elements( ftypes ) ne 1 then ftypes = "fits.accsig"
		files = findfile( directory + "/*." + ftypes, COUNT=nfil )
		if nfil GT 0 then begin
			fhst = trecsfitshead( files[0] )
			fhsts = replicate( fhst, nfil )
			for i=1,nfil-1 do fhsts[i] = trecsfitshead( files[i] )
			return, fhsts
		  endif else return,nfil

	 endif else if keyword_set( fits_header ) then begin

		fhd = fits_header
		if N_elements( filename ) ne 1 then filename = ""

	  endif else fhd = headfits( filename )

;Check if it is old version of fits header keywords, otherwise process new version:

	if max( strpos( fhd, "COFF" ) ) ge 0 then begin

		fhst = { TRECS_HEAD_v8,					$
			filename:	filename,			$
			date:		sxpar( fhd, "DATE_FH" ),	$
			obsmode:	sxpar( fhd, "obsmode" ),	$
			readmode:	sxpar( fhd, "readmode" ),	$
			filter1:	sxpar( fhd, "FILTER1" ),	$
			filter2:	sxpar( fhd, "FILTER2" ),	$
			filter:		"",				$
			grating:	sxpar( fhd, "GRATING" ),	$
			sector:		sxpar( fhd, "SECTOR" ),		$
			slit:		sxpar( fhd, "SLIT" ),		$
			Lyot:		sxpar( fhd, "LYOT" ),		$
			pupil:		sxpar( fhd, "PUPILIMA" ),	$
			aperture:	sxpar( fhd, "APERTURE" ),	$
			window:		sxpar( fhd, "WINDOW" ),		$
			Tdet:		float(sxpar( fhd, "DETARRAY" )),$
			frameCoadds:	sxpar( fhd, "FRMCOADD"),	$
			chopCoadds:	sxpar( fhd, "CHPCOADD"),	$
			chopSettle:	sxpar( fhd, "CHPSETTL"),	$
			ncoadds:	0L,				$
			savesets:	sxpar( fhd, "SAVESETS" ),	$
			nodsets:	sxpar( fhd, "NODSETS" ),	$
			nodbeams:	sxpar( fhd, "NNODS" ),		$
			totCoadds:	0L,				$
			Welldepth:	fix( sxpar( fhd, "WELLDPTH") ),	$
			BiasLevel:	fix( sxpar( fhd, "BIASLEVL") ),	$
			FrameTime:	sxpar( fhd, "FRMTIME"), 	$
			onsrcTime:	float(sxpar( fhd, "OBJTIME")), 	$
			exposeTime:	sxpar( fhd, "EXPTIME"), 	$
			ChopFreq:	sxpar( fhd, "CHPFREQ" ),	$
			ChopDelay:	sxpar( fhd, "CHPDELAY" ),	$
			SaveFreq:	float(sxpar( fhd, "SAVEFREQ" )),$
			nodTime:	sxpar( fhd, "NODTIME"), 	$
			nodDelay:	sxpar( fhd, "NODDELAY" ),	$
			totalFrames:	sxpar( fhd, "TOTFRMS"),		$
			procFrames:	sxpar( fhd, "FRMSPROC"),	$
			sigFrm:		float(sxpar( fhd, "SIGFRM" )),	$
			sigFmax:	sxpar( fhd, "SIGMAX" ),		$
			bkaStart:	sxpar( fhd, "BKASTART" ),	$
			bkaEnd:		sxpar( fhd, "BKAEND" ),		$
			sigRead:	sxpar( fhd, "SIGREADN" ),	$
			rdaEnd:		sxpar( fhd, "COFFADUS" ),	$
			bkwStart:	sxpar( fhd, "BKWSTART" ),	$
			bkwEnd:		sxpar( fhd, "BKWEND" ),		$
			bkwMax:		sxpar( fhd, "BKWMAX" ),		$
			ChopDC:		sxpar( fhd, "EFF_CHP" ),	$
			NodDc:		sxpar( fhd, "EFF_NOD" )		}

	 endif else begin

		fhst = { TRECS_HEAD_v9,					$
			filename:	filename,			$
			date:		sxpar( fhd, "DATE_FH" ),	$
			obsmode:	sxpar( fhd, "obsmode" ),	$
			readmode:	sxpar( fhd, "readmode" ),	$
			filter1:	sxpar( fhd, "FILTER1" ),	$
			filter2:	sxpar( fhd, "FILTER2" ),	$
			filter:		"",				$
			grating:	sxpar( fhd, "GRATING" ),	$
			sector:		sxpar( fhd, "SECTOR" ),		$
			slit:		sxpar( fhd, "SLIT" ),		$
			Lyot:		sxpar( fhd, "LYOT" ),		$
			pupil:		sxpar( fhd, "PUPILIMA" ),	$
			aperture:	sxpar( fhd, "APERTURE" ),	$
			window:		sxpar( fhd, "WINDOW" ),		$
			Tdet:		float(sxpar( fhd, "DETARRAY" )),$
			frameCoadds:	sxpar( fhd, "FRMCOADD"),	$
			chopCoadds:	sxpar( fhd, "CHPCOADD"),	$
			chopSettle:	sxpar( fhd, "CHPSETTL"),	$
			ncoadds:	0L,				$
			savesets:	sxpar( fhd, "SAVESETS" ),	$
			nodsets:	sxpar( fhd, "NODSETS" ),	$
			nodbeams:	sxpar( fhd, "NNODS" ),		$
			totCoadds:	0L,				$
			Welldepth:	fix( sxpar( fhd, "WELLDPTH") ),	$
			BiasLevel:	fix( sxpar( fhd, "BIASLEVL") ),	$
			FrameTime:	sxpar( fhd, "FRMTIME"), 	$
			onsrcTime:	sxpar( fhd, "OBJTIME"), 	$
			exposeTime:	sxpar( fhd, "EXPTIME"), 	$
			ChopFreq:	sxpar( fhd, "CHPFREQ" ),	$
			ChopDelay:	sxpar( fhd, "CHPDELAY" ),	$
			SaveFreq:	float(sxpar( fhd, "SAVEFREQ" )),$
			nodTime:	sxpar( fhd, "NODTIME"), 	$
			nodDelay:	sxpar( fhd, "NODDELAY" ),	$
			totalFrames:	sxpar( fhd, "TOTFRMS"),		$
			procFrames:	sxpar( fhd, "FRMSPROC"),	$
			sigFrm:		float(sxpar( fhd, "SIGFRM" )),	$
			sigFmin:	sxpar( fhd, "SIGFMIN" ),	$
			sigFmax:	sxpar( fhd, "SIGFMAX" ),	$
			bkaStart:	sxpar( fhd, "BKASTART" ),	$
			bkaEnd:		sxpar( fhd, "BKAEND" ),		$
			bkaMin:		sxpar( fhd, "BKAMIN" ),		$
			bkaMax:		sxpar( fhd, "BKAMAX" ),		$
			sigRead:	float(sxpar( fhd, "SIGREAD" )),	$
			sigRmin:	sxpar( fhd, "SIGRMIN" ),	$
			sigRmax:	sxpar( fhd, "SIGRMAX" ),	$
			rdaStart:	sxpar( fhd, "RDASTART" ),	$
			rdaEnd:		sxpar( fhd, "RDAEND" ),		$
			rdaMin:		sxpar( fhd, "RDAMIN" ),		$
			rdaMax:		sxpar( fhd, "RDAMAX" ),		$
			bkwStart:	sxpar( fhd, "BKWSTART" ),	$
			bkwEnd:		sxpar( fhd, "BKWEND" ),		$
			bkwMin:		sxpar( fhd, "BKWMIN" ),		$
			bkwMax:		sxpar( fhd, "BKWMAX" ),		$
			AirMass1:	float(sxpar( fhd, "AIRMASS1" )),$
			AirMass2:	float(sxpar( fhd, "AIRMASS2" )),$
			DHSLabel:	sxpar( fhd, "DHSLABEL" ),	$
			CompStat:	sxpar( fhd, "COMPSTAT" ),	$
			UsrNote:	sxpar( fhd, "USRNOTE" ),	$
			MJD_Obs:	sxpar( fhd, "MJD-OBS" ),	$
			Ctype1:		sxpar( fhd, "CTYPE1" ),		$
			CrPix1:		sxpar( fhd, "CRPIX1" ),		$
			CrVal1:		sxpar( fhd, "CRVAL1" ),		$
			Ctype2:		sxpar( fhd, "CTYPE2" ),		$
			CrPix2:		sxpar( fhd, "CRPIX2" ),		$
			CrVal2:		sxpar( fhd, "CRVAL2" ),		$
			ChopDC:		sxpar( fhd, "EFF_CHP" ),	$
			NodDc:		sxpar( fhd, "EFF_NOD" )		}
	  endelse

	if fhst.filter1 eq "Open" then fhst.filter = fhst.filter2 $
				  else fhst.filter = fhst.filter1

	fhst.ncoadds = fhst.framecoadds * (fhst.chopcoadds>1)
	fhst.totcoadds = fhst.ncoadds * fhst.savesets * (fhst.nodsets>1) * (fhst.nodbeams>1)

;adjust if aborted obs.

	if( fhst.procFrames LT fhst.totalFrames AND fhst.procFrames GT 0) then begin 
		fhst.totcoadds = fhst.ncoadds * fhst.procFrames
		if( fhst.chopcoadds GT 0 ) then fhst.totcoadds = fhst.totcoadds/2
	   endif

return, fhst
end
