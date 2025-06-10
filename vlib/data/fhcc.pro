;+
; NAME
;       fitshead_cc
;
; PURPOSE:
;       Convert string FITS header into an IDL structure and return it.
;
; CALLING SEQUENCE:
;
;	fheadstruct = fitshead_cc( fhd, filename )
;
; INPUTS:
;
;
; OUTPUTS:
;	Structure variable or array of such, containing fits header info.
;
; MODIFICATION HISTORY:
;
;  Written  July 2003, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified June 2005, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Aug  2006, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Mar  2007, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Sep  2010, by:    Frank Varosi, Department of Astronomy, University of Florida.
;  Modified June 2011, by:    Frank Varosi, changed name to function fitshead_cc
;
;# $Name:  $ $Id: fitshead_cc.pro,v 1.12 2011/06/23 07:49:33 varosi Exp $
;-

function fitshead_cc, fhd, filename, EXTENSION=exten

  if N_elements( filename ) ne 1 then filename = "unknown"

  WplateRec = sxpar( fhd, "WPLATE" )
  sw = size( WplateRec )

  if sw[sw[0]+1] LT 7 then WplateAng = WplateRec else begin
     dpos = strpos( WplateRec,"_deg")
     if( dpos gt 0 ) then begin
        WplateAng = float( strmid( WplateRec, 0, dpos ) )
     endif else WplateAng = 0.0
  endelse

  if keyword_set( exten ) then begin

     fhst = { EXT_HEAD_cc1,				$
              UTstart:	sxpar( fhd, "UTSTART" ),	$
              filter1:	sxpar( fhd, "FILTER1" ),	$
              filter2:	sxpar( fhd, "FILTER2" ),	$
              filter:		"",			$
              grating:	sxpar( fhd, "GRATING" ),	$
              sector:	sxpar( fhd, "SECTOR" ),		$
              slit:	sxpar( fhd, "SLIT" ),		$
              Lyot:	sxpar( fhd, "LYOT" ),		$
              hwp:	sxpar( fhd, "HWP" ),		$
              wplate:	WplateAng,                      $
              prism:	sxpar( fhd, "PRISM" ),		$
              aperture:	sxpar( fhd, "APERTURE" ),	$
              window:	sxpar( fhd, "WINDOW" ),		$
              pupilAng:	sxpar( fhd, "PUPILANG" ),	$
              nodbeam:	strtrim(sxpar( fhd, "NOD"),2),	$
              nodset:	fix(sxpar( fhd, "NODSET" )),	$
              NodDelay:	sxpar( fhd, "NODDELAY" ),	$
              NodDelAvg:sxpar( fhd, "NODDELAV" ),	$
              Tdet:	float(sxpar( fhd, "DETARRAY" )),$
              AirMassNod:float(sxpar( fhd, "AMSTART" )),$
              WELLAVG:	Long( sxpar( fhd,"WELLAVG")),   $
              WELLMAX:	Long( sxpar( fhd,"WELLMAX")),	$
              WELLMIN:	Long( sxpar( fhd,"WELLMIN")),	$
              SIGWELL:	float( sxpar( fhd,"SIGWELL")),	$
              SIGWMAX:	float( sxpar( fhd,"SIGWMAX")),	$
              SIGWMIN:	float( sxpar( fhd,"SIGWMIN")),	$
              CLAMPAVG:	Long( sxpar( fhd,"CLAMPAVG")),	$
              CLAMPMAX:	Long( sxpar( fhd,"CLAMPMAX")),	$
              CLAMPMIN:	Long( sxpar( fhd,"CLAMPMIN")),	$
              SIGCLAMP:	float( sxpar( fhd,"SIGCLAMP")),	$
              SIGCLMAX:	float( sxpar( fhd,"SIGCLMAX")),	$
              SIGCLMIN:	float( sxpar( fhd,"SIGCLMIN")),	$
              OCLMPAVG:	Long( sxpar( fhd,"OCLMPAVG")),	$
              OCLMPMAX:	Long( sxpar( fhd,"OCLMPMAX")),	$
              OCLMPMIN:	Long( sxpar( fhd,"OCLMPMIN")),	$
              SIGOCLMP:	float( sxpar( fhd,"SIGOCLMP")),	$
              SIGOCMAX:	float( sxpar( fhd,"SIGOCMAX")),	$
              SIGOCMIN:	float( sxpar( fhd,"SIGOCMIN")),	$
              WLMAXAVG:	Long( sxpar( fhd,"WLMAXAVG")),	$
              WLMINAVG:	Long( sxpar( fhd,"WLMINAVG")),	$
              WLMAXMAX:	Long( sxpar( fhd,"WLMAXMAX")),	$
              WLMINMIN:	Long( sxpar( fhd,"WLMINMIN")),	$
              SWMAXAVG:	float( sxpar( fhd,"SWMAXAVG")),	$
              SWMINAVG:	float( sxpar( fhd,"SWMINAVG")),	$
              SWMAXMAX:	float( sxpar( fhd,"SWMAXMAX")),	$
              SWMINMIN:	float( sxpar( fhd,"SWMINMIN")),	$
              CCMAXAVG:	Long( sxpar( fhd,"CCMAXAVG")),	$
              CCMINAVG:	Long( sxpar( fhd,"CCMINAVG")),	$
              CCMAXMAX:	Long( sxpar( fhd,"CCMAXMAX")),	$
              CCMINMIN:	Long( sxpar( fhd,"CCMINMIN")),	$
              SCMAXAVG:	float( sxpar( fhd,"SCMAXAVG")),	$
              SCMINAVG:	float( sxpar( fhd,"SCMINAVG")),	$
              SCMAXMAX:	float( sxpar( fhd,"SCMAXMAX")),	$
              SCMINMIN:	float( sxpar( fhd,"SCMINMIN")),	$
              OCMAXAVG:	Long( sxpar( fhd,"OCMAXAVG")),	$
              OCMINAVG:	Long( sxpar( fhd,"OCMINAVG")),	$
              OCMAXMAX:	Long( sxpar( fhd,"OCMAXMAX")),	$
              OCMINMIN:	Long( sxpar( fhd,"OCMINMIN")),	$
              SOMAXAVG:	float( sxpar( fhd,"SOMAXAVG")),	$
              SOMINAVG:	float( sxpar( fhd,"SOMINAVG")),	$
              SOMAXMAX:	float( sxpar( fhd,"SOMAXMAX")),	$
              SOMINMIN:	float( sxpar( fhd,"SOMINMIN"))  }

     if( fhst.filter1 eq "Open" ) then fhst.filter = fhst.filter2 $
     else fhst.filter = fhst.filter1

  endif else begin

	fhst = { FITS_HEAD_cc9,					$
                 fileread:	filename,			$
                 filename:	sxpar( fhd, "FILENAME" ),	$
                 instrument:	sxpar( fhd, "INSTRUME" ),	$
		plateScale:	sxpar( fhd, "PLATESCA" ),	$
		date_FH:	sxpar( fhd, "DATE_FH" ),	$
                 date:		sxpar( fhd, "DATE" ),	$
                 UTstart:	sxpar( fhd, "UTSTART" ),	$
                 UTend:		sxpar( fhd, "UTEND" ),	$
                 MJD:		sxpar( fhd, "MJD-OBS" ),	$
		CAMmode:	sxpar( fhd, "cammode" ),	$
		obsmode:	sxpar( fhd, "obsmode" ),	$
		readmode:	sxpar( fhd, "readmode" ),	$
		ObjTime:	sxpar( fhd, "OBJTIME"), 	$
		ExpTime:	sxpar( fhd, "EXPTIME"), 	$
		ElapsedTime:	sxpar( fhd, "ELAPSED"), 	$
                 Object:	"", 	$
		MCEsim:		fix(sxpar( fhd, "MCE_SIM")),	$
		MCEwell:	fix(sxpar( fhd, "MCE_WELL")),	$
		MCEbias:	fix(sxpar( fhd, "MCE_BIAS")),	$
		filter1:	sxpar( fhd, "FILTER1" ),	$
		filter2:	sxpar( fhd, "FILTER2" ),	$
		filter:		"",				$
		grating:	sxpar( fhd, "GRATING" ),	$
		centWave:	sxpar( fhd, "CENTWAVE" ),	$
		sector:		sxpar( fhd, "SECTOR" ),		$
		slit:		sxpar( fhd, "SLIT" ),		$
		Lyot:		sxpar( fhd, "LYOT" ),		$
		hwp:		sxpar( fhd, "HWP" ),		$
		wplate:		WplateAng,                      $
		prism:		sxpar( fhd, "PRISM" ),		$
		aperture:	sxpar( fhd, "APERTURE" ),	$
		window:		sxpar( fhd, "WINDOW" ),		$
		Tdet:		float(sxpar( fhd, "DETARRAY" )),$
		DTempCon:	fix(sxpar( fhd, "DTEMPCON" )),  $
		frameCoadds:	sxpar( fhd, "FRMCOADD"),	$
		chopCoadds:	sxpar( fhd, "CHPCOADD"),	$
		chopSettle:	sxpar( fhd, "CHPSETTL"),	$
		ncoadds:	0L,				$
		savesets:	sxpar( fhd, "SAVESETS" ),	$
		nodsets:	sxpar( fhd, "NODSETS" ),	$
		nodbeams:	sxpar( fhd, "NNODS" ),		$
		totCoadds:	0L,				$
		Welldepth:	strtrim( sxpar( fhd,"WELLDPTH"),2),	$
		BiasLevel:	strtrim( sxpar( fhd,"BIASLEVL"),2),	$
                Vgate:		strtrim( sxpar( fhd,"VGATE"),2),	$
		FrameTime:	sxpar( fhd, "FRMTIME"),	$
		ChopFreq:	float( sxpar( fhd, "CHPFREQ")),	$
		ChopDelay:	float( sxpar( fhd, "CHPDELAY")),$
		ChopThrow:	float( sxpar( fhd, "CHPTHROW")),$
                 ChopPA:	float( sxpar( fhd, "CHPPA")),	$
		NodThrow:	float( sxpar( fhd, "NODTHROW")),$
                 NodPA:	        float( sxpar( fhd, "NODPA")),	$
		SaveFreq:	float(sxpar( fhd, "SAVEFREQ")),$
		SaveTime:	float(sxpar( fhd, "SAVETIME")),$
		nodTime:	float( sxpar( fhd, "NODTIME")), $
		nodDelay:	float( sxpar( fhd, "NODDELAY")),$
                 nodDelAvg:     float( sxpar( fhd, "NODDELAV")),$
		ChopEff:	float( sxpar( fhd, "EFF_CHP")),	$
		NodEff:		float( sxpar( fhd, "EFF_NOD")),	$
		totalFrames:	Long(sxpar( fhd, "TOTFRMS")),	$
		procFrames:	Long(sxpar( fhd, "FRMSPROC")),	$
		AirMass:	float(sxpar( fhd, "AIRMASS" )),$
		AirMass1:	float(sxpar( fhd, "AM1" )),$
		AirMass2:	float(sxpar( fhd, "AM2" )),$
		CompStat:	sxpar( fhd, "COMPSTAT" ),	$
		UserNote:	sxpar( fhd, "USERNOTE" ),	$
                 WELLAVG:	Long( sxpar( fhd,"WELLAVG")),   $
                 WELLMAX:	Long( sxpar( fhd,"WELLMAX")),	$
                 WELLMIN:	Long( sxpar( fhd,"WELLMIN")),	$
                 SIGWELL:	float( sxpar( fhd,"SIGWELL")),	$
                 SIGWMAX:	float( sxpar( fhd,"SIGWMAX")),	$
                 SIGWMIN:	float( sxpar( fhd,"SIGWMIN")),	$
                 CLAMPAVG:	Long( sxpar( fhd,"CLAMPAVG")),	$
                 CLAMPMAX:	Long( sxpar( fhd,"CLAMPMAX")),	$
                 CLAMPMIN:	Long( sxpar( fhd,"CLAMPMIN")),	$
                 SIGCLAMP:	float( sxpar( fhd,"SIGCLAMP")),	$
                 SIGCLMAX:	float( sxpar( fhd,"SIGCLMAX")),	$
                 SIGCLMIN:	float( sxpar( fhd,"SIGCLMIN")),	$
                 OCLMPAVG:	Long( sxpar( fhd,"OCLMPAVG")),	$
                 OCLMPMAX:	Long( sxpar( fhd,"OCLMPMAX")),	$
                 OCLMPMIN:	Long( sxpar( fhd,"OCLMPMIN")),	$
                 SIGOCLMP:	float( sxpar( fhd,"SIGOCLMP")),	$
                 SIGOCMAX:	float( sxpar( fhd,"SIGOCMAX")),	$
                 SIGOCMIN:	float( sxpar( fhd,"SIGOCMIN")),	$
                 WLMAXAVG:	Long( sxpar( fhd,"WLMAXAVG")),	$
                 WLMINAVG:	Long( sxpar( fhd,"WLMINAVG")),	$
                 WLMAXMAX:	Long( sxpar( fhd,"WLMAXMAX")),	$
                 WLMINMIN:	Long( sxpar( fhd,"WLMINMIN")),	$
                 SWMAXAVG:	float( sxpar( fhd,"SWMAXAVG")),	$
                 SWMINAVG:	float( sxpar( fhd,"SWMINAVG")),	$
                 SWMAXMAX:	float( sxpar( fhd,"SWMAXMAX")),	$
                 SWMINMIN:	float( sxpar( fhd,"SWMINMIN")),	$
                 CCMAXAVG:	Long( sxpar( fhd,"CCMAXAVG")),	$
                 CCMINAVG:	Long( sxpar( fhd,"CCMINAVG")),	$
                 CCMAXMAX:	Long( sxpar( fhd,"CCMAXMAX")),	$
                 CCMINMIN:	Long( sxpar( fhd,"CCMINMIN")),	$
                 SCMAXAVG:	float( sxpar( fhd,"SCMAXAVG")),	$
                 SCMINAVG:	float( sxpar( fhd,"SCMINAVG")),	$
                 SCMAXMAX:	float( sxpar( fhd,"SCMAXMAX")),	$
                 SCMINMIN:	float( sxpar( fhd,"SCMINMIN")),	$
                 OCMAXAVG:	Long( sxpar( fhd,"OCMAXAVG")),	$
                 OCMINAVG:	Long( sxpar( fhd,"OCMINAVG")),	$
                 OCMAXMAX:	Long( sxpar( fhd,"OCMAXMAX")),	$
                 OCMINMIN:	Long( sxpar( fhd,"OCMINMIN")),	$
                 SOMAXAVG:	float( sxpar( fhd,"SOMAXAVG")),	$
                 SOMINAVG:	float( sxpar( fhd,"SOMINAVG")),	$
                 SOMAXMAX:	float( sxpar( fhd,"SOMAXMAX")),	$
                 SOMINMIN:	float( sxpar( fhd,"SOMINMIN")),	$
                 HUMOUT1:	float( sxpar( fhd,"HUMOUT1")),	$
                 HUMOUT2:	float( sxpar( fhd,"HUMOUT2")),	$
                 T_TUBE1:	float( sxpar( fhd,"T_TUBE1")),	$
                 T_TUBE2:	float( sxpar( fhd,"T_TUBE2")),	$
                 RA:	sxpar( fhd, "RA" ),		$
                 DEC:	sxpar( fhd, "DEC" ),		$
                 CD1_1:	sxpar( fhd, "CD1_1" ),		$
                 CD1_2:	sxpar( fhd, "CD1_2" ),		$
                 CD2_1:	sxpar( fhd, "CD2_1" ),		$
                 CD2_2:	sxpar( fhd, "CD2_2" ),		$
                 Ctype1:"", $
                 CrPix1:float( sxpar( fhd,"CRPIX1")),	$
                 CrVal1:float( sxpar( fhd,"CRVAL1")),	$
                 Ctype2:"", $
                 CrPix2:float( sxpar( fhd,"CRPIX2")),	$
                 CrVal2:float( sxpar( fhd,"CRVAL2"))	}

        fhst.object = sxpar( fhd,"OBJECT")

        fhst.Ctype1 = sxpar( fhd, "CTYPE1" )
        fhst.Ctype2 = sxpar( fhd, "CTYPE2" )

        if ( fhst.crval1 eq 0 ) and ( fhst.crval2 eq 0 ) then begin
           fhst.crval1 = fhst.RA
           fhst.crval2 = fhst.DEC
        endif

        if ( fhst.crpix1 eq 0 ) and ( fhst.crpix2 eq 0 ) then begin
           fhst.crpix1 = 320/2.
           fhst.crpix2 = 240/2.
        endif

	if( fhst.filter1 eq "Open" ) then fhst.filter = fhst.filter2 $
				     else fhst.filter = fhst.filter1

	fhst.ncoadds = fhst.framecoadds * (fhst.chopcoadds>1)
	fhst.totcoadds = fhst.ncoadds * fhst.savesets * (fhst.nodsets>1) * (fhst.nodbeams>1)

;adjust if aborted obs.

	if( fhst.procFrames eq 0 ) then begin
		if strpos( fhst.CompStat, "ABORT" ) ge 0 then begin
			iat = 1 + strpos( fhst.CompStat, "@" )
			iend = strpos( fhst.CompStat, "fr" )
			fhst.procFrames = Long( strmid( fhst.CompStat, iat, iend-iat ) )
		 endif else fhst.procFrames = fhst.totalFrames
	   endif

        if( fhst.procFrames LT fhst.totalFrames AND fhst.procFrames GT 0) then begin 
		fhst.totcoadds = fhst.ncoadds * fhst.procFrames
		if( fhst.chopcoadds GT 0 ) then fhst.totcoadds = fhst.totcoadds/2
             endif

     endelse

return, fhst
end
