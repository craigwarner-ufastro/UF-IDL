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
;	fhd = FITS header string array.
;	filename = filename (string) from which FITS header was obtained.
;
; KEYWORDS:
;	/EXTEN : create structure for extension header info (default is main)
;
; OUTPUTS:
;	Structure variable or array of such, containing fits header info.
;
; MODIFICATION HISTORY:
;
;  Written  July 2003, by: Frank Varosi, Department of Astronomy, University of Florida.
;  Modified June 2005, by: Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Aug  2006, by: Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Mar  2007, by: Frank Varosi, Department of Astronomy, University of Florida.
;  Modified Sep  2010, by: Frank Varosi, Department of Astronomy, University of Florida.
;  Modified June 2011, by: Frank Varosi, changed name to function fitshead_cc
;  Modified Nov  2011, by: F.V., added Wplate computed from string to float if needed.
;  Modified Feb  2012, by: F.V., added MaskAngle to EXT_HEAD, CentWave to both.
;  Modified Aug  2020, by: F.V., added RA and DEC in degrees and offset values.
;  Modified Feb  2022, by: F.V., added pixelScale computed from CD matrix.
;
;# $Name:  $ $Id: fitshead_cc.pro,v 1.13 2012/02/14 21:27:40 varosi Exp $
;-

function fitshead_cc, fhd, filename, EXTENSION=exten

  if N_elements( filename ) ne 1 then filename = "unknown"

  WplateRec = sxpar( fhd, "WPLATE")
  sw = size( WplateRec )

  if sw[sw[0]+1] LT 7 then WplateAng = float(WplateRec) else begin
     dpos = strpos( WplateRec,"_deg")
     if( dpos gt 0 ) then begin
        WplateAng = float( strmid( WplateRec, 0, dpos ) )
     endif else WplateAng = 0.0
  endelse

  MaskRotRec = sxpar( fhd, "MASK-ROT")
  sw = size( MaskRotRec )

  if sw[sw[0]+1] LT 7 then MaskRotAng = float(MaskRotRec) else begin
     dpos = strpos( MaskRotRec,"_deg")
     if( dpos gt 0 ) then begin
        MaskRotAng = float( strmid( MaskRotRec, 0, dpos ) )
     endif else MaskRotAng = 0.0
  endelse

  if keyword_set( exten ) then begin

     fhst = { EXT_HEAD_cc3,				$
              UTstart:	sxpar( fhd, "UTSTART"),	$
              filter1:	sxpar( fhd, "FILTER1"),	$
              filter2:	sxpar( fhd, "FILTER2"),	$
              filter:		"",			$
              grating:	sxpar( fhd, "GRATING"),	$
              CentWave:	float(sxpar( fhd,"CENTWAVE")),	$
              sector:	sxpar( fhd, "SECTOR"),		$
              slit:	sxpar( fhd, "SLIT"),		$
              Lyot:	sxpar( fhd, "LYOT"),		$
              hwp:	sxpar( fhd, "HWP"),		$
              Wplate:	WplateAng,                      $
              MaskAngle:MaskRotAng,                     $
              prism:	sxpar( fhd, "PRISM"),		$
              aperture:	sxpar( fhd, "APERTURE"),	$
              window:	sxpar( fhd, "WINDOW"),		$
              pupilAng:	sxpar( fhd, "PUPILANG"),	$
              nodbeam:	strtrim(sxpar( fhd, "NOD"),2),	$
              nodset:	fix(sxpar( fhd, "NODSET")),	$
              NodDelay:	sxpar( fhd, "NODDELAY"),	$
              NodDelAvg:sxpar( fhd, "NODDELAV"),	$
              Tdet:	float(sxpar( fhd, "DETARRAY")),$
              AirMassNod:float(sxpar( fhd, "AMSTART")),$
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

     if( fhst.filter1 eq "Open") then fhst.filter = fhst.filter2 $
     else fhst.filter = fhst.filter1

  endif else begin

     rotator1 = float(sxpar(fhd,'rotator1'))
     rotator2 = float(sxpar(fhd,'rotator2'))
     rotation = round( rotator1 - rotator2 )

     ;;before 2014 there was just one keyword for PWV:
     pwvm = float( sxpar( fhd,"PWVMON"))
     pwv1 = float( sxpar( fhd,"PWVMON1"))
     pwv2 = float( sxpar( fhd,"PWVMON2"))

     if( pwvm gt 0 ) then pwv = pwvm else begin
        if( pwv1 gt 0 and pwv2 gt 0 ) then pwv = (pwv1 + pwv2)/2 else begin
           if( pwv1 gt 0 ) then pwv = pwv1 else pwv = pwv2 
        endelse
     endelse

	fhst = { FITS_HEAD_cc16,				$
                 fileread:	filename,			$
                 filename:	sxpar( fhd, "FILENAME"),	$
                 instrument:	sxpar( fhd, "INSTRUME"),	$
                 plateScale:	0.0,	$
                 pixelScale:    0.0,	$
                 date_FH:	sxpar( fhd, "DATE_FH"),	$
                 date:		sxpar( fhd, "DATE"),	$
                 UTstart:	sxpar( fhd, "UTSTART"),	$
                 UTend:		sxpar( fhd, "UTEND"),	$
                 ST:		sxpar( fhd, "ST1"),	$
                 MJD:		sxpar( fhd, "MJD-OBS"),	$
		CAMmode:	sxpar( fhd, "cammode"),	$
		obsmode:	sxpar( fhd, "obsmode"),	$
		readmode:	sxpar( fhd, "readmode"),	$
		ObjTime:	sxpar( fhd, "OBJTIME"), 	$
		ExpTime:	sxpar( fhd, "EXPTIME"), 	$
		ElapsedTime:	sxpar( fhd, "ELAPSED"), 	$
		ncoadds:	0L,				$
                 nchops:	sxpar( fhd, "NCHOPS"),	$
		savesets:	sxpar( fhd, "SAVESETS"),	$
		nodsets:	sxpar( fhd, "NODSETS"),	$
		nodbeams:	sxpar( fhd, "NNODS"),		$
		totCoadds:	0L,				$
                 Object:	"", 	$
		MCEsim:		fix(sxpar( fhd, "MCE_SIM")),	$
		MCEwell:	fix(sxpar( fhd, "MCE_WELL")),	$
		MCEbias:	fix(sxpar( fhd, "MCE_BIAS")),	$
		filter1:	sxpar( fhd, "FILTER1"),	$
		filter2:	sxpar( fhd, "FILTER2"),	$
		filter:		"",				$
		grating:	sxpar( fhd, "GRATING"),	$
		CentWave:	float(sxpar( fhd,"CENTWAVE")),	$
		sector:		sxpar( fhd, "SECTOR"),		$
		slit:		sxpar( fhd, "SLIT"),		$
		Lyot:		sxpar( fhd, "LYOT"),		$
		hwp:		sxpar( fhd, "HWP"),		$
		wplate:		WplateAng,                      $
		prism:		sxpar( fhd, "PRISM"),		$
		aperture:	sxpar( fhd, "APERTURE"),	$
		window:		sxpar( fhd, "WINDOW"),		$
		Tdet:		float(sxpar( fhd, "DETARRAY")),$
		DTempCon:	fix(sxpar( fhd, "DTEMPCON")),  $
		frameCoadds:	sxpar( fhd, "FRMCOADD"),	$
		chopCoadds:	sxpar( fhd, "CHPCOADD"),	$
		chopSettle:	sxpar( fhd, "CHPSETTL"),	$
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
                 NodEff:	float( sxpar( fhd, "EFF_NOD")),	$
                 totalFrames:	Long(sxpar( fhd, "TOTFRMS")),	$
                 procFrames:	Long(sxpar( fhd, "FRMSPROC")),	$
                 AirMass:	float(sxpar( fhd, "AIRMASS")),$
                 AirMass1:	float(sxpar( fhd, "AM1")),$
                 AirMass2:	float(sxpar( fhd, "AM2")),$
                 rotator1:      rotator1, $
                 rotator2:      rotator2, $
                 rotation:      rotation, $
                 zd1:           float(sxpar(fhd,'zd1')),$
                 zd2:           float(sxpar(fhd,'zd1')),$
                 instpa:        float( sxpar( fhd,"INSTRPA")),$
                 humidity:      0.0, $
                 PWV1:          pwv1, $
                 PWV2:          pwv2, $
                 PWV:           pwv, $
                 CompStat:	sxpar( fhd, "COMPSTAT"),	$
                 UserNote:	sxpar( fhd, "USERNOTE"),	$
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
                 RA:	sxpar( fhd, "RA"),		$
                 DEC:	sxpar( fhd, "DEC"),		$
                 RA_DEG:	sxpar( fhd, "RADEG"),		$
                 DEC_DEG:       sxpar( fhd, "DECDEG"),		$
                 RA_OFF:	sxpar( fhd, "RA-OFF"),		$
                 DEC_OFF:       sxpar( fhd, "DEC-OFF"),	$
                 CD1_1:	sxpar( fhd, "CD1_1"),		$
                 CD1_2:	sxpar( fhd, "CD1_2"),		$
                 CD2_1:	sxpar( fhd, "CD2_1"),		$
                 CD2_2:	sxpar( fhd, "CD2_2"),		$
                 Ctype1:"", $
                 CrPix1:float( sxpar( fhd,"CRPIX1")),	$
                 CrVal1:float( sxpar( fhd,"CRVAL1")),	$
                 Ctype2:"", $
                 CrPix2:float( sxpar( fhd,"CRPIX2")),	$
                 CrVal2:float( sxpar( fhd,"CRVAL2"))	}

        fhst.object = sxpar( fhd,"OBJECT")  ;;this is done later to force string tags
        fhst.Ctype1 = sxpar( fhd, "CTYPE1")
        fhst.Ctype2 = sxpar( fhd, "CTYPE2")
        fhst.humidity = ( fhst.HUMOUT1 + fhst.HUMOUT2 )/2

        if ( fhst.crval1 eq 0 ) and ( fhst.crval2 eq 0 ) then begin
           fhst.crval1 = fhst.RA
           fhst.crval2 = fhst.DEC
        endif

        if ( fhst.crpix1 eq 0 ) and ( fhst.crpix2 eq 0 ) then begin
           fhst.crpix1 = 320/2.
           fhst.crpix2 = 240/2.
        endif

	if( fhst.filter1 eq "Open") then fhst.filter = fhst.filter2 $
				    else fhst.filter = fhst.filter1

	fhst.ncoadds = fhst.framecoadds * (fhst.chopcoadds>1)
	fhst.totcoadds = fhst.ncoadds * fhst.savesets * (fhst.nodsets>1) * (fhst.nodbeams>1)

        fhst.pixelScale = (sqrt(fhst.cd1_1^2 + fhst.cd1_2^2) + $
                           sqrt(fhst.cd2_2^2 + fhst.cd2_1^2)) * 3600/2

        fhst.plateScale = sxpar( fhd, "PLATESCA")
        if( fhst.plateScale LE 0 ) then fhst.plateScale = fhst.pixelScale

;adjust if aborted obs.

	if( fhst.procFrames eq 0 ) then begin
		if strpos( fhst.CompStat, "ABORT") ge 0 then begin
			iat = 1 + strpos( fhst.CompStat, "@")
			iend = strpos( fhst.CompStat, "fr")
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
