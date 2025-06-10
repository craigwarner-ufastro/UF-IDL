;+
; NAME
;       fitshead_trecs
;
; PURPOSE:
;       Convert string FITS header into an IDL structure and return it.
;
; CALLING SEQUENCE:
;
;	fheadstruct = fitshead_trecs( fhd, filename )
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
;  Written July 2003, by:  Frank Varosi, Department of Astronomy, University of Florida.
;  Modified June 2011, by: Frank Varosi, changed name to function fitshead_trecs
;  Modified Feb  2012, by: F.V., added CentWave to main header struct.
;  Modified Jan  2022, FV: tag "humidity" instead of humout1/2 since only one value in header.
;  Modified Feb  2022, by: F.V., added pixelScale computed from CD matrix.
;
;# $Name:  $ $Id: fitshead_trecs.pro,v 1.4 2012/02/14 21:30:43 varosi Exp $
;-

function fitshead_trecs, fhd, filename, EXTENSION=exten

  if N_elements( filename ) ne 1 then filename = "unknown"

  if keyword_set( exten ) then begin

    fhst = { EXT_HEAD_Trecs9,				$
              UTstart:	sxpar( fhd, "UTSTART"),	$
              UTend:	sxpar( fhd, "UTEND"),	$
              nodbeam:	strtrim(sxpar( fhd, "NOD"),2),	$
              nodset:	fix(sxpar( fhd, "NODSET")),	$
              AirMassNod:float(sxpar( fhd, "AMSTART")) }

  endif else begin

     fhst = { FITS_HEAD_Trecs12,			$
              fileread:	filename,			$
              filename:	file_basename( filename,".fits"),$
              instrument: sxpar( fhd, "INSTRUME"),	$	
              plateScale: 0.0,	$
              pixelScale: 0.0,	$
              date_FH:	sxpar( fhd, "DATE_FH"),	$
              date:	sxpar( fhd, "DATE-OBS"),	$
              UTstart:	sxpar( fhd, "UTSTART"),	$
              UTend:	sxpar( fhd, "UTEND"),	$
              ST:	sxpar( fhd, "ST"),	$
              MJD:	sxpar( fhd, "MJD-OBS"),	$
              CAMmode:	"imaging",	$
              obsmode:	sxpar( fhd, "obsmode"),	$
              readmode:	strtrim( sxpar( fhd,"readmode"),2),	$
              ObjTime:	sxpar( fhd, "OBJTIME"), 	$
              ExpTime:	sxpar( fhd, "OBJTIME"), 	$
              ElapsedTime: sxpar( fhd, "OBJTIME"), $
              ncoadds:	0L,				$
              nchops:	sxpar( fhd, "NCHOPS"),	$
              savesets:	sxpar( fhd,"SAVESETS"),	$
              nodsets:	sxpar( fhd,"NODSETS"),	$
              nodbeams:	sxpar( fhd,"NNODS"),		$
              totCoadds:0L,				$
              Object:	sxpar( fhd, "object"),	$
              MCEsim:	fix(sxpar( fhd,"MCE_SIM")),	$
              MCEwell:	fix(sxpar( fhd,"WELLDPTH")),	$
              MCEbias:	fix(sxpar( fhd,"BIASLEVL")),	$
              filter1:	strtrim(sxpar( fhd,"FILTER1"),2),	$
              filter2:	strtrim(sxpar( fhd,"FILTER2"),2),	$
              filter:	"",				$
              grating:	strtrim(sxpar( fhd,"GRATING"),2),	$
              centWave:	float(sxpar( fhd,"CENTWAVE")),	$
              sector:	strtrim(sxpar( fhd,"SECTOR"),2),$
              slit:	strtrim(sxpar( fhd,"SLIT"),2),	$
              Lyot:	strtrim(sxpar( fhd,"LYOT"),2),	$
              pupil:	strtrim(sxpar( fhd,"PUPILIMA"),2),	$
              aperture:	strtrim(sxpar( fhd,"APERTURE"),2),	$
              window:	strtrim(sxpar( fhd,"WINDOW"),2),$
              Tdet:	float(sxpar( fhd,"DETARRAY")), $
              Tsetpoint:float(sxpar( fhd,"DETSET")),   $
              DTempCon:	0,  $
              frameCoadds:	sxpar( fhd,"FRMCOADD"),	$
              chopCoadds:	sxpar( fhd,"CHPCOADD"),	$
              chopSettle:	sxpar( fhd,"CHPSETTL"),	$
              Welldepth:strtrim( sxpar( fhd,"WELLDPTH"),2),	$
              BiasLevel:strtrim( sxpar( fhd,"BIASLEVL"),2),	$
              FrameTime:sxpar( fhd,"FRMTIME"), 	$
              ChopFreq:	sxpar( fhd,"CHPFREQ"),	$
              ChopDelay:sxpar( fhd,"CHPDELAY"),	$
              ChopThrow:float( sxpar( fhd,"CHPTHROW")), $
              ChopPA:	float( sxpar( fhd,"CHPPA")),	$
              NodThrow:	float( sxpar( fhd,"NODTHROW")),$
              NodPA:	float( sxpar( fhd,"NODPA")),	$
              SaveFreq:	float(sxpar( fhd,"SAVEFREQ")),$
              SaveTime:	float(sxpar( fhd,"SAVETIME")),$
              nodTime:	sxpar( fhd, "NODTIME"), 	$
              nodDelay:	sxpar( fhd, "NODDELAY"),	$
              ChopEff:	float( sxpar( fhd, "EFF_CHP")),	$
              NodEff:		float( sxpar( fhd, "EFF_NOD")),	$
              totalFrames:	sxpar( fhd, "TOTFRMS"),		$
              procFrames:	sxpar( fhd, "FRMSPROC"),	$
              AirMass:	float(sxpar( fhd, "AIRMASS")),$
              AirMass1:	float(sxpar( fhd, "AMSTART")),$
              AirMass2:	float(sxpar( fhd, "AMEND")),  $
              CompStat:	sxpar( fhd, "COMPSTAT"),	$
              UsrNote:	sxpar( fhd, "USRNOTE"),	$
              MJD_Obs:	sxpar( fhd, "MJD-OBS"),	$
              bkaStart:	Long(sxpar( fhd, "BKASTART")),	$
              bkaEnd:	Long(sxpar( fhd, "BKAEND")),	$
              WLMinAvg:	Long(sxpar( fhd, "BKAMIN")),	$
              WLMaxAvg:	Long(sxpar( fhd, "BKAMAX")),	$
              SigWell:	float(sxpar( fhd, "SIGFRM")),	$
              SWminAvg:	float(sxpar( fhd, "SIGFMIN")),	$
              SWmaxAvg:	float(sxpar( fhd, "SIGFMAX")),	$
              rdaStart:	Long(sxpar( fhd, "RDASTART")),	$
              rdaEnd:	Long(sxpar( fhd, "RDAEND")),	$
              CCMinAvg:	Long(sxpar( fhd, "RDAMIN")),	$
              CCMaxAvg:	Long(sxpar( fhd, "RDAMAX")),	$
              SigClamp:	float(sxpar( fhd, "SIGREAD")),	$
              SCminAvg:	float(sxpar( fhd, "SIGRMIN")),	$
              SCmaxAvg:	float(sxpar( fhd, "SIGRMAX")),	$
              Humidity:	sxpar( fhd, "HUMIDITY"),	$
              T_Tube1:	sxpar( fhd, "TAMBIENT"),	$
              T_Tube2:	sxpar( fhd, "TAMBIENT"),	$
              RA:	sxpar( fhd, "RA"),	$
              DEC:	sxpar( fhd, "DEC"),	$
              CD1_1:	sxpar( fhd, "CD1_1"),		$
              CD1_2:	sxpar( fhd, "CD1_2"),		$
              CD2_1:	sxpar( fhd, "CD2_1"),		$
              CD2_2:	sxpar( fhd, "CD2_2"),		$
              Ctype1:	sxpar( fhd, "CTYPE1"),		$
              CrPix1:	sxpar( fhd, "CRPIX1"),		$
              CrVal1:	sxpar( fhd, "CRVAL1"),		$
              Ctype2:	sxpar( fhd, "CTYPE2"),		$
              CrPix2:	sxpar( fhd, "CRPIX2"),		$
              CrVal2:	sxpar( fhd, "CRVAL2")		}

     if strpos( fhst.filter1,"Open") ge 0 then fhst.filter = fhst.filter2 $
				          else fhst.filter = fhst.filter1

     if strpos( fhst.filter,'24.') ge 0 then fhst.platescale = 0.085
     if strpos( fhst.grating,"Mirr") LT 0 then fhst.camMode = "Spectroscopy"

     fhst.ncoadds = fhst.framecoadds * (fhst.chopcoadds>1)
     fhst.totcoadds = fhst.ncoadds * fhst.savesets * (fhst.nodsets>1) * (fhst.nodbeams>1)

     fhst.pixelScale = (sqrt(fhst.cd1_1^2 + fhst.cd1_2^2) + $
                        sqrt(fhst.cd2_2^2 + fhst.cd2_1^2)) * 3600/2

     fhst.plateScale = sxpar( fhd, "PLATESCA")
     if( fhst.plateScale LE 1e-22 ) then fhst.plateScale = fhst.pixelScale

;adjust if aborted obs.

     if( fhst.procFrames LT fhst.totalFrames AND fhst.procFrames GT 0) then begin 
        fhst.totcoadds = fhst.ncoadds * fhst.procFrames
        if( fhst.chopcoadds GT 0 ) then fhst.totcoadds = fhst.totcoadds/2
     endif

  endelse

return, fhst
end
