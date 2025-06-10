;+
; NAME
;       fitsheadstructs
;
; PURPOSE:
;       Read FITS header(s) and convert into an IDL structure and return it.
;
; CALLING SEQUENCE:
;
;	fheadstruct = fitsheadstructs( FILE=filename )
;
;	fheadstruct = fitsheadstructs( HEADER=fits_header_strings )
;
;	fheadstructarray = fitsheadstructs( DIR=directory )
;
; INPUTS:  keywords only.
;
; KEYWORD PARAMETERS:
;
;	HEADER = if string array then convert this FITS header info to a structure.
;
;	EXTENSION = if string array then convert this extension header info to a structure.
;
;	INSTRUMENT = string name of instrument.
;
;	DIRECTORY = directory, process all files in directory.
;
;	FTYPES = default is *_accsig.fits
;
;	FILE = string filename, then read header from file.
;
; OUTPUTS:
;	Function returns structure variable (or array), containing fits header info.
;
; MODIFICATION HISTORY:
;
;  Written  June 2011: Frank Varosi, Dept. of Astronomy,  Univ. of Florida.
;  Mod. FV  Oct. 2011: added keywords EXTENSION and INSTRUMENT.
;
;# $Name:  $ $Id: fitsheadstructs.pro,v 1.3 2012/02/14 21:40:19 varosi Exp $
;-

function FitsHeadStructs, HEADER=fits_header, EXTENSION=exthd, INSTRUMENT=instrument, $
                          FILE=filename, DIRECTORY=directory, FTYPES=ftypes

	if keyword_set( directory ) then begin

           if N_elements( ftypes ) ne 1 then ftypes = "_accsig.fits"
           files = findfile( directory + "/*" + ftypes, COUNT=nfil )

           if( nfil GT 0 ) then begin
              fhst = fitsheadstructs( files[0] )
              fhsts = replicate( fhst, nfil )
              for i=1,nfil-1 do fhsts[i] = fitsheadstructs( files[i] )
              return, fhsts
           endif else return,nfil

        endif else if keyword_set( exthd ) then begin

           exten = 1
           fhd = exthd
           if N_elements( filename ) ne 1 then filename = ""

        endif else if keyword_set( fits_header ) then begin

           szfh = size( fits_header )

           if( szfh[0] gt 1 ) then begin
              nfh = szfh[2]
              fhs = FitsHeadStructs( HEADER=fits_header[*,0] )
              fhsv = replicate( fhs, nfh )
              for k=1,nfh-1 do fhsv[k] = FitsHeadStructs( HEADER=fits_header[*,k] )
              return, fhsv
           endif

           fhd = fits_header
           if N_elements( filename ) ne 1 then filename = ""

        endif else begin

           fhd = headfits( filename )
           fits_header = fhd
        endelse

        if N_elements( instrument ) ne 1 then instrument = sxpar( fhd, "INSTRUME" )

        if( strpos( instrument, "Canari" ) ge 0 ) then begin

           return, fitshead_cc( fhd, filename, EXTENSION=exten ) 

        endif else return, fitshead_trecs( fhd, filename, EXTENSION=exten )
end
