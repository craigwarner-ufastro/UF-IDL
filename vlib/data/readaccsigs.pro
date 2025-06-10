;+
; NAME
;       readaccsigs
;
; PURPOSE:
;       Read multiple FITS files of single image data in specified directory.
;
; CALLING SEQUENCE:
;
;	datacube = readaccsigs( fits_headers, fhead_structs )
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
;	DIRECTORY = directory, process all files in directory.
;
;	FTYPES = default is *_accsig.fits
;
; OUTPUTS:
;	3D Matrix (cube) of image data,  scaled to ADU per second.
;
;       FITS headers string array,  and FITS headers as structure array.
;
; MODIFICATION HISTORY:
;
;  Written  June 2011, by:    Frank Varosi, Department of Astronomy, University of Florida.
;
;# $Name:  $ $Id: readaccsigs.pro,v 1.7 2011/08/03 05:22:39 varosi Exp $
;-


function readaccsigs, fits_headers, DIRECTORY=directory, FTYPES=ftypes, DISPLAY=display, TVSHOW=tvshow

  if N_elements( directory ) ne 1 then directory = "/data"
  if N_elements( ftypes ) ne 1 then ftypes = "_accsig.fits"

	fils = findfile( directory+"/*"+ftypes, COUNT=nf )
	help,fils,nf
	dataStruct = readaccsig( fils[0], fhd )
	help,fhd
        nrec = 250
	fits_headers = strarr( nrec, nf )
	data_structs = replicate( dataStruct, nf )

	for i=0,nf-1 do begin

		print," "
		print,fils[i]
		data_structs[i] = readaccsig( fils[i], fhd )
		fits_headers[0,i] = fhd

		if keyword_set( display ) then begin
			erase
			tvscl, data_structs[i].image
			empty
			wait,display
		 endif else if keyword_set( tvshow ) then begin
			tvs, data_structs[i].image,/ERASE,/COL
			empty
			wait,tvshow
		  endif
	  endfor

return, data_structs
end
