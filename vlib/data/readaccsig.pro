;+
; NAME
;       readaccsig
;
; PURPOSE:
;       Read FITS file of single image data :  accum(SIG/DIF) result from FAS/DAS
;
; CALLING SEQUENCE:
;
;	data_struct = readaccsig( filename, fits_header_strings )
;
; INPUTS:
;       filename
;
; KEYWORD PARAMETERS:
;
;	DIRECTORY = directory, process all files in directory.
;
;	FTYPES = default is *_accsig.fits
;
; RETURN:
;	Structure containing 2D matrix of image data (scaled to ADU per second)
;       and important FITS header information, and estimations of noise and FWHM.
;
; OUTPUTS:
;	Matrix of image data,  scaled to ADU per second.
;
;       FITS header string array,  and FITS header as structure.
;
; MODIFICATION HISTORY:
;
;  Written  June 2011, by:    Frank Varosi, Department of Astronomy, University of Florida.
;
;# $Name:  $ $Id: readaccsig.pro,v 1.10 2011/06/23 07:50:06 varosi Exp $
;-

function readaccsig, filename, fits_header, DIRECTORY=directory, VERBOSE=verbose, FTYPES=ftypes

	if keyword_set( directory ) then begin
		sz = size( directory )
		if sz[sz[0]+1] ne 7 then directory = "/data"
		if N_elements( ftypes ) ne 1 then ftypes = "_accsig.fits"
		filename = pickfile( PATH=directory, FILTER="*"+ftypes,/READ )
		if strlen( filename ) LT 9 then return,0
	   endif

	image = readfits( filename, fits_header )

        fhead_struct = fitsheadstructs( HEADER=fits_header, FILE=filename )
        expseconds = fhead_struct.totcoadds * fhead_struct.frametime / 1000
        image = image / float( expseconds )

        PSFfitLor = fitPSF( image, /LOR )
        PSFFitMof = fitPSF( image, /MOFF )

        negPSFfitLor = replicate( PSFfitLor, 1 )
        negPSFfitMof = replicate( PSFfitMof, 1 )

        for i=1,N_tags(negPSFfitLor)-1 do negPSFfitLor.(i)=0
        for i=1,N_tags(negPSFfitMof)-1 do negPSFfitMof.(i)=0
        if N_elements( nrad ) ne 1 then nrad = 50

        image_struct = { image: image, $
                         PeakSNR:  0.0, $
                         TotSNR:   0.0, $
                         Radius: 0, $
                         AperSNR:   fltarr(nrad), $
                         MaxData: max( image ), $
                         SkyNoise: float( sky_noise( image, sky)), $
                         SkyLevel: float( sky ), $
                         minData: min( image ), $
                         PSFfitL: PSFfitLor, $
                         PSFfitM: PSFfitMof, $
                         negPSFfitL: negPSFfitLor, $
                         negPSFfitM: negPSFfitMof  }
                         
                         
        image_struct.PeakSNR = image_struct.MaxData / image_struct.SkyNoise

        if( image_struct.PeakSNR gt 20 and strpos( fhead_struct.obsmode,"chop") ge 0 ) then begin
           cx = round( PSFfitLor.cx )
           cy = round( PSFfitLor.cy )
           szim = size( image )
           ptotflux = 0.0
           for irad=1,nrad do begin
              totflux = total( image[ disk_region([cx,cy],RADIUS=irad,SIZE=szim[1:2]) ] - sky )
              image_struct.AperSNR[irad-1] = totflux / image_struct.SkyNoise / irad /sqrt(!PI)
              image_struct.Radius = irad
              if( totflux LT ptotflux ) then break
              ptotflux = totflux
           endfor
           image_struct.TotSNR = totflux / image_struct.SkyNoise / irad /sqrt(!PI)
        endif


        if( abs( image_struct.minData ) gt 2*image_struct.MaxData/3 ) then begin
           image_struct.negPSFfitL = fitPSF( -image, /LOR )
           image_struct.negPSFFitM = fitPSF( -image, /MOFF )
        endif

        data_struct = create_struct( image_struct, fhead_struct )
	if keyword_set( verbose ) then help,/st,data_struct

return, data_struct
end
