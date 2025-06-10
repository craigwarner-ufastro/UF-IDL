function himdev, image, IM_RANGE=imran, DEV_RANGE=devran, NPIX=npix, LOG10=Log10

if keyword_set( Log10) then begin
	return, imagxy( aLog10( image>Log10 ), $
			sqrt( Local_Variance( image,/ALL ) >0 ), $
					NPIX=npix, XRAN=imran, YRAN=devran )
  endif else begin
	return, imagxy( image, sqrt( Local_Variance( image,/ALL ) >0 ), $
					NPIX=npix, XRAN=imran, YRAN=devran )
   endelse
end
