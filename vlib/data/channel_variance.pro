pro channel_variance, data, varis, NCHANNEL=nchan, NVREGION=nvreg, COADDS=coadds, $
					SUBTRACTIONS=subs, HEADER=fhd, $
					PLOT=plotit, USE_VARMAP=usevarmap

	if N_elements( fhd ) gt 1 then coadds = sxpar(fhd,"FRMCOADD")
	if N_elements( coadds ) ne 1 then coadds=1
	if N_elements( subs ) ne 1 then subs=2
	if N_elements( nchan ) ne 1 then nchan=16
	if N_elements( nvreg ) ne 1 then nvreg=1

	sz = size(data)
	npixch = sz[1]/nchan
	npixvr = sz[2]/nvreg
	ndat = sz[3]
	if sz[0] eq 2 then ndat = 1
	if nvreg gt 1 then varis=fltarr( nvreg, nchan, ndat ) else varis=fltarr( nchan, ndat )

	for j=0,ndat-1 do begin

	  for i=0,nchan-1 do begin

	    chandat = float( data[ i*npixch:(i+1)*npixch-1, *, j ] )

	    if nvreg gt 1 then begin

		for k=0,nvreg-1 do begin
			regdat = chandat[ *, k*npixvr:(k+1)*npixvr-1 ]
			varis[k,i,j] = variance( regdat )
		 endfor

	     endif else varis[i,j] = variance( chandat )

	   endfor
	 endfor

	varis = varis/(coadds*subs)
end
